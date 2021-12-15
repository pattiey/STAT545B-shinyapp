#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(lubridate)
library(datateachr)
library(ggplot2)
library(dplyr)
library(leaflet)
library(leaflet.extras)

# Define UI for application that draws a histogram
ui <- navbarPage(

    # Application title
    "Exploring the vancouver_trees Dataset",

    tabPanel(
        "Main",
        titlePanel("The Vancouver Street Trees Dataset"),
        mainPanel(
            htmlOutput("header"),
            h1("Dataset Information"),
            p("The street tree dataset includes a listing of public trees on boulevards in the City of Vancouver and provides data on tree coordinates, species and other related characteristics. Park trees and private trees are not included in the inventory."),
            p("For more information visit the ",
              a("City of Vancouver Open Data Portal website.",
                href = "https://opendata.vancouver.ca/explore/dataset/street-trees/information/?disjunctive.species_name&disjunctive.common_name&disjunctive.height_range_id")),
            p("Data for this project was accessed through the ",
              a("UBC-MDS/datateachr",
                href="https://github.com/UBC-MDS/datateachr"),
              " package."),
            h1("App Features"),
            p("This app contains three main components: Table, Tree Map, and Histogram"),
            h3("Table"),
            p("This page contains a table of the trees found in the dataset.",
              " Trees can be filtered by year, street side, and neighbourhood.",
              " The table can be exported as a csv."),
            h3("Tree Map"),
            p("This page contains an interactive map of the trees in Vancouver with a variety of filters adjustable by the user.",
              " Hovering over trees in the map displays the tree_id and clicking on a tree displays information about the species, year planted, and address of the tree"),
            h4("User adjustable filters"),
            p(strong(" - Year Planted: "), "Displays trees planted within a specified range"),
            p(strong(" - Street Side: "), "Displays trees with specified street sides"),
            p(strong(" - Neighbourhood: ", "Displays trees in a specified neighbourhood")),
            h3("Histogram"),
            p("This page contains a histogram displaying the distribution fo the yearly number of trees planted per neighbourhood,",
              " coloured by decade in which a tree was planted.",
              " The user is able to adjust the binwidth of the histogram.",
              " The Density Overlay toggle exists for if the user would like to include a density distribution overlay on the histogram or not."),
            h1("Have Fun :)")
        )
    ),

    tabPanel(
        "Table",
        sidebarPanel(
            "Table Filters",
            numericRangeInput(
                "table_years",
                "Year planted range of trees to display:",
                value = c(2015, 2019),
                separator = "to",
                min = min(vancouver_trees$year_planted, na.rm = T),
                max = max(vancouver_trees$year_planted, na.rm = T),
            ),
            checkboxInput("include_na", "Include trees with no date_planted", value = FALSE),
            # Select street side of trees
            checkboxGroupInput("table_street_side",
                               "Street side of trees to display",
                               unique(vancouver_trees$street_side_name),
                               selected = unique(vancouver_trees$street_side_name)),
            # Select neighbourhood to display
            selectInput("table_neighbourhood",
                        "Select neighbourhood to display:",
                        choices = append("ALL", unique(vancouver_trees$neighbourhood_name))),
            downloadButton("downloadData", "Download")
        ),
        mainPanel(
            dataTableOutput(outputId = 'table')
        )
    ),

    tabPanel(
        "Tree Map",
        titlePanel("Map of trees in Vancouver"),
        sidebarPanel(
            # Slider input for year range
            sliderInput("map_years",
                        "Year planted range of trees to display:",
                        min = 1989,
                        max = 2019,
                        value = c(2015, 2019),
                        sep = ""),
            # Select street side of trees
            checkboxGroupInput("map_street_side",
                               "Street side of trees to display",
                               unique(vancouver_trees$street_side_name),
                               selected = unique(vancouver_trees$street_side_name)),
            # Select neighbourhood to display
            selectInput("map_neighbourhood",
                        "Select neighbourhood to display:",
                        choices = append("ALL", unique(vancouver_trees$neighbourhood_name)))
        ),

        # Show a plot of the generated distribution
        mainPanel(
            leafletOutput(outputId = "mymap")
        )
    ),
    tabPanel(
        "Histogram",
        titlePanel("Histogram of annual trees planted per neighbourhood"),
        sidebarPanel(
            # Slider input for binwidth
            sliderInput("binwidth",
                        "Width of bins to display:",
                        min = 1,
                        max = 50,
                        value = 30),
            # Switch input to determine whether to display density overlay
            switchInput(inputId = "density",
                        label = "Density Overlay",
                        value = TRUE)
        ),
        mainPanel(
            plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    # Insert COV logo
    src = "https://vancouver.ca/images/cov/feature/PRYEAK---3100-Graveley.jpg"
    output$header <- renderText({c('<img src="', src, '">')})

    # add year and decade features to data
    vancouver_trees <- vancouver_trees %>%
        # use year() from lubridate to extract year from date
        mutate(year_planted = year(date_planted),
               height_range_id = as.factor(height_range_id))
    vancouver_trees <- vancouver_trees %>%
        mutate(decade_planted = cut(year_planted, breaks = c(1990, 2000, 2010, 2020),
                                    labels = c("90's", "00's", "10's")))

    # filter function
    filter_table <- function(data, include_na, year_min, year_max, street_side, neighbourhood){
        res <- data %>%
            filter((year_planted >= year_min & year_planted <= year_max) | is.na(year_planted)) %>%
            filter(street_side_name %in% street_side)

        if (!include_na){
            res <- res %>% filter(!is.na(year_planted))
        }
        if (neighbourhood != "ALL") {
            res <- res %>% filter(neighbourhood_name == neighbourhood)
        }
        return(res)
    }

    # vancouver_trees table
    output$table <- renderDataTable({
        vancouver_trees %>%
            filter_table(input$include_na,
                         input$table_years[1],
                         input$table_years[2],
                         input$table_street_side,
                         input$table_neighbourhood) %>%
            select(-c(year_planted, decade_planted))
    })

    # download to CSV
    output$downloadData <- downloadHandler(
        filename = function() {
            year_min <- input$table_years[1]
            year_max <- input$table_years[2]
            neighbourhood <- input$table_neighbourhood
            paste0("vancouver_trees_",
                   year_min,
                   "_to_",
                   year_max,
                   "_",
                   neighbourhood)
        },
        content = function(file) {
            include_na <- input$include_na
            year_min <- input$table_years[1]
            year_max <- input$table_years[2]
            street_side <- input$table_street_side
            neighbourhood <- input$table_neighbourhood
            data <- vancouver_trees %>%
                filter_table(include_na, year_min, year_max, street_side, neighbourhood) %>%
                select(-c(year_planted, decade_planted))
            write.csv(data, file, row.names = FALSE)
        }
    )

    pal <- colorFactor(palette = "Set3",
                        domain = vancouver_trees$height_range_id)
    output$mymap <- renderLeaflet({
        year_min <- input$map_years[1]
        year_max <- input$map_years[2]
        street_side <- input$map_street_side
        neighbourhood <- input$map_neighbourhood
        data <- vancouver_trees %>%
            filter_table(FALSE, year_min, year_max, street_side, neighbourhood) %>%
            # filter trees without location or date data
            filter(!is.na(latitude),
                   !is.na(longitude))

        leaflet(data) %>%
            # set original map window to be in median position
            setView(lat = median(data$latitude, na.rm = TRUE),
                    lng = median(data$longitude, na.rm = TRUE),
                    zoom = 11.5) %>%
            addTiles() %>%
            # add circle markers for trees
            addCircleMarkers(data = data,
                       lat = ~ latitude,
                       lng = ~ longitude,
                       weight = 1,
                       radius = ~ sqrt(diameter)*3,
                       color = ~pal(height_range_id),
                       label = ~as.character(tree_id),
                       popup = ~as.character(paste(paste("Species:",
                                                   common_name),
                                                   paste("Year planted:",
                                                   year_planted),
                                                   paste("Address:",
                                                   civic_number,
                                                   std_street), sep = '<br>')),
                       fillOpacity = 0.5) %>%
            # add a legend for the marker colours
            addLegend("bottomright",
                      pal = pal,
                      values = data$height_range_id,
                      title = "Height Range ID",
                      opacity = 1)
    })

    output$distPlot <- renderPlot({
        # summarise annual number of trees planted per neighbourhood
        x <- vancouver_trees %>%
            group_by(year_planted, neighbourhood_name, decade_planted) %>%
            summarise(annual_trees_planted = n()) %>%
            filter(!is.na(decade_planted))
        binwidth <- input$binwidth
        density <- input$density

        # draw the histogram with the specified binwidth
        plot <- ggplot(x, aes(annual_trees_planted, fill = decade_planted, color = decade_planted)) +
            geom_histogram(position = "dodge",
                           alpha = 0.2, aes(y = ..density..),
                           binwidth = binwidth) +
            labs(fill = "Decade Planted",
                 color = "Decade Planted",
                 title = "Density distribution of annual number of trees planted in a neighbourhood",
                 x = "Number of trees planted in a given year")
        # if density toggled on, include density plot
        if(density) {
            plot <- plot + geom_density(alpha = 0.3)
        }
        plot
    })
}

# Run the application
shinyApp(ui = ui, server = server)
