# STAT 545 Assignment

For this assignment we are tasked with creating a Shiny app. I have chosen to create an app that explores the `vancouver_trees` dataset from the [`UBC-MDS/datateachr`](https://github.com/UBC-MDS/datateachr) package (Option B).

Link to Shiny app: https://pattiey.shinyapps.io/STAT545B/

# Dataset Information

The street tree dataset includes a listing of public trees on boulevards in the City of Vancvouer and provides data on tree coordinates, species and other related characteristics. Park trees and private trees are not included in the inventory.

For more information visit the [City of Vancouver Open Data Portal website](https://opendata.vancouver.ca/explore/dataset/street-trees/information/?disjunctive.species_name&disjunctive.common_name&disjunctive.height_range_id).

Data for this project was accessed through the [`UBC-MDS/datateachr`](https://github.com/UBC-MDS/datateachr) package.

# App Features:

This app contains three main components: Table, Tree Map, and Histogram

## Table

This page contains a table of the trees found in the dataset. Trees can be filtered by year, street side, and neighbourhood. The table can be exported as a csv.

## Tree Map

This page contains an interactive map of the trees in Vancouver with a variety of filters adjustable by the user. Hovering over trees in the map displays the `tree_id` and clicking on a tree displays information about the species, year planted, and address of the tree. Trees without location or date planted information are omitted from the map.

### User adjustable filters

* Year Planted: Display trees planted within specified range
* Street Side: Display trees with specified street sides
* Neighbourhood: Display trees in specified neighbourhood

## Histogram

This page contains a histogram displaying the distribution of the yearly number of trees planted per neighbourhood, coloured by decade in which a tree was planted.
The user is able to adjust the binwidth of the histogram. The `Density Overlay` toggle exists for if the user would like to include a density distribution overlay on the histogram or not.

# Code

All the working code for this project is contained within `code/app.R` following a standard Shiny app structure. 
