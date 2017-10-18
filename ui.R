#########################  Overview and description
# 
#   1. packages
#         
#   2. UI dashboard page
#
#
#


###########################################################################
######################### 1.
###########################################################################
#
# for postgresql connection
library(RPostgreSQL)

# for boxes and other style elements
library(shinydashboard)

# for showing the tables, i.e. specified with DT::
library(DT)

# shiny
library(shiny)

# getting geo locations (for calculating the distances for zip code radius search)
library(geosphere)

# barcharts and scatterplots are created with plotly, i.e. plotlyOutput and renderPloly
library(plotly)

# necessary for boxes
#library(shinydashboard)

# necessary to show the private and commercial dealers on map
library(leaflet)

# package for working with strings
library(stringr)

# getting geolocations (for getting the exact location of dealer, i.e. street)
library(ggmap)


###########################################################################
######################### 2.
###########################################################################
#
shinyUI(dashboardPage(
  dashboardHeader(disable = TRUE),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    uiOutput("completeUIOutput")
)))
