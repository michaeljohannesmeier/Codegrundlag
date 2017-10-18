#########################  Overview and description
# 
#   1. Global settings
#         
#   2. Server function
#         The server function sources all necessary files
#
#
#

###########################################################################
######################### 1.
###########################################################################
#
# the option is to force R to show the number in full, i.e. dont show 1+e6 but show 1000000
options("scipen"=100, "digits"=4)

# table container for tabel aggregatedCommercial
sketch = htmltools::withTags(table(
  class = 'display',
  thead(
    tr(
      th(rowspan = 2, 'Marke'),
      th(rowspan = 2, 'Modell'),
      th(rowspan = 2, 'PLZ'),
      th(colspan = 5, 'gewerbliche Anbieter'),
      th(colspan = 5, 'private Anbieter')
    ),
    tr(
      lapply(rep(c('Anzahl', 'Durchschnittliche Standzeit', 'Durchschnittspreis', 'Max. Preis', 'Min. Preis'), 2), th)
    )
  )
))


opacity <- 0.8




###########################################################################
######################### 2.
###########################################################################
#
shinyServer(function(input, output, session) {
  
  ######################### inloads all files
  #
  # the following files will be laoded 
  #   1. make.csv, 
  #   2. model.csv
  #   3. de_postal_codes.txt
  #   4. colourIdLabel.csv 
  #
  source("outsourced/loadFiles.R", local = TRUE)
  
  #########################  laods all functions
  # 
  # the following functions will be loaded
  #   1. queryFromDatabase
  #   2. aggregateData
  #   3. renameIDToName
  #   4. generatePlotHistoDensityCommercial
  #   5. generatePlotHistoDensityPrivate
  #   6. generatePlotHistosAmountLifetime
  #   7. generateScatterCommercial
  #   8. generateScatterPrivate
  #   9. getGeoLocations
  #   10. plotGeoLocations
  #   11. getLongLat
  #   12. transformLabelIdFuel
  #   13. transformLabelIdColour
  #   14. createLegend
  #   15. renameIDToNameFuelUsageColor
  #   
  source("outsourced/functions.R", local = TRUE)
  
  #########################  laods all reactive values
  #
  source("outsourced/reactiveValues.R", local = TRUE)
  
  #########################  laods different other definitions and reorganisations
  #
  source("outsourced/others.R", local = TRUE)
  
  #########################  laods all output objects which will be rendered
  #
  source("outsourced/outputRenders.R", local = TRUE)
  
  #########################  laods all events (observe, observeEvents, reactives, eventReactives)
  #
  # the following events will be loaded
  #   1. observeEvent --> reacting to:  input$addMakeToSaved
  #   2. observeEvent --> reacting to:  input$choosenYearFrom
  #                                     input$choosenYearTo
  #                                     input$choosenPowerFrom
  #                                     input$choosenPowerTo
  #                                     input$choosenColour
  #                                     input$choosenMileageFrom
  #                                     input$choosenMileageTo
  #                                     input$choosenPriceFrom
  #                                     input$choosenPriceTo)
  #   3. observeEvent --> reacting to:  input$showDetailBool
  #   4. observeEvent --> reacting to:  input$deleteMakeFromSave
  #   5. observeEvent --> reacting to:  input$getQuery
  #   6. observeEvent --> reacting to:  input$deleteMakeFromSave
  #   7. observeEvent --> reacting to:  input$selectZip
  #   8. observeEvent --> reacting to:  input$choosenMake
  #   9. observeEvent --> reacting to:  input$getQuery
  #
  source("outsourced/events.R", local = TRUE)

})
