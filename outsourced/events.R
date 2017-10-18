
#########################  Overview and description
# 
#   1. observeEvent --> reacting to:  input$addMakeToSaved
#           Saves make, model and other characteristica to the reactive value (a table) called
#           marktstudie$savedMakes.
#         
#   2. observeEvent --> reacting to:  input$choosenYearFrom
#                                     input$choosenYearTo
#                                     input$choosenPowerFrom
#                                     input$choosenPowerTo
#                                     input$choosenColour
#                                     input$choosenMileageFrom
#                                     input$choosenMileageTo
#                                     input$choosenPriceFrom
#                                     input$choosenPriceTo)
#
#   3. observeEvent --> reacting to:  input$showDetailBool
#           Clears all reactive values to default, i.e. to "beliebig" when the checkbox showDetailBool is
#           disabeled. This shall prevent failure, when there have been wrong user input and then the 
#           detail options are closed, i.e. user inputs mileageFrom 80 and mileage 40 (not possible) but 
#           the closes the details options. Therefore, they will be set to default, i.e. "beliebig" after
#           closing.
#
#   4. observeEvent --> reacting to:  input$deleteMakeFromSave
#           Delets the selected object from the reactive value marktstudie$savedMakes (table containing all
#           choosen objects so far).
#
#   5. observeEvent --> reacting to:  input$selectZip
#           Clears the saved error messages when zip is selected
# 
#   6. observeEvent --> reacting to:  input$choosenMake
#           Clears the saved error messages when make is selected
#
#   7. observeEvent --> reacting to:  input$getQuery
#           Main part after pressing the button getQuery. Thereafter, the zip code distance is calculated,
#           the data is queried from the database, the data is transformed and the plots are created.
#


###########################################################################
######################### 1.
###########################################################################
#
observeEvent(input$addMakeToSaved, {
  
  ######### error handling start
  #
  # Old error messages will be cleared and then new messages will be saved to marktstudie$errorMessage.
  marktstudie$errorMessage <- ""
  
  # If no make is choosen, then an error message will be shown and process stops.
  if(input$choosenMake == ""){return(marktstudie$errorMessage <- paste0(marktstudie$errorMessage, "<br/>", "Bitte w\u00e4len Sie eine Modell aus"))}
  
  # If alreade 5 makes are saved, then an error message will be shown and process stops.
  if(nrow(marktstudie$savedMakes) == 5){return(marktstudie$errorMessage <- paste0(marktstudie$errorMessage, "<br/>", "Es sind maximal nur 5 Fahrzeuge ausw\u00e4hlbar"))}
  
  # Check, if make/model is already saved in list. If this is the case, an error will be shown and the
  # process stops. Therefore a short bool alreadyInList is used.
  alreadyInList <- FALSE
  if(nrow(marktstudie$savedMakes) > 0){
    for(i in 1:nrow(marktstudie$savedMakes)){
      if(marktstudie$savedMakes[i, "Marke"] == input$choosenMake & marktstudie$savedMakes[i, "Modell"] == input$choosenModel){
        alreadyInList <- TRUE
      }
    }
    if(alreadyInList){
      marktstudie$errorMessage <- paste0(marktstudie$errorMessage, "<br/>", "Fahrzeug ist schon in der Liste")
      return()
    }
  }
  
  # The following loop is for the error messages if detail search in enabeled. It will be checked, if from-values
  # are shorter than to-values (therefore all two inputs have to be not "beliebig") The boolExit ensures, that the process is not stopped immediately, but when all
  # error messages are concatenated so that multiple error messages are possible.
  boolExit <- FALSE
  if(input$showDetailBool){
    if(input$choosenMileageFrom != "beliebig" & input$choosenMileageTo != "beliebig"){
      if(as.integer(input$choosenMileageFrom) > as.integer(input$choosenMileageTo)){marktstudie$errorMessage <- paste0(marktstudie$errorMessage, "<br/>", "Km von muss kleiner Km bis sein")
      boolExit <- TRUE
      }
    }
    if(input$choosenYearFrom != "beliebig" & input$choosenYearTo != "beliebig"){
      if(input$choosenYearFrom > input$choosenYearTo){marktstudie$errorMessage <- paste0(marktstudie$errorMessage, "<br/>", "Baujahr von muss kleiner Baujahr bis sein")
      boolExit <- TRUE
      }
    }
    if(input$choosenPowerFrom != "beliebig" & input$choosenPowerTo != "beliebig"){
      if(input$choosenPowerFrom > input$choosenPowerTo){marktstudie$errorMessage <- paste0(marktstudie$errorMessage, "<br/>", "PS von muss kleiner PS bis sein")
      boolExit <- TRUE
      }
    }
    if(input$choosenPriceFrom != "beliebig" & input$choosenPriceTo != "beliebig"){
      if(input$choosenPriceFrom > input$choosenPriceTo){marktstudie$errorMessage <- paste0(marktstudie$errorMessage, "<br/>", "Preis von muss kleiner Preis bis sein")
      boolExit <- TRUE
      }
    }
  }
  if(boolExit){return()}
    
  ######### error handling end
  
  ######### save inputs in reactive value start
  #
  # If there are no wrong user inputs, then the inputs are save in the tables marktstudie$savedMakes.
  # Amount thereby is the current amount of saved makes plus 1
  amount <- nrow(marktstudie$savedMakes)+1
  marktstudie$savedMakes[amount, "Marke"] <- input$choosenMake
  marktstudie$savedMakes[amount, "Modell"] <- input$choosenModel
  marktstudie$savedMakes[amount, "Kraftstoff"] <- input$choosenFuel
  marktstudie$savedMakes[amount, "Km_min"] <- marktstudie$choosenMileageFrom
  marktstudie$savedMakes[amount, "Km_max"] <- marktstudie$choosenMileageTo
  marktstudie$savedMakes[amount, "Baujahr_min"] <- marktstudie$choosenYearFrom
  marktstudie$savedMakes[amount, "Baujahr_max"] <- marktstudie$choosenYearTo
  marktstudie$savedMakes[amount, "Preis_min"] <- marktstudie$choosenPriceFrom
  marktstudie$savedMakes[amount, "Preis_max"] <- marktstudie$choosenPriceTo
  marktstudie$savedMakes[amount, "PS_min"] <- marktstudie$choosenPowerFrom
  marktstudie$savedMakes[amount, "PS_max"] <- marktstudie$choosenPowerTo
  marktstudie$savedMakes[amount, "Farbe"] <- marktstudie$choosenColour
  marktstudie$savedMakes[amount, "Anbieter"] <- marktstudie$choosenCustomerType
  
  ######### save inputs in reactive values end
  
})


###########################################################################
######################### 2.
###########################################################################
#
#   It is necessary to save the hidden inputs in reactive elements, so that the hidden inpus can have 
#   default values. TODO: maybe a better approach would be to check inside observer 1., if the input is
#   null (i.e. if is.na(input$choosenYearFrom) and then save a default value "beliegib".
#
observeEvent(c(input$choosenYearFrom,
               input$choosenYearTo,
               input$choosenPowerFrom,
               input$choosenPowerTo,
               input$choosenColour,
               input$choosenMileageFrom,
               input$choosenMileageTo,
               input$choosenPriceFrom,
               input$choosenPriceTo,
               input$choosenCustomerType),{
                 
                 
  ######### error handling start
  #
  # Old error messages will be cleared and then new messages will be saved to marktstudie$errorMessage.
  # The following loop is for the error messages if detail search in enabeled. It will be checked, if from-values
  # are shorter than to-values (therefore all two inputs have to be not "beliebig") The boolExit ensures, that the process is not stopped immediately, but when all
  # error messages are concatenated so that multiple error messages are possible.
  marktstudie$errorMessage <- ""  
  boolExit <- FALSE
  if(input$choosenMileageFrom != "beliebig" & input$choosenMileageTo != "beliebig"){
    if(as.integer(input$choosenMileageFrom) > as.integer(input$choosenMileageTo)){marktstudie$errorMessage <- paste0(marktstudie$errorMessage, "<br/>", "Km von muss kleiner Km bis sein")
    boolExit <- TRUE
    }
  }
  if(input$choosenYearFrom != "beliebig" & input$choosenYearTo != "beliebig"){
    if(input$choosenYearFrom > input$choosenYearTo){marktstudie$errorMessage <- paste0(marktstudie$errorMessage, "<br/>", "Baujahr von muss kleiner Baujahr bis sein")
    boolExit <- TRUE
    }
  }
  if(input$choosenPowerFrom != "beliebig" & input$choosenPowerTo != "beliebig"){
    if(input$choosenPowerFrom > input$choosenPowerTo){marktstudie$errorMessage <- paste0(marktstudie$errorMessage, "<br/>", "PS von muss kleiner PS bis sein")
    boolExit <- TRUE
    }
  }
  if(input$choosenPriceFrom != "beliebig" & input$choosenPriceTo != "beliebig"){
    if(input$choosenPriceFrom > input$choosenPriceTo){marktstudie$errorMessage <- paste0(marktstudie$errorMessage, "<br/>", "Preis von muss kleiner Preis bis sein")
    boolExit <- TRUE
    }
  }
  if(boolExit){return()}
  
  ######### error handling end
  
  ######### save inputs in reactive values start
  #
  # saving the input values to the respective reactive values
  marktstudie$choosenYearFrom <- input$choosenYearFrom
  marktstudie$choosenYearTo <- input$choosenYearTo
  marktstudie$choosenPowerFrom <- input$choosenPowerFrom
  marktstudie$choosenPowerTo <- input$choosenPowerTo
  marktstudie$choosenColour <- input$choosenColour
  marktstudie$choosenMileageFrom <- input$choosenMileageFrom
  marktstudie$choosenMileageTo <- input$choosenMileageTo
  marktstudie$choosenPriceFrom <- input$choosenPriceFrom
  marktstudie$choosenPriceTo <- input$choosenPriceTo
  marktstudie$choosenCustomerType <- input$choosenCustomerType
  
  ######### save inputs to reactive values end
  
})

###########################################################################
######################### 3.
###########################################################################
#
#   setting all reactive values for hidden inputs to default to prevent wrong inputs, i.e. see description
#   on the top
#
observeEvent(input$showDetailBool ,{
  if(!input$showDetailBool){
    marktstudie$choosenYearFrom = "beliebig"
    marktstudie$choosenYearTo = "beliebig"
    marktstudie$choosenPowerFrom = "beliebig"
    marktstudie$choosenPowerTo = "beliebig"
    marktstudie$choosenColour = "beliebig"
    marktstudie$choosenMileageFrom = "beliebig"
    marktstudie$choosenMileageTo = "beliebig"
    marktstudie$choosenPriceFrom = "beliebig"
    marktstudie$choosenPriceTo = "beliebig"
    marktstudie$choosenCustomerType = "beliebig"
  }
})

###########################################################################
######################### 4.
###########################################################################
#
observeEvent(input$deleteMakeFromSave ,{
  
  ######### error messages start
  #
  # if no row is selected in table, then error will be displayed and process finishes
  marktstudie$errorMessage <- ""
  if(length(input$tableChoosenMakes_rows_selected) == 0){
    marktstudie$errorMessage <- paste0(marktstudie$errorMessage, "<br/>", "Bitte markieren Sie mindestens ein Fahrzeug")
    return()
  }
  
  ######### error messages end
  
  ######### deletion start
  
  selectedRows <- input$tableChoosenMakes_rows_selected
  marktstudie$savedMakes <- marktstudie$savedMakes[-c(selectedRows),]
  
  ######### deletion end
  
})

###########################################################################
######################### 5.
###########################################################################
#
#   deletes error message if input to selectZip input
observeEvent(input$selectZip, {
  marktstudie$errorMessage <- ""
})

###########################################################################
######################### 6.
###########################################################################
#
#   deletes error message if input to choosenMake input
observeEvent(input$choosenMake, {
  marktstudie$errorMessage <- ""
})


###########################################################################
######################### 7.
###########################################################################
#
observeEvent(input$getQuery, {
  
  
  ########## error handling start
  # 
  # if no zip is selected, then error message is shown and process finishes
  marktstudie$errorMessage <- ""
  if(input$selectZip == ""){
    marktstudie$errorMessage <- paste0(marktstudie$errorMessage, "<br/>", "Bitte w\u00e4len Sie eine PLZ/Ort aus")
    return()
  }
  
  ########## error handling end
  
  ########## calculate distance start
  #
  withProgress(message = "Starte Umkreissuche", value = 0.1, {
  
  marktstudie$enteredZip <- input$selectZip
    
  # extract only Zip from input (input is zip and name of region etc.)
  originZip <- unlist(strsplit(input$selectZip, " "))[1]
  
  print("Start calculating distances")
  print("Origin zip:")
  print(originZip)
  
  marktstudie$radius <- input$selectRadius
  
  # get radius and latitude and longitude from originZip (originZip = input from user)
  radius <- as.numeric(input$selectRadius)
  targetLatLong <- zipCodes[zipCodes["Postal.Code"]==originZip, c("Latitude", "Longitude")] 
  zipCodes["Distances"] <- NA
  
  # for every Zip in the table, the distance from the originZip is calculated and then compared to radius
  for(i in 1:nrow(zipCodes)){
    zipCode <- zipCodes[i,c("Latitude", "Longitude")]
    zipCodes[i, "Distance"] <- distm (c(targetLatLong[1][[1]],targetLatLong[2][[1]]) , c(zipCode[1][[1]], zipCode[2][[1]]), fun = distHaversine)/1000
  }
  ggg <<- zipCodes
  
  targetZipCodes <- zipCodes[zipCodes["Distance"]<radius, "Postal.Code"]
  
  # concatenate ZIPs into one string, that this string can be used for the query as a character string
  choosenZip <- targetZipCodes
  choosenZip <- paste(choosenZip, collapse = "','")
  choosenZip <- paste0("('", choosenZip, "')")
  
  print("Finished calculation distances")
  print("Target ZIP Codes are:")
  print(targetZipCodes)
  
  })
  
  ########## calculate distance end
  
  ########## querying start
  
  withProgress(message = "Starte Abfrage von Datenbank", value = 0.4, {
  print("Start query")
  
  # connection settings 
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, dbname="auto", host="pg-analytics.lotserver.de", port=54321, user="rolot", password="9b8Y9chlRY")

  marktstudie$dataFromQuery <- data.frame()
  
  # for each make in the list of saved makes, a query is executed in loops
  for(i in 1:nrow(marktstudie$savedMakes)){
      
      # database needs id of make and model and not name, therefore transforming name to id using the CSVs
      # makeCSV and model CSV loaded via the file loadFiles.R
      choosenIDMake <- makeCSV[makeCSV["make_name"]==marktstudie$savedMakes[i, "Marke"], "make_id"]
      choosenIDModel <- modelCSV[modelCSV["model_name"]==marktstudie$savedMakes[i, "Modell"] & modelCSV["make_id"]==choosenIDMake, "model_id"]
      
      # database needs shortcut for fuel and not name, therefore transforming name to shortcut via the function
      # transformLabelIdFuel
      fuelId <- transformLabelIdFuel(marktstudie$savedMakes[i, "Kraftstoff"])
      
      # database needs id for color and not name, therefore transforming name to shortcut via the function
      # transformLabelIdColour
      colourId <- transformLabelIdColour(marktstudie$savedMakes[i, "Farbe"], colourLabelID)
      
      if(marktstudie$savedMakes[i, "Anbieter"] == "beliebig"){customerID <- "beliebig"}
      if(marktstudie$savedMakes[i, "Anbieter"] == "kommerziell"){customerID <- "D"}
      if(marktstudie$savedMakes[i, "Anbieter"] == "privat"){customerID <- "P"}
      
      print(customerID)
    
      # the actual query is executed in function queryFromDatabase. The results are row-binded to dataFromQuery so
      # that for each makes query the data are saved.
      
      marktstudie$dataFromQuery <- rbind(marktstudie$dataFromQuery, queryFromDatabase(con,
                                                              choosenIDMake,
                                                              choosenIDModel,
                                                              choosenZip,
                                                              "2017-03-26",
                                                              "2017-04-26",
                                                              marktstudie$savedMakes[i, "Baujahr_min"],
                                                              marktstudie$savedMakes[i, "Baujahr_max"],
                                                              marktstudie$savedMakes[i, "Preis_min"],
                                                              marktstudie$savedMakes[i, "Preis_max"],
                                                              marktstudie$savedMakes[i, "PS_min"],
                                                              marktstudie$savedMakes[i, "PS_max"],
                                                              marktstudie$savedMakes[i, "Km_min"],
                                                              marktstudie$savedMakes[i, "Km_max"],
                                                              colourId,
                                                              fuelId,
                                                              customerID
                                                              ))
  }
  
  nnnData <<- marktstudie$dataFromQuery
  
  })
  
  
  # if no data could be found, then the reative values are set to true. boolNoReults triggers...TODO
  # and boolSearchFinished triggers ...
  if(nrow(marktstudie$dataFromQuery)==0){
    marktstudie$boolNoResults <- TRUE
    marktstudie$boolSearchFinished <- TRUE
    return()
  }
  
  
  dbDisconnect(con)
  
  print("Query finished")
  
  ########## querying end
  
  ########## transforming data start

  withProgress(message = "Aggregiere Daten", value = 0.6, {  
  print("Transforming data starts")
  
  marktstudie$dataFromQuery[["name"]][is.na(marktstudie$dataFromQuery[["name"]])] <- ""
  
  
  # the database delivers make and model as id, not as name. User want to see names, therefore transforming
  # ids to names using functin renameIDToName
  marktstudie$dataFromQuery <- renameIDToName(marktstudie$dataFromQuery, modelCSV, makeCSV)
  
  # database delibers id and shortcuts for fuel, usages state and color. User wants to see names, therefore 
  # transforming fuel, usage state and color to names using function renameIDToNameFuelUsageColor (each
  # for commercial as well as private data) TODO: better do this right after getting data from query
  marktstudie$dataFromQuery <- renameIDToNameFuelUsageColor(marktstudie$dataFromQuery)
  
  # there was the problem, that some names have been as D as well as P in the database. Deviding the customers in private and commercial then is a problem, when the same costumer
  # is saved as D and P on the database on the same time. Therefore, such cases will be converted to P, i.e. private customers.
  changeCustomerStat <- as.vector(na.omit(match(marktstudie$dataFromQuery[["name"]][marktstudie$dataFromQuery[["customer_type"]] == "P"], marktstudie$dataFromQuery[["name"]][marktstudie$dataFromQuery[["customer_type"]] == "D"])))
  changeCustomerStat <- marktstudie$dataFromQuery[["name"]][marktstudie$dataFromQuery[["customer_type"]] == "D"][changeCustomerStat]
  for(i in 1:length(changeCustomerStat)){
    marktstudie$dataFromQuery[["customer_type"]][marktstudie$dataFromQuery[["name"]] == changeCustomerStat[i]] <- "P"
  }
  
  # For private customers, the name as well as the street should not be displayed. Therefore, a new colum namePrivate is generated (where all entries are "private"). For private
  # cosumters, this column will be shown instead of column name (the name column of the private customers is still needed to show the customers cars in the tab "Karte")
  marktstudie$dataFromQuery[["namePrivate"]][marktstudie$dataFromQuery[["customer_type"]]=="P"] <- rep("Privat", nrow(marktstudie$dataFromQuery[marktstudie$dataFromQuery[["customer_type"]]=="P", ]))
  marktstudie$dataFromQuery[["street"]][marktstudie$dataFromQuery[["customer_type"]]=="P"] <- NA
  
  # transforming characters to UTF-8 to display german letters
  characters <- c("name", "city", "street")
  for(i in characters){
    Encoding(marktstudie$dataFromQuery[i][[1]]) <- "UTF-8"
  }
  
  
  # to plot the geolocations of the dealers, the long and lat is provided via the function getGeoLocations
  addressesLongLat <-getGeoLocations(marktstudie$dataFromQuery)
  
  ttt <<- targetLatLong
  
  for(i in 1:nrow(marktstudie$dataFromQuery)){
    marktstudie$dataFromQuery[["distance"]][i] <- distm (c(targetLatLong[1][[1]],targetLatLong[2][[1]]) , c(marktstudie$dataFromQuery[["Lat"]][i], marktstudie$dataFromQuery[["Long"]][i]), fun = distHaversine)/1000
  }
  
  ukukuk <<- marktstudie$dataFromQuery
  
  
  # to show the aggregated data, original data (dataFromQuery) is aggregated into two tables (commercial and 
  # private) using the function aggregateData
  marktstudie$aggregatedCommercialTable <- aggregateData(data = marktstudie$dataFromQuery)[[1]]
  marktstudie$aggregatedPrivateTable <- aggregateData(data = marktstudie$dataFromQuery)[[2]]
  
  
  
  # it can be the case, that there could not be found data for commercial or private dealers. In this case, a 
  # reactive value (marktstudie$markerResults) will be set to onlyCommercial or onlyPrivate respetiveley. Thereby, 
  # the function aggregateData delivers NULL if there is no data for a table
  marktstudie$markerResults <- "all"
  if(!is.null(marktstudie$aggregatedCommercialTable) & is.null(marktstudie$aggregatedPrivateTable)){marktstudie$markerResults <- "onlyCommercial"}
  if(is.null(marktstudie$aggregatedCommercialTable) & !is.null(marktstudie$aggregatedPrivateTable)){marktstudie$markerResults <- "onlyPrivate"}
  setProgress(0.7)
  
  # the not aggregated detailed tables for commercial as well as private cars is delivered via the function
  # aggregateData and saved in reactive values
  marktstudie$dataCommercial <- aggregateData(data = marktstudie$dataFromQuery)[[3]]
  marktstudie$dataPrivate <- aggregateData(data = marktstudie$dataFromQuery)[[4]]
  
  nnnDataCommercial <<- marktstudie$dataCommercial
  nnnDataPrivate <<- marktstudie$dataPrivate
  
  })
  
  ########## transforming data end
  
  ########## plotting data start
  
  withProgress(message = "Erstelle Grafiken", value = 0.8,{
  
  
  # the plot showing dealers on map is created using the fuction plotGeoLocations 
  marktstudie$geoLocationsPlots <- plotGeoLocations(addressesLongLat)
  
  # price histogramm for commercial and private cars on first box (Ueberblick) is created
  marktstudie$plotHistCommercial <- generatePlotHistoDensityCommercial(marktstudie$dataCommercial,  marktstudie$savedMakes)
  marktstudie$plotHistPrivate <- generatePlotHistoDensityPrivate(marktstudie$dataPrivate, marktstudie$savedMakes)
  
  plot11 <<-marktstudie$plotHistCommercial 
  
  # bar charts amount and lifetime for commercial and private cars on first box (Ueberblick) is created
  marktstudie$plotHistAmount<- generatePlotHistosAmountLifetime(marktstudie$aggregatedCommercialTable, marktstudie$aggregatedPrivateTable)[[1]]
  marktstudie$plotHistLifetime<- generatePlotHistosAmountLifetime(marktstudie$aggregatedCommercialTable, marktstudie$aggregatedPrivateTable)[[2]]
  
  
  # scatterplot price mileage for commercial and private cars on first box (Ueberblick) is created
  marktstudie$plotScatter1Commercial <- generateScatterCommercial(marktstudie$dataCommercial)[[1]]
  marktstudie$plotScatter1Private <- generateScatterPrivate(marktstudie$dataPrivate)[[1]]
  
  plot22 <<- marktstudie$plotScatter1Commercial
  
  # scatterplot price year for commercial and private cars on first box (Ueberblick) is created
  marktstudie$plotScatter2Commercial <- generateScatterCommercial(marktstudie$dataCommercial)[[2]]
  marktstudie$plotScatter2Private <- generateScatterPrivate(marktstudie$dataPrivate)[[2]]
  
  # legend for all plos is disabled, therefore generating ownl legend on top of box Ueberblick
  marktstudie$myLegend <- createLegend(marktstudie$savedMakes[["Marke"]],marktstudie$savedMakes[["Modell"]])
  
  })
  
  ########## plotting data end
  
  ########## setting bool values start
  #
  # TODO: booled values are for
  #
  marktstudie$boolSearchFinished <- TRUE
  marktstudie$boolNoResults <- FALSE
  
  # setting the reactive value to true and false is on porpuse. This triggers the renderUI of the search box
  # otherwise, the box would not collapse after!! the first search was executed but no inputs have been changed
  # (the output only renders, if some values change. After the first search, the bool value is already TRUE, therefore
  # the output is not rendered and the box will not close)
  marktstudie$boolSearchCollapsed <- FALSE
  marktstudie$boolSearchCollapsed <- TRUE
  
  ########## setting bool values end
  
  # search box will colla
  #updateSelectizeInput(session = session, inputId = 'selectZip', choices = zipCodesSearch, server = TRUE)
  
  marktstudie$choosenYearFrom = "beliebig"
  marktstudie$choosenYearTo = "beliebig"
  marktstudie$choosenPowerFrom = "beliebig"
  marktstudie$choosenPowerTo = "beliebig"
  marktstudie$choosenColour = "beliebig"
  marktstudie$choosenMileageFrom = "beliebig"
  marktstudie$choosenMileageTo = "beliebig"
  marktstudie$choosenPriceFrom = "beliebig"
  marktstudie$choosenPriceTo = "beliebig"
  
  
})



