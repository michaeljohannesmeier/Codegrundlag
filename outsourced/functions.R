

#########################  Description and overview
# 
#   1. queryFromDatabase
#         Query the data from database
#
#   2. aggregateData
#         Takes original data from database and delivers 4 tables. The aggregated tables for commercial and private dealers
#         and the detail tables for commercial and private dealers
#
#   3. renameIDToName
#         renames id of model and make to name using modelCSV and makeCSV
#
#   4. generatePlotHistoDensityCommercial
#         Generates price histogram for commercial dealers shown in box Ueberblick
#
#   5. generatePlotHistoDensityPrivate
#         Generates price histogram for private dealers shown in box Ueberblick
#
#   6. generatePlotHistosAmountLifetime
#         Generate two bar charts: bar chart showing amount of aggregated commercial and private dealers as well as
#         bar chart of lifetime of aggregated commercial and private dealers
#
#   7. generateScatterCommercial
#         Generates two plots (scatterplot for price mileage and price year) for commercial dealers
#
#   8. generateScatterPrivate
#         Generates two plots (scatterplot for price mileage and price year) for private dealers
#
#   9. getGeoLocations
#         Get the longitude and latitude for the full address of dealers
#
#   10. getLongLat
#         Small helper function for getGeoLocations
#
#   11. plotGeoLocations
#         Generates a plot showing dealers on map
#
#   12. transformLabelIdFuel
#   13. transformLabelIdColour
#   14. createLegend
#   15. renameIDToNameFuelUsageColor
#   


###########################################################################
######################### 1.
###########################################################################
#
queryFromDatabase <- function(con, make, model, zip, dateFrom, dateTo, yearFrom, yearTo, priceFrom, priceTo, powerFrom, powerTo, mileageFrom, mileageTo, colour, fuel, customer) {
  
  # creatin sting for query. Will be empty "" or for example "year >= 2005 and" whereby the inputs, i.e. the parameters of the function is used
  yearFromQuery <- ifelse(yearFrom == "beliebig", "", paste0("year >= ", yearFrom, " and "))
  yearToQuery <- ifelse(yearTo == "beliebig", "", paste0("year <= ", yearTo, " and "))
  mileageFromQuery <- ifelse(mileageFrom == "beliebig", "", paste0("mileage >= ", mileageFrom, " and "))
  mileageToQuery <- ifelse(mileageTo == "beliebig", "", paste0("mileage <= ", mileageTo, " and "))
  powerFromQuery <- ifelse(powerFrom == "beliebig", "", paste0("power >=", powerFrom, " and "))
  powerToQuery <- ifelse(powerTo == "beliebig", "", paste0("power <=", powerTo, " and "))
  priceFromQuery <- ifelse(priceFrom == "beliebig", "", paste0("price >=", priceFrom, " and "))
  priceToQuery <- ifelse(priceTo == "beliebig", "", paste0("price <=", priceTo, " and "))
  colourQuery <- ifelse(colour == "beliebig", "", paste0("fu.body_colour = '", colour, "' and "))
  fuelQuery <- ifelse(fuel == "beliebig", "", paste0("fu.fuel = '", fuel, "' and "))
  customerQuery <- ifelse(customer == "beliebig", "", paste0("cu.customer_type = '", customer, "' and "))
  
  print(customerQuery)
  
  dbGetQuery(con, paste0("
                        select 
                         cu.customer_name as name,
                         (cu.addresses[1]).country as country,
                         (cu.addresses[1]).zip_code as zip,
                         (cu.addresses[1]).city as city,
                         cu.addresses[1].street as street, 
                         make as make, 
                         model as model,
                         min(price) as price,
                         fa.obid as obid,
                         max(fa.created) as seen,
                         min(fu.created) as created,
                         fu.power as power, 
                         fu.year as year, 
                         fu.fuel as fuel,
                         fu.mileage as mileage,
                         fu.title as title,
                         fu.usage_state as usagestate,
                         cu.customer_type,
                         fu.body_colour as colour
                         from 
                         public.fast fa, 
                         public.full fu, 
                         public.customer cu
                         where ",
                         yearFromQuery,
                         yearToQuery,
                         powerFromQuery,
                         powerToQuery,
                         priceFromQuery,
                         priceToQuery,
                         colourQuery,
                         fuelQuery,
                         mileageFromQuery,
                         mileageToQuery,
                         customerQuery,
                         #power >= '", powerFrom, "' and
                         #power >= '", powerTo, "' and
                         "make = '", make, "' and
                         model = '", model, "' and
                         fu.usage_state != 'A' and
                         fa.obid=fu.obid and 
                         fa.portal=fu.portal and 
                         fu.customer_id=cu.customer_id and 
                         fu.portal=cu.portal and
                         -- (cu.addresses[1]).country='D' and
                         (cu.addresses[1]).zip_code IN ", zip,"and
                         cu.customer_type in ('P', 'D') and
                         fa.created>='", dateFrom, "' and
                         fa.created<='", dateTo,"'
                        group by 1,2,3,4,5,6,7,9,12,13,14,15,16,17,18,19
                         "))
}


###########################################################################
######################### 2.
###########################################################################
#
#         Takes original data from database and delivers 4 tables. The aggregated tables for commercial and private dealers
#         and the detail tables for commercial and private dealers
#
aggregateData <- function(data){
  
  # creating the lifetime of all cars
  data["lifetime"] <- as.numeric(as.Date(data["seen"][[1]])-as.Date(data["created"][[1]])+1)
  
  # row of ones makes sum over cars possible
  data["amount"] <- rep(1, nrow(data))
  
  # split data into private data only
  dataPrivate <- data[data[["customer_type"]]=="P",]
  dataPrivate <- dataPrivate[!is.na(dataPrivate[["price"]]), ]
  
  
  # check if data for private customers are found, if yes, aggregate, if not set tables to NULL
  if(nrow(dataPrivate) > 0){
  
    # for each aggregation, a different FUN is used. TODO: maybe there is a better way to aggregate this in one step
    dataPrivateAmount <- aggregate(dataPrivate$amount, by=list(dataPrivate$make, dataPrivate$model), FUN=sum)
    dataPrivatePriceMean <- aggregate(dataPrivate$price, by=list(dataPrivate$make, dataPrivate$model), FUN=mean)
    dataPrivatePriceMin <- aggregate(dataPrivate$price, by=list(dataPrivate$make, dataPrivate$model), FUN=min)
    dataPrivatePriceMax <- aggregate(dataPrivate$price, by=list(dataPrivate$make, dataPrivate$model), FUN=max)
    dataPrivateLifetimeMean <- aggregate(dataPrivate$lifetime, by=list(dataPrivate$make, dataPrivate$model), FUN=mean)
    colnames(dataPrivateAmount)<-c("make", "model", "privateAmount")
    colnames(dataPrivatePriceMean)<-c("make", "model", "privatePriceMean")
    colnames(dataPrivatePriceMin)<-c("make", "model", "privatePriceMin")
    colnames(dataPrivatePriceMax)<-c("make", "model", "privatePriceMax")
    colnames(dataPrivateLifetimeMean)<-c("make", "model", "privateLifetimeMean")
    
    # merge the aggregates into one table and round the results
    dataPrivateAggregate <- merge(dataPrivateAmount,dataPrivatePriceMean, all.x = TRUE)
    dataPrivateAggregate <- merge(dataPrivateAggregate,dataPrivatePriceMin, all.x = TRUE)
    dataPrivateAggregate <- merge(dataPrivateAggregate,dataPrivatePriceMax, all.x = TRUE)
    dataPrivateAggregate <- merge(dataPrivateAggregate,dataPrivateLifetimeMean, all.x = TRUE)
    dataPrivateAggregate[,c(3:ncol(dataPrivateAggregate))] <- round(dataPrivateAggregate[,c(3:ncol(dataPrivateAggregate))],2)
  } else {
    dataPrivate <- NULL
    dataPrivateAggregate <- NULL
  }
  
  # split data into commercial data only
  dataCommercial <- data[data[["customer_type"]]=="D",]
  dataCommercial <- dataCommercial[!is.na(dataCommercial[["price"]]), ]
  
  # check if data for commercial customers are found, if yes, aggregate, if not set tables to NULL
  if(nrow(dataCommercial) > 0){
  
    # for each aggregation, a different FUN is used. TODO: maybe there is a better way to aggregate this in one step
    dataCommercialAmount <- aggregate(dataCommercial$amount, by=list(dataCommercial$make, dataCommercial$model), FUN=sum)
    dataCommercialPriceMean <- aggregate(dataCommercial$price, by=list(dataCommercial$make, dataCommercial$model), FUN=mean)
    dataCommercialPriceMin <- aggregate(dataCommercial$price, by=list(dataCommercial$make, dataCommercial$model), FUN=min)
    dataCommercialPriceMax <- aggregate(dataCommercial$price, by=list(dataCommercial$make, dataCommercial$model), FUN=max)
    dataCommercialLifetimeMean <- aggregate(dataCommercial$lifetime, by=list(dataCommercial$make, dataCommercial$model), FUN=mean)
    colnames(dataCommercialAmount)<-c("make", "model", "commercialAmount")
    colnames(dataCommercialPriceMean)<-c("make", "model", "commercialPriceMean")
    colnames(dataCommercialPriceMin)<-c("make", "model", "commercialPriceMin")
    colnames(dataCommercialPriceMax)<-c("make", "model", "commercialPriceMax")
    colnames(dataCommercialLifetimeMean)<-c("make", "model", "commercialLifetimeMean")
    
    # merge the aggregates into one table and round the results
    dataCommercialAggregate <- merge(dataCommercialAmount,dataCommercialPriceMean, all.x = TRUE)
    dataCommercialAggregate <- merge(dataCommercialAggregate,dataCommercialPriceMin, all.x = TRUE)
    dataCommercialAggregate <- merge(dataCommercialAggregate,dataCommercialPriceMax, all.x = TRUE)
    dataCommercialAggregate <- merge(dataCommercialAggregate,dataCommercialLifetimeMean, all.x = TRUE)
    dataCommercialAggregate[,c(3:ncol(dataCommercialAggregate))] <- round(dataCommercialAggregate[,c(3:ncol(dataCommercialAggregate))],2)
  } else {
    dataCommercial <- NULL
    dataCommercialAggregate <- NULL
  }
  
  # return all tables
  return(list(
    dataCommercialAggregate,
    dataPrivateAggregate,
    dataCommercial,
    dataPrivate
  ))
  
}

###########################################################################
######################### 3.
###########################################################################
#
#         renames id of model and make to name using modelCSV and makeCSV
#
renameIDToName <- function(data, modelCSV, makeCSV) {
  
  # for all rows, check if make_id is found in CSV, then make_name will be saved into data, otherwise "" will be saved
  for(i in 1: nrow(data)){
    
    if(length(as.character(makeCSV[makeCSV["make_id"]==data[i, "make"],"make_name"]))==0 | is.na(data[i, "make"])){
      data[i, "make"] <- ""
    } else {
      data[i, "make"] <- as.character(makeCSV[makeCSV["make_id"]==data[i, "make"],"make_name"])
    }
    if(length(as.character(modelCSV[modelCSV["model_id"]==data[i, "model"],"model_name"])) == 0 | is.na(data[i, "model"])){
      data[i, "model"] <- ""
    } else {
      data[i, "model"] <- as.character(modelCSV[modelCSV["model_id"]==data[i, "model"],"model_name"])
    }
  }
  return(data)
}


###########################################################################
######################### 4.
###########################################################################
#
#         Generates price histogram for commercial dealers shown in box Ueberblick
#
generatePlotHistoDensityCommercial <- function(dataCommercial, listMakeModel){
  
  # if there is no commercial data, then plot will not be shown
  if(is.null(dataCommercial)){return(NULL)}
  
  # title text size will be smaller than default
  myFontHead <- list(size = 8)
  myFontAxis <- list(size = 7)
  
  # sets the plot
  plotCommercial <- plot_ly(alpha = 0.6)
  
  # add traces for all saved makes and models
  for(i in 1:nrow(listMakeModel)){
    
    # there can be the case, that for a make less than 2 observations are found. The density functions needs at least two observations to calculate the density
    # Therefore, the model will be skipped 
    # if(length(na.omit(dataCommercial$price[dataCommercial[["make"]] == listMakeModel[["Marke"]][i] & dataCommercial[["model"]] == listMakeModel[["Modell"]][i]]))<=1){
    #   next
    # }
    
    # create density values for price. NA omit is used, because the funcionn density can not handle nas
    #dataHistCommercial <- density(na.omit(dataCommercial$price[dataCommercial[["make"]] == listMakeModel[["Marke"]][i] & dataCommercial[["model"]] == listMakeModel[["Modell"]][i]]))
    
    # the density function gives values smaller than 0. This values will be cut off
    #dataHistCommercial <- data.frame(x=dataHistCommercial$x[dataHistCommercial$x>0], y=dataHistCommercial$y[dataHistCommercial$x>0])
    
    # name of make
    name <- paste(listMakeModel[["Marke"]][i], listMakeModel[["Modell"]][i])
    
    # generates the actual plot
    # plotCommercial <- add_trace(plotCommercial, x = dataHistCommercial$x, y = dataHistCommercial$y, type = 'scatter', mode = 'lines', textfont = list(size = 6),
    #                             name = name, line = list(color = colorListLinesMarkers[i])) %>%
    #   layout(title = "H\u00e4ufigkeit kommerzieller Fahrzeuge (Preis)", font = myFontHead, xaxis = list(title = "Preis", font = myFontAxis), yaxis = list(title = "H\u00e4ufigkeit",
    #     font = myFontAxis), showlegend = FALSE)%>% 
    #   config(displayModeBar = F)
    
    start <- min(na.omit(dataCommercial$price))/1000
    end <- max(na.omit(dataCommercial$price))/1000
    size <- (end - start) / 10
    
    tickvals <- seq(start, end, size)
    tickvals <- round(tickvals, 1)
    
    plotCommercial <- add_trace(plotCommercial, x = (dataCommercial$price[dataCommercial[["make"]] == listMakeModel[["Marke"]][i] & dataCommercial[["model"]] == listMakeModel[["Modell"]][i]])/1000,
                                type = 'histogram', textfont = list(size = myFontAxis), opacity = opacity,
                                autobinx = FALSE, hoverinfo = "y",
                                xbins = list(start = start, end = end, size = size),
                                name = name, marker = list(color = colorListLinesMarkers[i])) %>%
      layout(xaxis = list(showgrid = TRUE, autotick = FALSE, tickmode = "array", tickvals = tickvals, title = "Preis in Tsd", font = myFontAxis, tickangle = 45),
             title = "H\u00e4ufigkeit kommerzieller Fahrzeuge (Preis)", font = myFontHead, 
             yaxis = list(title = "H\u00e4ufigkeit",font = myFontAxis), 
             showlegend = FALSE, 
             bargap=0.05, 
             barmode = "stack")%>%
      config(displayModeBar = F)
    
  }
  
  return(plotCommercial)
}

###########################################################################
######################### 5.
###########################################################################
#
#         Generates price histogram for private dealers shown in box Ueberblick
#
generatePlotHistoDensityPrivate <- function(dataPrivate, listMakeModel){
  
  # if there is no private data, then plot will not be shown
  if(is.null(dataPrivate)){return(NULL)}
  
  # title text size will be smaller than default
  myFontHead <- list(size = 8)
  myFontAxis <- list(size = 7)
  
  # sets the plot
  plotPrivate <- plot_ly(alpha = 0.6)
  
  # add traces for all saved makes and models
  for(i in 1:nrow(listMakeModel)){
    
    # there can be the case, that for a make less than 2 observations are found. The density functions needs at least two observations to calculate the density
    # Therefore, the model will be skipped
    # if(length(na.omit(dataPrivate$price[dataPrivate[["make"]] == listMakeModel[["Marke"]][i] & dataPrivate[["model"]] == listMakeModel[["Modell"]][i]]))<=1){
    #   next
    # }
    
    # create density values for price. NA omit is used, because the funcionn density can not handle nas
    #dataHistPrivate <- density(na.omit(dataPrivate$price[dataPrivate[["make"]] == listMakeModel[["Marke"]][i] & dataPrivate[["model"]] == listMakeModel[["Modell"]][i]]))
    
    # the density function gives values smaller than 0. This values will be cut off
    #dataHistPrivate <- data.frame(x=dataHistPrivate$x[dataHistPrivate$x>0], y=dataHistPrivate$y[dataHistPrivate$x>0])
    
    # name of make
    name <- paste(listMakeModel[["Marke"]][i], listMakeModel[["Modell"]][i])
    
    # generates the actual plot
    # plotPrivate <- add_trace(plotPrivate, x = dataHistPrivate$x, y = dataHistPrivate$y, type = 'scatter', mode = 'lines',
    #                          name = name, line = list(color = colorListLinesMarkers[i])) %>%
    #   layout(title = "H\u00e4ufigkeit privater Fahrzeuge (Preis)", font = myFontHead, xaxis = list(title = "Preis", font = myFontAxis), yaxis = list(title = "H\u00e4ufigkeit", font = myFontAxis),
    #          showlegend = FALSE)%>% 
    #   config(displayModeBar = F)
    
    start <- min(na.omit(dataPrivate$price))/1000
    end <- max(na.omit(dataPrivate$price))/1000
    size <- (end - start) / 10
    
    tickvals <- seq(start, end, size)
    tickvals <- round(tickvals, 1)
    
    textFrom <- seq(start, end, size)[-length(seq(start, end, size))]
    textTo <- seq(start, end, size)[-1]
    text <- paste(textFrom, "-", textTo)
    print(tickvals)
    #hoverinfo = "text", text = ~text
    
    plotPrivate <- add_trace(plotPrivate, x = (dataPrivate$price[dataPrivate[["make"]] == listMakeModel[["Marke"]][i] & dataPrivate[["model"]] == listMakeModel[["Modell"]][i]])/1000,
                             type = 'histogram', name = name, marker = list(color = colorListLinesMarkers[i]), opacity = opacity, hoverinfo = "y",
                             autobinx = FALSE, xbins = list(start = start, end = end, size = size)) %>%
      layout(title = "H\u00e4ufigkeit privater Fahrzeuge (Preis)", font = myFontHead, 
             xaxis = list(showgrid = TRUE, autotick = FALSE, tickmode = "array", tickvals = tickvals, title = "Preis in Tsd", font = myFontAxis, tickangle = 45), 
             yaxis = list(title = "H\u00e4ufigkeit", font = myFontAxis),
             showlegend = FALSE, 
             bargap=0.05, 
             barmode = "stack")%>% 
      config(displayModeBar = F)
    
  }
  return(plotPrivate)
}

###########################################################################
######################### 6.
###########################################################################
#
#         Generate two bar charts: bar chart showing amount of aggregated commercial and private dealers as well as
#         bar chart of lifetime of aggregated commercial and private dealers.
#
generatePlotHistosAmountLifetime <- function(aggregatedCommercialData, aggregatedPrivateData){
  
  ######### commercial plot amount starts
  
  # define size of heading
  myFontHead <- list(size = 8)
  
  # set the plot
  plotAmount <- plot_ly(alpha = 0.8)
  
  
  # only generate plot, if there is data for commercial customers
  if(!is.null(aggregatedCommercialData)){
    
    # names of all makes and models
    xLabels <- paste(aggregatedCommercialData$make, aggregatedCommercialData$model)
    
    # title
    title <- "Anzahl Fahrzeuge"
    
    # the title is overwritten, depending if only commercial or only private data was found
    if(marktstudie$markerResults == "onlyCommercial"){title <- "Anzahl kommerzielle Fahrzeuge"}
    if(marktstudie$markerResults == "onlyPrivate"){title <- "Anzahl privater Fahrzeuge"}
    
    # generate the actual plot. Config(displayModeBar) prohibist plotly options.
    plotAmount <- add_trace(plotAmount, x = xLabels, y = aggregatedCommercialData$commercialAmount, name = "kommerziell",
                            type = "bar", hoverinfo = "y", marker = list(color = colorListBars[1]))%>% layout(barmode = 'group', title = title, font = myFontHead, showlegend = FALSE)%>% 
      config(displayModeBar = F)
  }
  
  ######### commercial plot amount ends
  
  ######### private plot amount starts
  
  # only generate plot, if there is data for private customers
  if(!is.null(aggregatedPrivateData)){
    
    # names of all makes and models
    xLabels <- paste(aggregatedPrivateData$make, aggregatedPrivateData$model)
    
    # title
    title <- "Anzahl Fahrzeuge"
    
    # the title is overwritten, depending if only commercial or only private data was found
    if(marktstudie$markerResults == "onlyCommercial"){title <- "Anzahl kommerzielle Fahrzeuge"}
    if(marktstudie$markerResults == "onlyPrivate"){title <- "Anzahl privater Fahrzeuge"}
    
    # generates the actual plot
    plotAmount <- add_trace(plotAmount, x = xLabels, y = aggregatedPrivateData$privateAmount, name ="privat",
                            type = "bar", hoverinfo = "y", marker = list(color = colorListBars[2])) %>% layout(barmode = 'group', title = title, font = myFontHead, 
                            xaxis = list(tickangle = -25), showlegend = FALSE)%>% 
      config(displayModeBar = F)
  }
  
  ######### private plot amount starts
  
  ######### commercial plot lifetime starts
  
  # set the plot
  plotLifetime <- plot_ly(alpha = 0.8)
  
  # only generate plot, if there is data for commercial customers
  if(!is.null(aggregatedCommercialData)){
    
    # names of all makes and models
    xLabels <- paste(aggregatedCommercialData$make, aggregatedCommercialData$model)
    
    # title
    title <- "Standzeit Fahrzeuge"
    
    # the title is overwritten, depending if only commercial or only private data was found
    if(marktstudie$markerResults == "onlyCommercial"){title <- "Standzeit kommerzielle Fahrzeuge"}
    if(marktstudie$markerResults == "onlyPrivate"){title <- "Standzeit privater Fahrzeuge"}
    
    # generates the actual plot
    plotLifetime <- add_trace(plotLifetime, x = xLabels, y = aggregatedCommercialData$commercialLifetimeMean, name = "kommerziell",
                               type = "bar", hoverinfo = "y", marker = list(color = colorListBars[1]))%>% layout(barmode = 'group', title = title, 
                              xaxis = list(tickangle = -25), font = myFontHead, showlegend = FALSE)%>% 
      config(displayModeBar = F)
  }
  
  ######### commercial plot lifetime ends
  
  ######### private plot lifetime starts
  
  # only generate plot, if there is data for private customers
  if(!is.null(aggregatedPrivateData)){
    
    # names of all makes and models
    xLabels <- paste(aggregatedPrivateData$make, aggregatedPrivateData$model)
    
    # title
    title <- "Standzeit Fahrzeuge"
    
    # the title is overwritten, depending if only commercial or only private data was found
    if(marktstudie$markerResults == "onlyCommercial"){title <- "Standzeit kommerzielle Fahrzeuge"}
    if(marktstudie$markerResults == "onlyPrivate"){title <- "Standzeit privater Fahrzeuge"}
    
    # generate the actual plot
    plotLifetime <- add_trace(plotLifetime, x = xLabels, y = aggregatedPrivateData$privateLifetimeMean, name ="privat", 
                               type = "bar", hoverinfo = "y", marker = list(color = colorListBars[2]), xaxis = list(tickangle = 45)) %>% layout(barmode = 'group', title = title, font = myFontHead, showlegend = FALSE)%>% 
      config(displayModeBar = F)
  }
  
  ######### private plot lifetime ends

  return(list(plotAmount, plotLifetime))
  
}


###########################################################################
######################### 7.
###########################################################################
#
#           Generates two plots (scatterplot for price mileage and price year) for commercial dealers
#
generateScatterCommercial <- function(dataCommercial){
  
  # only generate plot if data for commercial is found
  if(is.null(dataCommercial)){return(NULL)}
  
  # define headline size
  myFontHead <- list(size = 8)
  myFontAxis <- list(size = 7)
  
  ######### commercial plot mileage start
  
  # set up plot
  plot1Commercial <- plot_ly()
  
  # title
  title <-"Scatterplot Preis KM (kommerziell)"
  
  # for every make in savedMakes, add a trace
  for(i in 1:nrow(marktstudie$savedMakes)){
    make <- marktstudie$savedMakes[i,"Marke"]
    model <- marktstudie$savedMakes[i,"Modell"]
    plot1Commercial <- add_trace(plot1Commercial, x = dataCommercial[dataCommercial$model == model & dataCommercial$make == make,"mileage"], y = dataCommercial[dataCommercial$model == model & dataCommercial$make == make,"price"], 
                                 type = "scatter", mode = "markers", opacity = opacity, 
                                 marker = list(color = colorListLinesMarkers[i]))%>% layout(title = title, font = myFontHead, xaxis = list(title = "Km", font = myFontAxis),
                                                                                                                     yaxis = list(title = "Preis", font = myFontAxis), showlegend = FALSE)%>% 
      config(displayModeBar = F)
  }
  
  ######### commercial plot mileage end
  
  ######### commercial plot year start
  
  # set up the plot
  plot2Commercial <- plot_ly()
  
  # title
  title <-"Scatterplot Preis Baujahr (kommerziell)"
  
  # for every make in savedMakes, add a trace
  for(i in 1:nrow(marktstudie$savedMakes)){
    make <- marktstudie$savedMakes[i,"Marke"]
    model <- marktstudie$savedMakes[i,"Modell"]
    plot2Commercial <- add_trace(plot2Commercial, x = dataCommercial[dataCommercial$model == model & dataCommercial$make == make,"year"], y = dataCommercial[dataCommercial$model == model & dataCommercial$make == make,"price"], 
                              type = "scatter", mode = "markers", opacity = opacity, 
                              marker = list(color = colorListLinesMarkers[i]))%>% layout(title = title, font = myFontHead, xaxis = list(title = "Baujahr", font = myFontAxis),
                                                                                                                  yaxis = list(title = "Preis", font = myFontAxis), showlegend = FALSE)%>% 
      config(displayModeBar = F)
  }
  
  ######### commercial plot year end
  
  return(list(plot1Commercial,plot2Commercial))
}

###########################################################################
######################### 8.
###########################################################################
#
#           Generates two plots (scatterplot for price mileage and price year) for private dealers
#
generateScatterPrivate <- function(dataPrivate){
  
  # only set up the plot if data for private costumers are found
  if(is.null(dataPrivate)){return(NULL)}
  
  # set size of headline text title
  myFontHead <- list(size = 8)
  myFontAxis <- list(size = 7)
  
  ######### private plot mileage start
  
  # set up the plot
  plot1Private <- plot_ly()
  
  # title
  title <-"Scatterplot Preis KM (privat)"
  
  # for every saved make in savedMakes, add a trace
  for(i in 1:nrow(marktstudie$savedMakes)){
    make <- marktstudie$savedMakes[i,"Marke"]
    model <- marktstudie$savedMakes[i,"Modell"]
    plot1Private <- add_trace(plot1Private, x = dataPrivate[dataPrivate$model == model & dataPrivate$make == make,"mileage"], y = dataPrivate[dataPrivate$model == model & dataPrivate$make == make,"price"], 
                              type = "scatter", mode = "markers", opacity = opacity,
                              marker = list(color = colorListLinesMarkers[i]))%>% layout(title = title, font = myFontHead, xaxis = list(title = "Km", font = myFontAxis),
                                                                            yaxis = list(title = "Preis", font = myFontAxis), showlegend = FALSE)%>% 
      config(displayModeBar = F)
  }
  
  ######### private plot mileage end
  
  ######### private plot year start
  
  # set up plot
  plot2Private <- plot_ly()
  
  # title
  title <-"Scatterplot Preis Baujahr (privat)"
  
  # for every make in savedMakes, add trace
  for(i in 1:nrow(marktstudie$savedMakes)){
    make <- marktstudie$savedMakes[i,"Marke"]
    model <- marktstudie$savedMakes[i,"Modell"]
    plot2Private <- add_trace(plot2Private, x = dataPrivate[dataPrivate$model == model & dataPrivate$make == make,"year"], y = dataPrivate[dataPrivate$model == model & dataPrivate$make == make,"price"], 
                              type = "scatter", mode = "markers", opacity = opacity, 
                              marker = list(color = colorListLinesMarkers[i]))%>% layout(title = title, font = myFontHead, xaxis = list(title = "Baujahr", font = myFontAxis),
                                                                                                                  yaxis = list(title = "Preis", font = myFontAxis), showlegend = FALSE)%>% 
      config(displayModeBar = F)
  }
  
  ######### private plot year end
  
  return(list(plot1Private,plot2Private))
}


###########################################################################
######################### 9.
###########################################################################
#
#           Get the longitude and latitude for the full address of dealers
#
getGeoLocations <- function(data){
  
  
  data[is.na(data)]<- ""
  
  # pull out relevant columsn for address search and concatenate. The concatenation is here to cick out multiples (not only based on the name, but also based on street, zip
  # and city, customer_type). Name private would not be neccessary, but it will be carried on here for later use in displaying the name "private" for private customers 
  # shown on the map
  addresses <- paste(name = data[["name"]], data[["street"]], data[["zip"]], data[["city"]], data[["customer_type"]], data[["namePrivate"]], sep = ";;;")
  
  # redundant addresses cicked out
  addresses <- unique(addresses)
  
  # resplit the addresses in different columns and rename columns
  addressesLongLat <- data.frame(str_split_fixed(addresses, ";;;",6), stringsAsFactors = FALSE)
  colnames(addressesLongLat) <- c("name", "street", "zip", "city", "customerType", "namePrivate")
  
  # mayb
  addressesLongLat[["Long"]]<-NA
  addressesLongLat[["Lat"]]<-NA
  
  # for every address, get Long and Lat 
  for(i in 1:nrow(addressesLongLat)){
    
    # address concatenated again for google api
    address <- paste(addressesLongLat[["street"]][i], addressesLongLat[["zip"]][i], addressesLongLat[["city"]][i])
    
    # get actual long lat from google. See small helper function getLongLat
    LatLong <- getLongLat(address)
    
    # save long lat in table
    addressesLongLat[["Lat"]][i] <- LatLong[[1]]
    addressesLongLat[["Long"]][i] <- LatLong[[2]]
  }
  
  
  marktstudie$dataFromQuery <- merge(marktstudie$dataFromQuery, addressesLongLat[,c("name", "Long", "Lat")], by = "name")
  
  return(addressesLongLat)
}

###########################################################################
######################### 10.
###########################################################################
#
#           Small helper function for function getGeoLocations
#
getLongLat <- function(address){   
  geo_reply = geocode(address, output='latlon', messaging=TRUE, override_limit=TRUE)
  return(list(
    geo_reply$lat,
    geo_reply$lon
  ))
}


###########################################################################
######################### 11.
###########################################################################
#
#           Generates a plot showing dealers on map
#
plotGeoLocations <- function(addressesLongLat){
  
  # creates icons inside of the marker
  iconCommercial <- makeAwesomeIcon(icon = "car", markerColor = "blue", iconColor = "black", library = "fa")
  iconPrivate <- makeAwesomeIcon(icon = "user", markerColor = "grey", iconColor = "black", library = "fa")
  
  # subsample of data for private and commercial
  dataCommercial <- addressesLongLat[addressesLongLat[["customerType"]] == "D", ]
  dataPrivate <- addressesLongLat[addressesLongLat[["customerType"]] == "P", ]
  
  # set up plot
  map <- leaflet() 
  map <- addTiles(map)
  
  #for each commercial customer, add marker
  for(i in 1:nrow(dataCommercial)){
    map <- addAwesomeMarkers(map, layerId = dataCommercial[["name"]][i], lng=dataCommercial$Long[i], lat=dataCommercial$Lat[i],
                      popup = dataCommercial$name[ i], icon = iconCommercial)
  }

  # for each private customer, add marker
  for(i in 1:nrow(dataPrivate)){
    map <- addAwesomeMarkers(map, layerId = dataPrivate[["name"]][i], lng=dataPrivate$Long[i], lat=dataPrivate$Lat[i],
                      popup = dataPrivate$namePrivate[ i], icon = iconPrivate)
  }
  return(map)
}
  

###########################################################################
######################### 12.
###########################################################################
#
#           Transforms label of fuels to shorcuts
#
transformLabelIdFuel <- function(fuel){
    if(fuel == "Benzin"){
      return("B")
    }
    if(fuel == "Diesel"){
      return("D")
    }
    if(fuel == "beliebig"){
      return("beliebig")
    }
}

###########################################################################
######################### 13.
###########################################################################
#
#           Transforms names of color to ids (database needs ids, customer wants to see names)
#
transformLabelIdColour <- function(colour, listColours){
  if(colour == "beliebig"){
    return("beliebig")
  } else {
    return(listColours[listColours$LabelDe == colour,"Id"])
  }
}


###########################################################################
######################### 14.
###########################################################################
#
#           Create the own little legend on top inside box Ueberblick
#
createLegend <- function(makes, models){
  
  # set up legend text
  myLegend <- ""
  
  
  if(marktstudie$markerResults == "all"){
    # first part of the legend, showing kommerzielle und private Haendler
    # the color of the text will be taken out of colorListBars, so that the colors matches the colors of the graphs
    # text here is fixed, stays always the same
    myLegend <- tags$span(style =paste0("color:", colorListBars[1]), HTML("- kommerzielle Anbieter"))
    
    # text here is also fixed, stays always the same. Added a lot of tab spaces for the rest of the legend
    myLegend <- paste0(myLegend, tags$span(style =paste0("color:", colorListBars[2]), 
                                           HTML("&emsp; - private Anbieter &emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;")))
  }
  
  
  # second part of the legend (showing makes and models)
  legendItems <- paste(makes, models)
  
  # each saved make in savedMakes will be displayed in legend. Tabs will be added between the texts, but for the last text, no tab will be added at the end.
  for(i in 1:nrow(marktstudie$savedMakes)){
    if(i < nrow(marktstudie$savedMakes)){
      myLegend <- paste0(myLegend, tags$span(style =paste0("color:", colorListLinesMarkers[i]), 
                                             HTML("- ", legendItems[i], " &emsp;")))
    } else {
      myLegend <- paste0(myLegend, tags$span(style =paste0("color:", colorListLinesMarkers[i]), 
                                             HTML("- ", legendItems[i])))
    }
  }
  myLegend <- tags$div(align = "center", HTML(myLegend))
  return(myLegend)
}


###########################################################################
######################### 15.
###########################################################################
#
#           Renames shortcusts for fuel and usagestat and color again back to full names
#
renameIDToNameFuelUsageColor <- function(data){
  
  if(!is.null(data)){
    
    # fuel
    data$fuel[data$fuel == "B"] <- "Benzin"
    data$fuel[data$fuel == "D"] <- "Diesel"
    data$fuel[data$fuel == "L"] <- "Gas"
    
    # usage state
    data$usagestate[data$usagestate == "U"] <- "gebraucht"
    data$usagestate[data$usagestate == "N"] <- "neu"
    
    # color
    for(i in 1:nrow(data)){
      data[["colour"]][i] <- as.character(colourLabelID[colourLabelID$Id == as.integer(data[["colour"]][i]),"LabelDe"])
    }
  }
  
  return(data)
  
}

