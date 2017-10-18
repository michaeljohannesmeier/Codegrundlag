

#########################  Description and overview
#   
#   #### all outpus for search  
#   1.                        UI                  completeUIOutput (search box)
#     1.1                     UI                  chooseMakeUi
#     1.2                     UI                  chooseModelUi
#     1.2 (eingefuegt)        UI                  selectZipUI
#     1.3                     UI                  detailSearch
#     1.4                     UI                  buttonAddToSavedMakes
#     1.5                     datatable           tableChoosenMakes
#     1.6                     UI                  buttonRemoveFromSavedMakes
#     1.7                     UI                  errorMessages
#     1.8                     UI                  resultsUI
#
#
#   #### all outputs for resuls (Ueberblick)
#       1.8.1                 datatable           tableNoResults
#       1.8.2                 UI                  myLegend
#       1.8.3                 plotly              histAmount 
#       1.8.4                 plotly              histLifetime
#       1.8.5                 plotly              histCommercial
#       1.8.6                 plotly              histPrivate
#       1.8.7                 plotly              scatter1Commercial
#       1.8.8                 plotly              scatter1Private
#       1.8.9                 plotly              scatter2Commercial
#       1.8.10                plotly              scatter2Private
#       1.8.11                UI                  tabPanelUI
#
#   #### all outputs for resuls (Detail)
#         1.8.11.1            UI                  uiTableCommercial
#         1.8.11.1.1          UI                  aggregatedCommercial
#         1.8.11.1.2          datatable           detailTableCommercial
#         1.8.11.2            UI                  uiTablePrivate
#         1.8.11.2.1          datatable           aggregatedPrivate
#         1.8.11.2.2          datatable           detailTablePrivate
#         1.8.11.3            leaflet             geoLocations
#         1.8.11.4            datatable           geoTable
#         1.8.11.5            UI                  analysisUISide
#         1.8.11.5.1          UI                  detailsAnalysisPlot
#         1.8.11.5.1.1        UI                  selectorChooseColor
#         1.8.11.6            plotly              analysisMainPlot
#
#
#
#
#
#
#



###########################################################################
######################### 1.
###########################################################################
#
output$completeUIOutput <- renderUI({
  
  return(list(
    div(class = "busy",  
        p("Marktstudie wird von Datenbank geladen..."),
        img(src="loading_icon.gif", width = "70%")
    ),
    tagList(
      tags$head(
        tags$link(rel="stylesheet", type="text/css",href="style.css"),
        tags$script(type="text/javascript", src = "busy.js")
      )
    ),
    
    box(
      title = "Suche",
      width= 14, 
      collapsible = TRUE,
      collapsed = marktstudie$boolSearchCollapsed,
      solidHeader = FALSE,
      fluidRow(
        column(
          width = 9,
          fluidRow(
            column(
              width=2,
              uiOutput("chooseMakeUi"),
              checkboxInput("showDetailBool", "Detailsuche", value = FALSE)
            ),
            column(
              width=2,
              uiOutput("chooseModelUi")
              
            ),
            column(
              width=2,
              selectInput("choosenFuel", "Kraftstoff", choices = c("beliebig", "Benzin","Diesel"))
              
            ),
            column(
              width=4,
              uiOutput("selectZipUI")
            ),
            column(
              width=2, 
              selectInput("selectRadius", "Umkreis (km)", seq(10,50, 10), selected= 10)
            )
          ),
          uiOutput("detailSearch"),
          actionButton("addMakeToSaved", "Hinzuf\u00fcgen"),
          br(),
          br(),
          uiOutput("buttonAddToSavedMakes")
          
        ),
        column(
          width = 3, 
          DT::dataTableOutput("tableChoosenMakes"),
          uiOutput("buttonRemoveFromSavedMakes")
        )
      ),
      tags$div(align = "center", uiOutput("errorMessages"))
    ),
    uiOutput("resultsUI")
  ))
  
})


###########################################################################
######################### 1.1
###########################################################################
#
output$chooseMakeUi <- renderUI({
  selectizeInput("choosenMake", "Marke", choices = as.character(makeCSV[["make_name"]]),
                 options = list(placeholder = 'Marke', onInitialize = I('function() { this.setValue(""); }')))
  
})

###########################################################################
######################### 1.2
###########################################################################
#
output$chooseModelUi <- renderUI({
  
  if(is.null(input$choosenMake)){return()}
  if(input$choosenMake == ""){
    return(selectizeInput("choosenModel", choices = "", label = "Modell",
                          options = list(placeholder = 'Modell', onInitialize = I('function() { this.setValue(""); }'))))
  } else {
    choosenIDMake <- makeCSV[makeCSV["make_name"]==input$choosenMake, "make_id"]
    return(selectInput("choosenModel", choices = as.character(modelCSV[modelCSV[,"make_id"]== choosenIDMake, "model_name"]), label = "Model"))
  }
})

###########################################################################
######################### 1. (eingefuegt)
###########################################################################
#
output$selectZipUI <- renderUI({
  if(!marktstudie$boolSearchFinished){
    print("inside also")
    
    updateSelectizeInput(session = session, inputId = 'selectZip', choices = zipCodesSearch, server = TRUE)
    return(selectizeInput("selectZip", label = "PLZ/Ort",
                   choices = NULL,
                   options = list(placeholder = 'PLZ/Ort',
                                  onInitialize = I('function() { this.setValue(""); }'))))
  } else {
    return(selectInput("selectZip", label = "PLZ/Ort", choices = zipCodesSearch, selected = marktstudie$enteredZip))
  }
})


###########################################################################
######################### 1.3
###########################################################################
#
output$detailSearch <- renderUI({
  if(input$showDetailBool == TRUE){
    return(list(
      fluidRow(
        column(
          width = 2,
          selectInput("choosenMileageFrom", "Kilometer von:", choices = c("beliebig", seq(20000,300000, 20000))),
          selectInput("choosenMileageTo", "Kilometer bis:", choices = c("beliebig", seq(20000,300000, 20000)))
        ),
        column(
          width = 2,
          selectInput("choosenYearFrom", "Baujahr von:", choices = c("beliebig", 2017:1950)),
          selectInput("choosenYearTo", "Baujahr bis:", choices = c("beliebig", 2017:1950))
        ),
        column(
          width = 2,
          selectInput("choosenPriceFrom", "Preis von:", choices = c("beliebig", seq(10000,100000, 10000))),
          selectInput("choosenPriceTo", "Preis bis:", choices = c("beliebig", seq(10000,100000, 10000)))
        ),
        column(
          width = 2,
          selectInput("choosenPowerFrom", "PS von:", choices = c("beliebig", seq(10,400,10))),
          selectInput("choosenPowerTo", "PS bis:", choices = c("beliebig", seq(10,400,10)))
        ),
        column(
          width = 2,
          selectInput("choosenColour", "Farbe:", choices = c("beliebig", as.character(colourLabelID$LabelDe))),
          selectInput("choosenCustomerType", "Anbieter", choices = c("beliebig", "kommerziell", "privat"))
        )
      )
    ))
  }
})

###########################################################################
######################### 1.4
###########################################################################
#
output$buttonAddToSavedMakes <- renderUI({
  if(nrow(marktstudie$savedMakes) == 0){return()}
  tags$button(id = "getQuery", type = "button", class = "btn action-button btn-large btn-primary", HTML('Report Anzeigen'))
  #submitButton("getQuery", "Report anzeigen")
  #tags$div(HTML('<button >Report Anzeigen</button>'))
})

###########################################################################
######################### 1.5
###########################################################################
#
output$tableChoosenMakes <- DT::renderDataTable({
  
  if(nrow(marktstudie$savedMakes) == 0){return()}
  
  datatable(marktstudie$savedMakes, rownames = NULL, extensions = "Buttons", option = list(lengthChange = FALSE, scrollX = TRUE, searching = FALSE, paging = FALSE,
                                                                                           buttons = I('colvis'), dom = 'Brtip', columnDefs = list(list(targets = c(2:12), visible = FALSE))))
})

###########################################################################
######################### 1.6
###########################################################################
#
output$buttonRemoveFromSavedMakes <- renderUI({
  if(nrow(marktstudie$savedMakes) == 0){return()}
  actionButton("deleteMakeFromSave", "L\u00f6schen")
})

###########################################################################
######################### 1.7
###########################################################################
#
output$errorMessages <- renderUI({
  if(marktstudie$errorMessage==""){return()}
  HTML("<div style='color: red'>", marktstudie$errorMessage, "</div")
})


###########################################################################
######################### 1.8.
###########################################################################
#
output$resultsUI <- renderUI({
  
  print(marktstudie$markerResults)
  if(!marktstudie$boolSearchFinished){return()}
  if(marktstudie$boolNoResults){
    return(list(
      tags$div(align = "center", 
               h3(paste0("Keine Eintr\u00e4ge in der Datenbank gefunden im Umkreis von ", input$selectRadius, " km von ")),
               h3(paste0(isolate(input$selectZip))),
               br(),
               h3(paste0("Bitte spezifizieren Sie Ihre Eingabe")),
               br(),
               br(),
               br(),
               br(),
               h4("Gesucht wurde nach Fahrzeugen mit folgenden Kriterien:"),
               fluidRow(
                 column(1),
                 column(10, dataTableOutput("tableNoResults"))
               )
      )
    ))
  }
  
  
  if(marktstudie$markerResults == "all"){
    return(list(
      box(
        width = 13,
        title = "Marktstudie \u00dcberblick",
        collapsible = TRUE,
        uiOutput("myLegend"),
        br(),
        fluidRow(
          column(
            width = 3,
            plotlyOutput("histAmount", width = "100%", height = "200px"),
            br(),
            plotlyOutput("histLifetime", width = "100%", height = "200px")
          ),
          column(
            width = 3,
            plotlyOutput("histCommercial", width = "100%", height = "200px"),
            br(),
            plotlyOutput("histPrivate", width = "100%", height = "200px")
          ),
          column(
            width = 3, 
            plotlyOutput("scatter1Commercial", width = "100%", height = "200px"),
            br(),
            plotlyOutput("scatter1Private", width = "100%", height = "200px")
          ),
          column(
            width = 3, 
            plotlyOutput("scatter2Commercial", width = "100%", height = "200px"),
            br(),
            plotlyOutput("scatter2Private", width = "100%", height = "200px")
          )
        )
      ),
      box(
        width = 13,
        title = "Marktstudie Details",
        collapsible = TRUE,
        uiOutput("tabPanelUI")
      )
    ))
  }
  if(marktstudie$markerResults == "onlyCommercial"){
    return(list(
      box(
        width = 13,
        title = "Marktstudie \u00dcberblick",
        collapsible = TRUE,
        uiOutput("myLegend"),
        br(),
        fluidRow( 
          column(
            width = 6,
            plotlyOutput("histCommercial", width = "100%", height = "400px")
          ),
          column(
            width = 3,
            plotlyOutput("histAmount", width = "100%", height = "200px"),
            br(),
            plotlyOutput("histLifetime", width = "100%", height = "200px")
          ),
          column(
            width = 3, 
            plotlyOutput("scatter1Commercial", width = "100%", height = "200px"),
            br(),
            plotlyOutput("scatter2Commercial", width = "100%", height = "200px")
          )
        )
      ),
      box(
        width = 13,
        title = "Marktstudie Details",
        collapsible = TRUE,
        uiOutput("tabPanelUI")
      )
    ))
  }
  if(marktstudie$markerResults == "onlyPrivate"){
    return(list(
      box(
        width = 13,
        title = "Marktstudie \u00dcberblick",
        collapsible = TRUE,
        uiOutput("myLegend"),
        br(),
        fluidRow( 
          column(
            width = 6,
            plotlyOutput("histPrivate", width = "100%", height = "400px")
          ),
          column(
            width = 3,
            plotlyOutput("histAmount", width = "100%", height = "200px"),
            br(),
            plotlyOutput("histLifetime", width = "100%", height = "200px")
          ),
          column(
            width = 3, 
            plotlyOutput("scatter1Private", width = "100%", height = "200px"),
            br(),
            plotlyOutput("scatter2Private", width = "100%", height = "200px")
          )
        )
      ),
      box(
        width = 13,
        title = "Marktstudie Details",
        collapsible = TRUE,
        uiOutput("tabPanelUI")
      )
    ))
  }
  
  
})

###########################################################################
######################### 1.8.1
###########################################################################
#
output$tableNoResults <- renderDataTable({
  
  datatable(marktstudie$savedMakes,rownames = NULL, 
            colnames = c("Marke", "Modell", "Kraftstoff", "Km min", "Km max", "Baujahr min", "Baujahr max","Preis min" ,"Preis max", "PS min", "PS max", "Farbe"),
            selection="single", option = list(lengthChange = FALSE, scrollX = TRUE, searching = FALSE, paging = FALSE))
  
})

###########################################################################
######################### 1.8.2
###########################################################################
#
output$myLegend <- renderUI({
  
  return(list(
    isolate(p(align = "center", paste("Ergebnisse fuer", marktstudie$savedMakes[["Marke"]], marktstudie$savedMakes[["Modell"]],
                                      "im Umkreis von ", input$selectRadius, " km von ",marktstudie$enteredZip))),
    marktstudie$myLegend, 
    br()
  ))
})


###########################################################################
######################### 1.8.3
###########################################################################
#
output$histAmount <- renderPlotly({
  if(is.null(marktstudie$plotHistAmount)){return()}
  marktstudie$plotHistAmount
})

###########################################################################
######################### 1.8.4
###########################################################################
#
output$histLifetime <- renderPlotly({
  if(is.null(marktstudie$plotHistLifetime)){return()}
  marktstudie$plotHistLifetime
})

###########################################################################
######################### 1.8.5
###########################################################################
#
output$histCommercial <- renderPlotly({
  if(is.null(marktstudie$plotHistCommercial)){return()}
  if(length(marktstudie$plotHistCommercial$x$attrs)==1){return()}
  marktstudie$plotHistCommercial
  
})

###########################################################################
######################### 1.8.6
###########################################################################
#
output$histPrivate <- renderPlotly({
  if(is.null(marktstudie$plotHistPrivate)){return()}
  if(length(marktstudie$plotHistPrivate$x$attrs)==1){return()}
  marktstudie$plotHistPrivate
  
})


###########################################################################
######################### 1.8.7
###########################################################################
#
output$scatter1Commercial <- renderPlotly({
  if(is.null(marktstudie$plotScatter1Commercial)){return()}
  marktstudie$plotScatter1Commercial
  
})

###########################################################################
######################### 1.8.8
###########################################################################
#
output$scatter1Private <- renderPlotly({
  if(is.null(marktstudie$plotScatter1Private)){return()}
  marktstudie$plotScatter1Private
  
})

###########################################################################
######################### 1.8.9
###########################################################################
#
output$scatter2Commercial <- renderPlotly({
  if(is.null(marktstudie$plotScatter2Commercial)){return()}
  marktstudie$plotScatter2Commercial
  
})

###########################################################################
######################### 1.8.10
###########################################################################
#
output$scatter2Private <- renderPlotly({
  if(is.null(marktstudie$plotScatter2Private)){return()}
  marktstudie$plotScatter2Private
  
})


###########################################################################
######################### 1.8.11
###########################################################################
#
output$tabPanelUI <- renderUI({
  
  if(!marktstudie$boolSearchFinished){return()}
  return(list(
    tabsetPanel(
      tabPanel("Kommerzielle",uiOutput("uiTableCommercial")),
      tabPanel("Private", uiOutput("uiTablePrivate")),
      tabPanel("Karte", 
               column(5,br(), br(),leafletOutput("geoLocations")),
               column(7,br(), br(),dataTableOutput("geoTable"))
      ),
      tabPanel("Analyse", 
               fluidRow(
                 column(3, selectInput("chooseAllCommercialPrivate", "Anbieter", choices = c("alle", "kommerzielle", "private"))),
                 column(3, selectInput("chooseXAxis", "x Achse", choices = c("Preis" = "price", "Km" = "mileage", "Baujahr" = "year", "PS" = "power"), selected = "mileage")),
                 column(3, selectInput("chooseYAxis", "y Achse", choices = c("Preis" = "price", "Km" = "mileage", "Baujahr" = "year", "PS" = "power", "H\u00e4ufigkeit" = "amount"), selected = "price")),
                 column(3, selectInput("chooseMakeModel", "Fahrzeug", choices = c("alle", paste(marktstudie$savedMakes[["Marke"]], marktstudie$savedMakes[["Modell"]]))))
               ),
               fluidRow(
                 column(
                   width = 3,
                    selectInput("chooseFuel", "Kraftstoff", choices = c("alle", "Benzin", "Diesel")),
                    uiOutput("selectorChooseColor"),
                    selectInput("chooseUsageState", "Zustand", choices = c("alle", "gebraucht", "neu")),
                    sliderInput("chooseAnalysisMileage", "Kilometer", min = min(na.omit(marktstudie$dataFromQuery[["mileage"]])), 
                                max = max(na.omit(marktstudie$dataFromQuery[["mileage"]])), value = c(min(na.omit(marktstudie$dataFromQuery[["mileage"]])),
                                                                                             max(na.omit(marktstudie$dataFromQuery[["mileage"]])))),
                    sliderInput("chooseAnalysisYear", "Baujahr", format="####", sep="", min = min(na.omit(marktstudie$dataFromQuery[["year"]])), step = 1, 
                                max = max(na.omit(marktstudie$dataFromQuery[["year"]])), value = c(min(na.omit(marktstudie$dataFromQuery[["year"]])),
                                                                                                   max(na.omit(marktstudie$dataFromQuery[["year"]])))),
                    sliderInput("chooseRadius", "Radius", min = 1, max = as.integer(input$selectRadius), step = 1, value = as.integer(input$selectRadius))
                  ),
                  column(
                    width = 9, 
                    br(),
                    br(),
                    plotlyOutput("analysisMainPlot")
                  )
               )
      )
    )
  ))
})

###########################################################################
######################### 1.8.11.1
###########################################################################
#
output$uiTableCommercial <- renderUI({
  if(marktstudie$markerResults == "all" | marktstudie$markerResults == "onlyCommercial"){
    return(list(
      DT::dataTableOutput("aggregatedCommercial"),
      br(),
      DT::dataTableOutput("detailTableCommercial")
    ))
  }
  if(marktstudie$markerResults == "onlyPrivate"){
    return(list(
      br(), br(), tags$div(align = "center", h4("Die Suche lieferte keine Ergebnisse f\u00fcr kommerzielle Fahrzeuge"))
    ))
  }
})

###########################################################################
######################### 1.8.11.1.1
###########################################################################
#
output$aggregatedCommercial <- DT::renderDataTable({
  
  datatable(marktstudie$aggregatedCommercialTable,rownames = NULL, 
            colnames = c("Marke", "Modell", "Anzahl", "Preis Durchschnitt", "Preis min" ,"Preis max", "Standzeit Durchschnitt"), selection="single", 
            option = list(lengthChange = FALSE, scrollX = TRUE, searching = FALSE, paging = FALSE))
})

###########################################################################
######################### 1.8.11.1.2
###########################################################################
#
output$detailTableCommercial <- DT::renderDataTable({
  
  rowsSelected <- input$aggregatedCommercial_rows_selected
  if(length(rowsSelected) == 0){return()}
  
  choosenMakeModel <- c(marktstudie$aggregatedCommercialTable[rowsSelected, "make"], marktstudie$aggregatedCommercialTable[rowsSelected, "model"])
  table <- marktstudie$dataCommercial[marktstudie$dataCommercial[["make"]]== choosenMakeModel[1] & marktstudie$dataCommercial[["model"]]== choosenMakeModel[2], ]
  datatable(table[,c(1,3,4,5,6,7,8,15,12,13,14,17,19)],rownames = NULL, extensions = "Buttons", 
            colnames = c("Name", "PLZ", "Ort", "Strasse", "Marke" ,"Modell", "Preis","Km", "PS","Baujahr", "Kraftstoff", "Zustand", "Farbe"), 
            option = list(lengthChange = TRUE, scrollX = TRUE, searching = FALSE, paging = FALSE, scrollY = "400px", 
                          buttons = I('colvis'), dom = 'Brtip', columnDefs = list(list(targets = c(1,2,3), visible = FALSE))))
  
})



###########################################################################
######################### 1.8.11.2
###########################################################################
#
output$uiTablePrivate <- renderUI({
  if(marktstudie$markerResults == "all" | marktstudie$markerResults == "onlyPrivate"){
    return(list(
      DT::dataTableOutput("aggregatedPrivate"),
      br(),
      DT::dataTableOutput("detailTablePrivate")
    ))
  }
  if(marktstudie$markerResults == "onlyCommercial"){
    return(list(
      br(), br(), tags$div(align = "center", h4("Die Suche lieferte keine Ergebnisse f\u00fcr private Fahrzeuge"))
    ))
  }
  
})

###########################################################################
######################### 1.8.11.2.1
###########################################################################
#
output$aggregatedPrivate <- DT::renderDataTable({
  
  datatable(marktstudie$aggregatedPrivateTable,rownames = NULL, 
            colnames = c("Marke", "Modell", "Anzahl", "Preis Durchschnitt", "Preis min" ,"Preis max", "Standzeit Durchschnitt"), selection="single", 
            option = list(lengthChange = FALSE, scrollX = TRUE, searching = FALSE, paging = FALSE))
})

###########################################################################
######################### 1.8.11.2.2
###########################################################################
#
output$detailTablePrivate <- DT::renderDataTable({
  
  rowsSelected <- input$aggregatedPrivate_rows_selected
  if(length(rowsSelected) == 0){return()}
  
  choosenMakeModel <- c(marktstudie$aggregatedPrivateTable[rowsSelected, "make"], marktstudie$aggregatedPrivateTable[rowsSelected, "model"])
  table <- marktstudie$dataPrivate[marktstudie$dataPrivate[["make"]]== choosenMakeModel[1] & marktstudie$dataPrivate[["model"]]== choosenMakeModel[2], ]
  datatable(table[,c(20,3,5,6,7,8,15,12,13,14,17,19)],rownames = NULL, extensions = "Buttons", 
            colnames = c("Name", "PLZ", "Ort", "Marke" ,"Modell", "Preis","Km", "PS","Baujahr", "Kraftstoff", "Zustand", "Farbe"), 
            option = list(lengthChange = TRUE, scrollX = TRUE, searching = FALSE, paging = FALSE, scrollY = "400px",
                          buttons = I('colvis'), dom = 'Brtip', columnDefs = list(list(targets = c(1,2), visible = FALSE))))
})




###########################################################################
######################### 1.8.11.3
###########################################################################
#
output$geoLocations <- renderLeaflet({
  if(is.null(marktstudie$geoLocationsPlots)){return()}
  marktstudie$geoLocationsPlots
})

###########################################################################
######################### 1.8.11.4
###########################################################################
#
output$geoTable <- renderDataTable({
  
  if(is.null(input$geoLocations_marker_click)){return()}
  
  print(input$geoLocations_marker_click$id)
  print(marktstudie$dataFromQuery[["customer_type"]][marktstudie$dataFromQuery[["name"]] == input$geoLocations_marker_click$id])
  
  #xxxdata[["customer_type"]][as.character(na.omit(xxxdata[["name"]] == xxxinput] == "D"
  
  
  #xxxdata <<- marktstudie$dataFromQuery
  if(marktstudie$dataFromQuery[["customer_type"]][marktstudie$dataFromQuery[["name"]] == input$geoLocations_marker_click$id] == "D"){
    print("worked1")
    return(
      datatable(marktstudie$dataFromQuery[marktstudie$dataFromQuery[["name"]] == input$geoLocations_marker_click$id, c(1,3,4,5,6,7,8,15,12,13,14,17,19)], 
                rownames = NULL, extensions = "Buttons", 
                colnames = c("Name", "PLZ", "Ort", "Strasse", "Marke" ,"Modell", "Preis","Km", "PS","Baujahr", "Kraftstoff", "Zustand", "Farbe"), 
                option = list(lengthChange = TRUE, scrollX = TRUE, searching = FALSE, paging = TRUE, 
                              buttons = I('colvis'), dom = 'Brtip', columnDefs = list(list(targets = c(6:12), visible = FALSE))))
    )
  }
  if(length(marktstudie$dataFromQuery[["customer_type"]][marktstudie$dataFromQuery[["name"]] == input$geoLocations_marker_click$id] == "P") > 0){
    print("worked2")
    return(
      datatable(marktstudie$dataFromQuery[marktstudie$dataFromQuery[["name"]] == input$geoLocations_marker_click$id, c(20,3,5,6,7,8,15,12,13,14,17,19)], 
                rownames = NULL, extensions = "Buttons", 
                colnames = c("Name", "PLZ", "Ort", "Marke" ,"Modell", "Preis","Km", "PS","Baujahr", "Kraftstoff", "Zustand", "Farbe"), 
                option = list(lengthChange = TRUE, scrollX = TRUE, searching = FALSE, paging = TRUE, 
                              buttons = I('colvis'), dom = 'Brtip', columnDefs = list(list(targets = c(5:11), visible = FALSE))))
    )
  }
})



###########################################################################
######################### 1.8.11.5.1.1
###########################################################################
#
output$selectorChooseColor <- renderUI({
  
  if(input$chooseMakeModel == "alle"){
    selectInput("chooseColor", "Farbe", choices = c("alle", as.character(colourLabelID$LabelDe[3:length(colourLabelID$LabelDe)])))
  } else {
    
    if(input$chooseYAxis != "amount"){
      selectInput("chooseColor", "Farbe", choices = c("alle (einfarbig)" = "allOne", "alle (in Fahrzeugfarbe)" = "allEach", as.character(colourLabelID$LabelDe[3:length(colourLabelID$LabelDe)])))
    } else {
      selectInput("chooseColor", "Farbe", choices = c("alle", as.character(colourLabelID$LabelDe[3:length(colourLabelID$LabelDe)])))
    }
  }
})


###########################################################################
######################### 1.8.11.6
###########################################################################
#
output$analysisMainPlot <- renderPlotly({
  if(is.null(input$chooseAllCommercialPrivate) | is.null(input$chooseColor) | is.null(input$chooseUsageState) | is.null(input$chooseFuel)){return()}
  if(input$chooseAllCommercialPrivate == "alle"){
    data <- rbind(marktstudie$dataCommercial, marktstudie$dataPrivate)
  }
  if(input$chooseAllCommercialPrivate == "kommerzielle"){
    data <- marktstudie$dataCommercial
  }
  if(input$chooseAllCommercialPrivate == "private"){
    data <- marktstudie$dataPrivate
  }
  
  print("firstData")
  print(data)
  
  if(input$chooseFuel != "alle"){
    print("fuel")
    data <- data[data[["fuel"]] == input$chooseFuel, ]
  }
  if(input$chooseUsageState != "alle"){
    print("usage")
    data <- data[data[["usagestate"]] == input$chooseUsageState, ]
  }
  if(input$chooseColor != "alle" & input$chooseColor != "allOne" & input$chooseColor != "allEach"){
    print("colour")
    data <- data[data[["colour"]] == input$chooseColor, ]
  }
  
  data <- data[data[["distance"]] <= as.numeric(input$chooseRadius), ]
  
  data <- data[data[["mileage"]] >= input$chooseAnalysisMileage[1] & data[["mileage"]] <= input$chooseAnalysisMileage[2], ]
  
  data <- data[data[["year"]] >= as.numeric(input$chooseAnalysisYear[1]) & data[["year"]] <= as.numeric(input$chooseAnalysisYear[2]), ]
  
  print("here comes the data")
  print(data)
  
  if(input$chooseYAxis == "price"){yLabel <- "Preis"}
  if(input$chooseYAxis == "mileage"){yLabel <- "Km"}
  if(input$chooseYAxis == "year"){yLabel <- "Baujahr"}
  if(input$chooseYAxis == "power"){yLabel <- "PS"}
  if(input$chooseYAxis == "amount"){yLabel <- "H&#228;ufigkeit"}
  
  if(input$chooseXAxis == "price"){xLabel <- "Preis"}
  if(input$chooseXAxis == "mileage"){xLabel <- "Km"}
  if(input$chooseXAxis == "year"){xLabel <- "Baujahr"}
  if(input$chooseXAxis == "power"){xLabel <- "PS"}
  
  
  title = paste("Scatterplot", yLabel, "-", xLabel)
  plotAnalysis <- plot_ly()
  if(input$chooseMakeModel == "alle"){   
    if(input$chooseYAxis != "amount"){
      
      for(i in 1:nrow(marktstudie$savedMakes)){
        make <- marktstudie$savedMakes[i,"Marke"]
        model <- marktstudie$savedMakes[i,"Modell"]
        plotAnalysis <- add_trace(plotAnalysis, x = data[data$model == model & data$make == make, input$chooseXAxis], y = data[data$model == model & data$make == make, input$chooseYAxis],
                                  type = "scatter", mode = "markers", marker = list(color = colorListLinesMarkers[i]),
                                  name = paste(marktstudie$savedMakes[i, "Marke"], marktstudie$savedMakes[i, "Modell"]))%>%
          layout(title = title, xaxis = list(title = xLabel),
                 yaxis = list(title = yLabel))%>%
          config(displayModeBar = F)
      } 
    }else {
      fffdata <<- data
      fffsavedMakes <<- marktstudie$savedMakes
      
      title <- paste(yLabel, "-", xLabel)
      for(i in 1:nrow(marktstudie$savedMakes)){
        # if(length(na.omit(data[[input$chooseXAxis]][data[["make"]] == marktstudie$savedMakes[["Marke"]][i] & data[["model"]] == marktstudie$savedMakes[["Modell"]][i]]))<=1){
        #   next
        # }
        # dataDensity <- density(na.omit(data[[input$chooseXAxis]][data[["make"]] == marktstudie$savedMakes[["Marke"]][i] & data[["model"]] == marktstudie$savedMakes[["Modell"]][i]]))
        # dataDensity <- data.frame(x=dataDensity$x[dataDensity$x>0], y=dataDensity$y[dataDensity$x>0])
        name <- paste(marktstudie$savedMakes[["Marke"]][i], marktstudie$savedMakes[["Modell"]][i])
        
        start <- min(na.omit(data[[input$chooseXAxis]]))
        end <- max(na.omit(data[[input$chooseXAxis]]))
        size <- (max(na.omit(data[[input$chooseXAxis]])) - min(na.omit(data[[input$chooseXAxis]]))) / 10
        
        textFrom <- seq(start, end, size)[-length(seq(start, end, size))]
        textTo <- seq(start, end, size)[-1]
        text <- paste(textFrom, "-", textTo)
        
        plotAnalysis <- add_trace(plotAnalysis, x = data[[input$chooseXAxis]][data[["make"]] == marktstudie$savedMakes[["Marke"]][i] & data[["model"]] == marktstudie$savedMakes[["Modell"]][i]],
                                 type = 'histogram', name = name, marker = list(color = colorListLinesMarkers[i]), opacity = opacity,
                                 xbins = list(start = start, end = end, size = size, autobinx = FALSE)) %>%
          layout(title = title, xaxis = list(title = xLabel), yaxis = list(title = yLabel), showlegend = FALSE, bargap=0.05, barmode = "stack")%>%
          config(displayModeBar = F)
        
        
        # plotAnalysis <- add_trace(plotAnalysis, x = dataDensity$x, y = dataDensity$y, type = 'scatter', mode = 'lines',
        #                           name = name, line = list(color = colorListLinesMarkers[i])) %>%
        #   layout(title = title, xaxis = list(title = xLabel), yaxis = list(title = yLabel))%>%
        #   config(displayModeBar = F)
      }
    }
  }
  if(input$chooseMakeModel != "alle"){
    i <- match(input$chooseMakeModel,paste(marktstudie$savedMakes[["Marke"]], marktstudie$savedMakes[["Modell"]]))
    if(input$chooseYAxis != "amount"){
      
        if(input$chooseColor != "allOne" & input$chooseColor != "allEach"){
          
          title <- paste("Scatterplot", xLabel, "-", yLabel,"f&#252;r", input$chooseMakeModel)
          
          plotAnalysis <- add_trace(plotAnalysis, x = data[[input$chooseXAxis]][paste(data$make, data$model) ==input$chooseMakeModel], y = data[[input$chooseYAxis]][paste(data$make, data$model) == input$chooseMakeModel],
                                    type = "scatter", mode = "markers", marker = list(color = colorListLinesMarkers[i]),
                                    name = paste(marktstudie$savedMakes[i, "Marke"], marktstudie$savedMakes[i, "Modell"]))%>%
            layout(title = title, xaxis = list(title = xLabel),
                   yaxis = list(title = yLabel))%>%
            config(displayModeBar = F)
          
        }
        
        if(input$chooseColor == "allOne"){
          
          title <- paste("Scatterplot", xLabel, "-", yLabel,"f&#252;r", input$chooseMakeModel)
          plotAnalysis <- add_trace(plotAnalysis, x = data[[input$chooseXAxis]][paste(data$make, data$model) ==input$chooseMakeModel], y = data[[input$chooseYAxis]][paste(data$make, data$model) == input$chooseMakeModel],
                                    type = "scatter", mode = "markers", marker = list(color = colorListLinesMarkers[i]),
                                    name = paste(marktstudie$savedMakes[i, "Marke"], marktstudie$savedMakes[i, "Modell"]))%>%
            layout(title = title, xaxis = list(title = xLabel),
                   yaxis = list(title = yLabel))%>%
            config(displayModeBar = F)
          
        }
        
        
        if(input$chooseColor == "allEach"){
          
          dataOld <<- data
          for(i in 1:nrow(data)){
            if(!is.null(data[["colour"]][i])){
              data[["colour"]][i] <- as.character(colourLabelID[colourLabelID$LabelDe == data[["colour"]][i],"LabelEn"])
            }
          }
          dataNew <<- data
          for(i in 1:nrow(data[paste(data$make, data$model) ==input$chooseMakeModel, ])){
            title <- paste("Scatterplot", xLabel, "-", yLabel,"f&#252;r", input$chooseMakeModel)
            color <- data[["colour"]][paste(data$make, data$model) ==input$chooseMakeModel][i]
            print(eval(parse(text = paste0("I('", data[["colour"]][paste(data$make, data$model) ==input$chooseMakeModel][i], "')"))))
            plotAnalysis <- add_trace(plotAnalysis, x = data[[input$chooseXAxis]][paste(data$make, data$model) ==input$chooseMakeModel][i], y = data[[input$chooseYAxis]][paste(data$make, data$model) == input$chooseMakeModel][i],
                                      type = "scatter", mode = "markers", marker = list(color = eval(parse(text = paste0("I('", color, "')")))))%>%
              layout(title = title, xaxis = list(title = xLabel),
                     yaxis = list(title = yLabel), showlegend = FALSE)%>%
              config(displayModeBar = F)
            plotFirst <<- plotAnalysis
          }
        }
    } else {
      if(length(na.omit(data[[input$chooseXAxis]][paste(data$make, data$model) ==input$chooseMakeModel])) > 2){
        # dataDensity <- density(na.omit(data[[input$chooseXAxis]][paste(data$make, data$model) ==input$chooseMakeModel]))
        # dataDensity <- data.frame(x=dataDensity$x[dataDensity$x>0], y=dataDensity$y[dataDensity$x>0])
        name <- input$chooseMakeModel
        title <- paste(yLabel, "-", xLabel,"f&#252;r", input$chooseMakeModel)
        
        start <- min(data[[input$chooseXAxis]])
        end <- max(data[[input$chooseXAxis]])
        size <- (max(data[[input$chooseXAxis]]) - min(data[[input$chooseXAxis]])) / 10
        
        textFrom <- seq(start, end, size)[-length(seq(start, end, size))]
        textTo <- seq(start, end, size)[-1]
        text <- paste(textFrom, "-", textTo)
        
        plotAnalysis <- add_trace(plotAnalysis, x = data[[input$chooseXAxis]][paste(data$make, data$model) ==input$chooseMakeModel],
                                  type = 'histogram', name = name, marker = list(color = colorListLinesMarkers[i]), opacity = opacity,
                                  xbins = list(start = start, end = end, size = size),autobinx = FALSE) %>%
          layout(title = title, xaxis = list(title = xLabel), yaxis = list(title = yLabel), showlegend = FALSE, bargap=0.05, barmode = "stack")%>%
          config(displayModeBar = F)
        
        # plotAnalysis <- add_trace(plotAnalysis, x = dataDensity$x, y = dataDensity$y, type = 'scatter', mode = 'lines',
        #                           name = name, line = list(color = colorListLinesMarkers[i])) %>%
        #   layout(title = title, xaxis = list(title = xLabel), yaxis = list(title = yLabel))%>%
        #   config(displayModeBar = F)
      }
    }
    
  }
  plotAnalysis
  
})


