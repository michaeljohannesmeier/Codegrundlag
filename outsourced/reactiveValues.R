#########################  Description and overview
#   
#
#
#
marktstudie <- reactiveValues(
  
  # reactive values for tables
  savedMakes = data.frame(Marke = vector(), Modell=vector(), Kraftstoff = vector(), Km_min=vector(), Km_max=vector(), Baujahr_min=vector(), Baujahr_max=vector(),
                          Preis_min = vector(), Preis_max = vector(), PS_min = vector(), PS_max = vector(), Farbe = vector(), Anbieter = vector()),
  dataFromQuery = NULL,
  aggregatedCommercialTable = data.frame(),
  aggregatedPrivateTable = data.frame(),
  dataPrivate = NULL, 
  dataCommercial = NULL,
  
  # reactive values for plots
  plotHistCommercial = NULL,
  plotHistPrivate = NULL,
  plotHistAmount= NULL,
  plotHistLifetime = NULL,
  geoLocationsPlots = NULL,
  scatter1x = "mileage",
  scatter1y = "price",
  scatter2x = "year",
  scatter2y = "price",
  plotScatter1Commercial = NULL,
  plotScatter1Private = NULL,
  plotScatter2Commercial = NULL,
  plotScatter2Private = NULL,
  analysisPlot = NULL,
  
  # reactive values for booleans
  boolSearchCollapsed = FALSE,
  boolSearchFinished = FALSE,
  boolScatter = FALSE,
  boolNoResults = FALSE,
  
  # reactive values for inputs and others
  radius = NULL,
  choosenYearFrom = "beliebig",
  choosenYearTo = "beliebig",
  choosenPowerFrom = "beliebig",
  choosenPowerTo = "beliebig",
  choosenColour = "beliebig",
  choosenMileageFrom = "beliebig",
  choosenMileageTo = "beliebig",
  choosenPriceFrom = "beliebig",
  choosenPriceTo = "beliebig",
  choicesScatters = c("KM" = "mileage", "Preis" = "price", "PS" = "power"),
  choosenCustomerType = "beliebig",
  markerResults = "all",
  errorMessage = "",
  myLegend = NULL,
  enteredZip = NULL
)