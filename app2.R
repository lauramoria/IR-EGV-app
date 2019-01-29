# IR EGV

# Settings -----------------------------
# packages en functies laden
rm(list=ls())                               #maakt geheugen leeg voor aanvang
options(repos = c(CRAN = "https://cran.rstudio.com"))
options(rsconnect.max.bundle.size=1000000000)

source("helpers2.R")
library(sp) # zit ook in raster
library(shiny)
library(leaflet)
library(plotly)
library(shinythemes)
library(DT)
library(ggplot2)
library(rgdal)

# other settings
proj4.rd <- sp::CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +towgs84=565.4171,50.3319,465.5524,-0.398957,0.343988,-1.87740,4.0725 +units=m +no_defs")
proj4.google <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs")


# data importeren ----------------------------
# setwd('/Users/jurriaan/Waternet/R-shiny/iregv_app')
# myData <- utils::read.csv("./data/algAlgIonLichtNutriVeld.csv", header = TRUE, na.strings = "-999999", sep=";", dec =".", stringsAsFactors = FALSE)
# myData <- convertDatumFEWS(myData)
# myData$locatie.omschrijving <- as.character(myData$locatie.omschrijving)



# 
# ### kolom voor seizoen toevoegen
# # stap 1 seizoenen toevoegen
# theDatazomer <- myData[(as.numeric(format(myData$datum, "%m")) > 3 & as.numeric(format(myData$datum, "%m")) < 10),]
# theDatawinter <- myData[(as.numeric(format(myData$datum, "%m")) < 4 | as.numeric(format(myData$datum, "%m")) > 9),]
# theDatazomer$seizoen <- "zomer"
# theDatawinter$seizoen <- "winter"
# theData <- base::rbind(theDatazomer, theDatawinter); myData <- NULL; theDatazomer <- NULL; theDatawinter <- NULL
# 
# data.table::as.data.table(theData)
# theData <- theData[theData$fewsparameter %in% c("CL","GELDHD","CA-1"),]
# 
# # opslaan Rdataset voor app --------------------------------
# saveRDS(theData, file= "./data/theData.rds")
theData <- base::readRDS('./data/theData.rds')


###Import LAT framework
# LATframework <- utils::read.csv("./data/coordinates_LAT_framework.csv", header = TRUE, sep = ";")
# saveRDS(LATframework, file= "./data/LATframework.rds")

LATframework <- base::readRDS("./data/LATframework.rds")


#Import reference points
#referencepoints <- utils::read.csv("./data/reference.points.csv", header = TRUE, sep = ";")
# saveRDS(referencepoints, file= "./data/referencepoints.rds")
referencepoints <- base::readRDS('./data/referencepoints.rds')





# app------------------------------

ui <- fluidPage(
  theme = shinytheme("sandstone"),
  headerPanel('IR-EGV diagram'),
  sidebarPanel(
    selectInput(inputId = 'eag', 'Ecologisch analyse gebied', choices = sort(unique(theData$locatie.EAG)), selected = "2550-EAG-1", multiple = TRUE),
    selectInput(inputId = 'locatiecode', 'Locatiecode', choices = sort(unique(theData$locatiecode)), selected = "BOT001", multiple = TRUE),
    selectInput(inputId = 'MeetpuntenSeizoen', 'Metingen uit zomer of winter weergeven', unique(theData$seizoen), 
                selected = "zomer", multiple = TRUE),
    sliderInput(inputId = 'period',label = 'Periode',
                min = min(theData$jaar), max=max(theData$jaar),
                value = c(min(theData$jaar), max(theData$jaar)))
  ),
  mainPanel(
    fluidRow(
           plotly::plotlyOutput("plot1"),
           leaflet::leafletOutput("map1")
    )
    
  )
)


server <- function(input, output, session) {
          
          observeEvent(input$eag, {
            updateSelectInput(session = session, inputId='locatiecode',
                              choices = sort(unique(theData[theData$locatie.EAG == input$eag, "locatiecode"])),
                              selected = "BOT001")
            }, ignoreInit = FALSE, ignoreNULL = TRUE
)

          selectedData <- reactive({
              gebiedData <- theData[theData$locatie.EAG %in% unique(input$eag),]
              gebiedData <- gebiedData[gebiedData$locatiecode %in% unique(input$locatiecode),]
              gebiedData <- gebiedData[gebiedData$seizoen %in% unique(input$MeetpuntenSeizoen),]

              gebiedData <- gebiedData[(gebiedData$jaar > input$period[1]
                               & gebiedData$jaar < input$period[2]),]
              
              if (dim(gebiedData)[1] * dim(gebiedData)[2] == 0){return(NULL)}
              else{return(gebiedData)}
  
            })

            output$plot1 <- plotly::renderPlotly({
              p = createIR_EGV_Graph(selectedData())
              if (is.null(p)){return(NULL)}
              else{
              plotly::ggplotly(p, tooltip = "all")}
              }
            )
            
            output$map1 <- renderLeaflet({
              gebiedData <- selectedData()
              
              if (length(gebiedData) == 0){return(NULL)}
              else{
                
              gebiedData <-
                sp::spTransform(SpatialPointsDataFrame(coords=gebiedData[,c("locatie.x","locatie.y")],
                                                   data=gebiedData, proj4string=proj4.rd),
                            CRSobj=proj4.google)
              gebiedData <- base::as.data.frame(gebiedData)
              pal <- leaflet::colorFactor(palette = rainbow(length(unique(gebiedData$locatiecode))),  domain = gebiedData$locatiecode)

              leaflet::leaflet(gebiedData) %>%
                addCircles(~locatie.x.1, ~locatie.y.1, label = as.character(gebiedData$locatiecode),
                           labelOptions = c(permanent =TRUE),
                           weight = 3, radius=40, color= ~pal(locatiecode), fillOpacity = 0.8) %>%
                
                addTiles()
              }
            })
           }

shiny::shinyApp(ui = ui, server = server)
