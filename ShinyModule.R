library(move)
library(shiny)
library(raster)
library(foreach)
library(sf)
library(fasterize)
library(rgeos)
library(leaflet)

# setwd("/root/app/")

shinyModuleUserInterface <- function(id, label) {
  ns <- NS(id)
  
  tagList(
    titlePanel("Basic leaflet map"),
    leafletOutput(ns("leafmap"))
  )
}

shinyModuleConfiguration <- function(id, input) {
  ns <- NS(id)
  configuration <- list()
  
  configuration
}

shinyModule <- function(input, output, session, data) {
  dataObj <- reactive({ data })
  current <- reactiveVal(data)
  
  output$leafmap <- renderLeaflet({
    bounds <- as.vector(bbox(extent(dataObj())))
    outl <- leaflet() %>% 
      fitBounds(bounds[1], bounds[2], bounds[3], bounds[4]) %>% 
      addTiles() %>%
      addProviderTiles("Esri.WorldStreetMap",group = "StreetMap") %>%
      addProviderTiles("Esri.WorldImagery", group = "Aerial") %>%
      addProviderTiles("Stamen.Terrain", group = "Terrain") %>%
      addPolylines(data =  as(dataObj(),'SpatialLines'), color ="grey", group = "lines") %>%
      addCircles(data = dataObj(), fillOpacity = 0.3, opacity = 0.5, color="blue", group = "points") %>%
      addLegend(position= "topright", colors=c("grey","blue"), 
                labels=c("lines","points") ,opacity = 0.7, title = "data") %>%
      addScaleBar(position="bottomleft", 
                  options=scaleBarOptions(maxWidth = 100, 
                                          metric = TRUE, imperial = F, updateWhenIdle = TRUE)) %>%
      addLayersControl(
        baseGroups = c("StreetMap", "Aerial", "Terrain"),
        overlayGroups = c("lines", "points"),
        options = layersControlOptions(collapsed = FALSE)
      )
        
    outl    
  })  
  
  return(reactive({ current() }))
}
