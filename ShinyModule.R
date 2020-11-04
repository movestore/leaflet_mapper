library(move)
library(shiny)
library(sp)
library(mapview)
library(leaflet)
library(leaflet.extras)

#test if i can write
# setwd("/root/app/")

shinyModuleUserInterface <- function(id, label) {
  ns <- NS(id)
  
  tagList(
    titlePanel("Basic leaflet map"),
    leafletOutput(ns("leafmap")),
    downloadButton(ns('savePlot'), 'Save Plot')
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
  
  ids <- namesIndiv(data)
  col <- rainbow(n=length(ids))
  data_spl <- move::split(data)
  
  mmap <- reactive({
    bounds <- as.vector(bbox(extent(dataObj())))
    cols <- colorFactor(gnuplot(), domain=namesIndiv(dataObj()))
    outl <- leaflet() %>% 
      fitBounds(bounds[1], bounds[2], bounds[3], bounds[4]) %>% 
      addTiles() %>%
      addProviderTiles("Esri.WorldTopoMap",group = "TopoMap") %>%
      addProviderTiles("Esri.WorldImagery", group = "Aerial")
      
    for (i in seq(along=ids))
    {
      outl <- outl %>%
        addPolylines(data = coordinates(data_spl[[i]]), color = col[i], group = ids[i], weight=3,opacity=0.3) %>%
        addCircles(data = data_spl[[i]], fillOpacity = 0.5, opacity = 0.7, color = col[i], group = ids[i])
    }

    outl <- outl %>%
      addLegend(position= "topright", colors=col, 
                labels=ids ,opacity = 0.7, title = "Animals") %>%
      addScaleBar(position="topleft", 
                  options=scaleBarOptions(maxWidth = 100, metric = TRUE, imperial = FALSE, updateWhenIdle = TRUE)) %>%
      addLayersControl(
        baseGroups = c("TopoMap", "Aerial"),
        overlayGroups = ids,
        options = layersControlOptions(collapsed = FALSE)
      )
    outl   
    })

  output$leafmap <- renderLeaflet({
    mmap()  
  })  
  
  ### save map, takes some seconds ###
  output$savePlot <- downloadHandler(
    filename = function() {
      paste("SimplePlot.png", sep="")
    },
    content = function(file) {
      mymap <- mmap()
      mapshot( x =mymap
               , remove_controls = "zoomControl"
               , file = file
               , cliprect = "viewport"
               , selfcontained = FALSE)
    }
  )
  
  
  return(reactive({ current() }))
}
