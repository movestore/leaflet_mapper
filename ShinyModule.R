library(move)
library(shiny)
library(sp)
library(mapview)
library(pals)
library(leaflet)
library(leaflet.extras)
library(htmlwidgets)

#test if i can write
# setwd("/root/app/")

shinyModuleUserInterface <- function(id, label) {
  ns <- NS(id)
  
  tagList(
    titlePanel("Basic interactive Map using leaflet"),
    leafletOutput(ns("leafmap"),height="85vh"),
    downloadButton(ns('savePlot'), 'Save as Html')
  )
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
    outl <- leaflet(options=leafletOptions(minZoom=2)) %>% 
      fitBounds(bounds[1], bounds[2], bounds[3], bounds[4]) %>%       
      
      addTiles() %>%
      addProviderTiles("Esri.WorldTopoMap",group = "TopoMap") %>%
      addProviderTiles("Esri.WorldImagery", group = "Aerial")

    for (i in seq(along=ids))
    {
      outl <- outl %>%
        addCircleMarkers(data = data_spl[[i]], popup=data_spl[[i]]$timestamp, fillOpacity = 0.5, opacity = 0.7, radius=1, color = col[i],, group = paste("Points", ids[i]))  %>%
      addPolylines(data = coordinates(data_spl[[i]]), color = col[i], weight=3,opacity=0.3, group = paste("Lines",ids[i]))
    }

    outl <- outl %>%
      addLegend(position= "topright", colors=col, 
                labels=ids ,opacity = 0.7, title = "Tracks") %>%
      addScaleBar(position="topleft", 
                  options=scaleBarOptions(maxWidth = 100, metric = TRUE, imperial = FALSE, updateWhenIdle = TRUE)) %>%
      addLayersControl(
        baseGroups = c("TopoMap", "Aerial"),
        overlayGroups = c(paste("Points",ids),paste("Lines",ids)),
        options = layersControlOptions(collapsed = FALSE)
      )
    outl   
    })

  output$leafmap <- renderLeaflet({
    mmap()  
  })  
  
  ### save map, takes some seconds ###
  output$savePlot <- downloadHandler(
    filename = "LeafletMap.html",
    content = function(file) {
      saveWidget(
        widget = mmap(),
        file=file
      )
      #mymap <- mmap()
      #mapshot( x =mymap
      #         , remove_controls = "zoomControl"
      #         , file = file
      #         , cliprect = "viewport"
      #         , selfcontained = FALSE)
    }
  )
  
  
  return(reactive({ current() }))
}


