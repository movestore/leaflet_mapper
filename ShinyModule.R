library("shiny")
library("move2")
library("sf")
library("mapview")
library("pals")
library("leaflet")
library("leaflet.extras")
library("htmlwidgets")

# to display messages to the user in the log file of the App in MoveApps
# one can use the function from the src/common/logger.R file:
# logger.fatal(), logger.error(), logger.warn(), logger.info(), logger.debug(), logger.trace()

shinyModuleUserInterface <- function(id, label) {
  # all IDs of UI functions need to be wrapped in ns()
  ns <- NS(id)

  tagList(
    titlePanel("Basic interactive Map using leaflet"),
    leafletOutput(ns("leafmap"),height="85vh"),
    downloadButton(ns('savePlot'), 'Save as Html')
  )
}


# The parameter "data" is reserved for the data object passed on from the previous app
shinyModule <- function(input, output, session, data) {
  # all IDs of UI functions need to be wrapped in ns()
  ns <- session$ns
  dataObj <- reactive({ data })
  current <- reactiveVal(data)
  
  ids <- unique(mt_track_id(data))
  col <- rainbow(n=length(ids))
  data_spl <- split(data,mt_track_id(data))
  
  mmap <- reactive({
    bounds <- as.vector(st_bbox(dataObj()))
    cols <- colorFactor(gnuplot(), domain=unique(mt_track_id(dataObj())))
    outl <- leaflet(options=leafletOptions(minZoom=2)) %>% 
      fitBounds(bounds[1], bounds[2], bounds[3], bounds[4]) %>%       
      
      addTiles() %>%
      addProviderTiles("Esri.WorldTopoMap",group = "TopoMap") %>%
      addProviderTiles("Esri.WorldImagery", group = "Aerial")
    
    for (i in seq(along=ids))
    {
      outl <- outl %>%
        addCircleMarkers(data = data_spl[[i]], popup=mt_time(data_spl[[i]]), fillOpacity = 0.5, opacity = 0.7, radius=1, color = col[i], group = paste("Points", ids[i]))  %>%
        addPolylines(data = st_coordinates(data_spl[[i]]), color = col[i], weight=3, opacity=0.3, group = paste("Lines",ids[i]))
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
