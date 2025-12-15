library("shiny")
library("move2")
library("sf")
library("mapview")
library("pals")
library("leaflet")
library("leaflet.extras")
library("htmlwidgets")


shinyModuleUserInterface <- function(id, label) {
  
  ns <- NS(id)
  
  tagList(
    titlePanel("Basic interactive Map using leaflet"),
    sidebarLayout(
      sidebarPanel(width = 3,
                   
        h4("Tracks"),
        checkboxGroupInput(ns("animals"), NULL, choices = NULL),
        fluidRow(
          column(6, actionButton(ns("select_all_animals"), "Select All Animals", class = "btn-sm")),
          column(6, actionButton(ns("unselect_animals"), "Unselect All Animals", class = "btn-sm"))
        ),
        
        
        br(),
        h4("Attributes"),
        selectInput( 
          ns("select_attr"), 
          "Optionally: Select other attributes to show on the point (multiple choices):", 
          choices = NULL,
          multiple = TRUE 
        ), 
        
        # selectInput(ns("attr_1"), NULL, choices = NULL, multiple = TRUE ),
        
        
        br(),
        h4("Download"),
        downloadButton(ns("save_html"),"Download as HTML", class = "btn-sm"),
        # downloadButton(ns("save_png"), "Save Map as PNG", class = "btn-sm"),
        ),
      mainPanel(leafletOutput(ns("leafmap"), height = "85vh") ,width = 9)
    )
  )
}

###################################
shinyModule <- function(input, output, session, data) {
  
  ns <- session$ns
  dataObj <- reactive({ data })
  current <- reactiveVal(data)
  
  
  if (is.null(data) || nrow(data) == 0) {
    message("Input is NULL or has 0 rows — returning NULL.")
    return(NULL)
  }
  
  if (!sf::st_is_longlat(data)) data <- sf::st_transform(data, 4326)
  
  track_col <- mt_track_id_column(data)
  
  
  all_ids <- reactive({
    sort(unique(as.character(data[[track_col]])))
  })
  
  observeEvent(all_ids(), {
    updateCheckboxGroupInput(session, "animals",
                             choices = all_ids(),
                             selected = all_ids())
  }, ignoreInit = FALSE)
  
  observeEvent(input$select_all_animals, {
    updateCheckboxGroupInput(session, "animals", selected = all_ids())
  })
  
  observeEvent(input$unselect_animals, {
    updateCheckboxGroupInput(session, "animals", selected = character(0))
  })
  
  # Filtered data (selected animals)
  selected_data <- reactive({
    req(input$animals)
    d <- data
    d[d[[track_col]] %in% input$animals, ]
  })
  
  ###############
  # attribute choices
  observe({
    d <- selected_data()
    req(nrow(d) > 0)
    
    # Event attributes
    event_attr <- sf::st_drop_geometry(d)
    event_choices <- names(ev)[colSums(!is.na(event_attr)) > 0]
    
    # Track attributes
    track_attr <- mt_track_data(d)
    track_choices <- character(0)
    
    if (!is.null(track_attr) && ncol(track_attr) > 0) {
      trk_choices <- names(track_attr)[colSums(!is.na(track_attr)) > 0]
    }
    
    choices <- sort(unique(c(event_choices, track_choices)))
    
    prev <- isolate(input$select_attr)
    sel  <- if (!is.null(prev)) intersect(prev, choices) else NULL
    
    updateSelectInput(session, "select_attr", choices = choices, selected = sel)
  })
  
  
  ###################
  # Lines 
  track_lines <- reactive({
    d <- selected_data()
    req(nrow(d) >= 2)
    mt_track_lines(d)
  })
  
  mmap <- reactive({
    d <- selected_data()
    req(nrow(d) >= 1)
    
    bounds <- as.vector(sf::st_bbox(d))
    ids <- sort(unique(as.character(d[[track_col]])))
    pal <- colorFactor(palette = pals::glasbey(), domain = ids)
    
    # colors for points
    d$col <- pal(as.character(d[[track_col]]))
    
    # Build and colors  for lines
    tl <- track_lines()
    tl$col <- pal(as.character(tl[[track_col]]))
    
    coords <- sf::st_coordinates(d)
    d$popup_html <- paste0(
      "<b>Track:</b> ", as.character(d[[track_col]]), "<br>",
      "<b>Time:</b> ", as.character(mt_time(d)), "<br>",
      "<b>Lon:</b> ", round(coords[,1], 6), "<br>",
      "<b>Lat:</b> ", round(coords[,2], 6)
    )
    
    leaflet(options = leafletOptions(minZoom = 2)) %>%
      fitBounds(bounds[1], bounds[2], bounds[3], bounds[4]) %>%
      addProviderTiles("OpenStreetMap", group = "OpenStreetMap") %>%
      addProviderTiles("Esri.WorldTopoMap", group = "TopoMap") %>%
      addProviderTiles("Esri.WorldImagery", group = "Aerial") %>%
      addScaleBar(position = "topleft") %>%
      
      addCircleMarkers(data = d,radius = 1, opacity = 0.7, fillOpacity = 0.5, color = ~col, popup = d$popup_html, group = "Points") %>%
      
      addPolylines( data = tl,  weight = 3,  opacity = 0.5, color = ~col,  group = "Lines") %>%
      
      addLegend(position = "bottomright", pal = pal,values = ids, title = "Tracks", opacity = 0.8) %>%
      
      addLayersControl(
        baseGroups = c("OpenStreetMap", "TopoMap", "Aerial"),
        overlayGroups = c("Lines", "Points"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  ########################################
  # Auto-save map for all individuals
  
  saved_html <- reactiveVal(FALSE)
  
  observe({
    if (saved_html()) return()
    
    d <- selected_data()
    req(nrow(d) > 0)
    
    htmlwidgets::saveWidget(widget = isolate(mmap()), file = "./data/output/autosave_leaflet_mapper.html", selfcontained = FALSE )
    
    saved_html(TRUE)
  })
  ######################################
  
  
  
  output$leafmap <- renderLeaflet(mmap())
  
  output$save_html <- downloadHandler(
    filename = function() paste0("LeafletMap_", Sys.Date(), ".html"),
    content = function(file) {
      saveWidget(widget = mmap(), file = file)
    }
  )
  
  return(reactive({ current() }))
}
