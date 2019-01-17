
#' Personas module
#' @param id namespace id.
networksUI <- function(id){
  
  ns <- NS(id)
  
  div(
    class = "container",
    fluidRow(
      column(
        3, uiOutput(ns("daterange"))
      ),
      column(
        3, 
        shinyWidgets::pickerInput(
          inputId = ns("type"), 
          label = "Type", 
          c(
            "Person" = "entities_persons",
            "Organisations" = "entities_organizations",
            "Places" = "entities_locations"
          ),
          selected = "entities_persons",
          width = "100%"
        )
      ),
      column(
        6,
        uiOutput(ns("sitetypes"))
      )
    ),
    uiOutput(ns("segments")),
    hr(),
    sigmajsOutput(ns("graph"), height = "100vh")
  )
  
}

networks <- function(input, output, session, pool){
  
  # common
  THEME <- .get_theme()
  hide <- list(show = FALSE)
  avg <- list(
    type = "average",
    name = "AVG"
  )
  
  output$daterange <- renderUI({
    
    ns <- session$ns
    
    query <- "SELECT MIN(published) AS min, MAX(published) AS max FROM articles;"
    range <- dbGetQuery(pool, query) %>%
      unname() %>%
      unlist()
    
    if(inherits(range, "numeric"))
      range <- as.POSIXct(range, origin = "1970-01-01 12:00")
    
    range <- as.Date(range)
    
    dateRangeInput(ns("daterangeOut"), "DATE RANGE", min = range[1], max = range[2], start = range[1], end = range[2])
  })
  
  output$sitetypes <- renderUI({
    
    query <- "SELECT DISTINCT thread_site_type AS type FROM articles"
    
    choices <- dbGetQuery(pool, query) %>% 
      pull(type)
    
    ns <- session$ns
    
    shinyWidgets::checkboxGroupButtons(
      inputId = ns("sitetypesOut"), 
      label = "TYPES", 
      choices = choices, 
      selected = choices,
      justified = TRUE, 
      width = "100%",
      checkIcon = list(yes = icon("ok", lib = "glyphicon"))
    )
  })
  
  output$segments <- renderUI({
    
    choices <- getOption("segments")
    
    ns <- session$ns
    
    shinyWidgets::checkboxGroupButtons(
      inputId = ns("segmentsOut"), 
      label = "SEGMENTS", 
      choices = choices, 
      selected = choices,
      justified = TRUE, 
      width = "100%",
      checkIcon = list(yes = icon("ok", lib = "glyphicon"))
    )
    
  })
  
  graph_data <- reactive({
    
    date_query <- .dates2query(input$daterangeOut)
    type_query <- .type2query(input$sitetypesOut)
    segment_query <- .select_segments(input$segmentsOut, type = "text")
    
    query <- paste0(
      "SELECT uuid, ", input$type, " AS entities_persons, ", segment_query, " FROM articles ", date_query, type_query, ";"
    )
    
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Computing", value = 0)
    
    progress$inc(.35, detail = "edges")
    
    edges <- dbGetQuery(pool, query) %>% 
      tidyr::separate_rows(entities_persons, sep = ",") %>%
      mutate(entities_persons = stringr::word(entities_persons, 2)) %>% 
      filter(!is.na(entities_persons)) %>% 
      split(.$uuid) %>% 
      purrr::map_df(function(x){
        tidyr::crossing(
          source = x$entities_persons, 
          target = x$entities_persons
        )
      }) %>%
      filter(target > source) %>% 
      count(source, target, sort = TRUE) %>% 
      mutate(id = 1:dplyr::n())
    
    progress$inc(.75, detail = "nodes")
    
    nodes <- bind_rows(
      edges %>% select(id = source, size = n),
      edges %>% select(id = target, size = n)
    ) %>% 
      group_by(id) %>% 
      summarise(size = sum(size)) %>% 
      ungroup() %>% 
      mutate(size = scales::rescale(size, to = c(3, 20))) %>% 
      mutate(
        label = id
      )
    
    list(nodes = nodes, edges = edges)
    
  })
  
  output$graph <- renderSigmajs({
    
    req(
      input$daterangeOut,
      input$sitetypesOut,
      input$segmentsOut
    )
    
    sigmajs() %>% 
      sg_nodes(graph_data()$nodes, id, label, size) %>% 
      sg_edges(graph_data()$edges, id, source, target) %>% 
      sg_force() %>% 
      sg_force_stop(5000) %>% 
      sg_kill() %>% 
      sg_neighbours() %>% 
      sg_drag_nodes() %>% 
      sg_cluster() %>% 
      sg_settings(
        edgeColor = "default",
        defaultEdgeColor = "#d3d3d3"
      )
    
  })
  
}