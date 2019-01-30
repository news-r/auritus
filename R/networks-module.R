globalVariables(
  c(
    ".",
    "tgt",
    "src",
    "name",
    "target",
    "label",
    "size"
  )
)

#' Personas module
#' @param id namespace id.
networksUI <- function(id){
  
  ns <- NS(id)
  
  div(
    class = "container",
    fluidRow(
      column(
        2, uiOutput(ns("daterange"))
      ),
      column(
        2, 
        shinyWidgets::pickerInput(
          inputId = ns("from"), 
          label = "From", 
          c(
            "Person" = "entities_persons",
            "Organisations" = "entities_organizations",
            "Places" = "entities_locations",
			"Media" = "thread_site"
          ),
          selected = "thread_site",
          width = "100%"
        )
      ),
      column(
        2, 
        shinyWidgets::pickerInput(
          inputId = ns("to"), 
          label = "to", 
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
    fluidRow(
      column(8, uiOutput(ns("slider"))),
      column(4, sliderInput(ns("cb"), "Filter edges", 0, max = .9, value = .5, step = .1, width = "100%"))
    ),
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

  output$slider <- renderUI({

    req(input$segmentsOut)

    ns <- session$ns

    sliderInput(
      ns("sliderOut"),
      "Filter nodes:", 
      min(graph_data()$nodes$size), 
      max(graph_data()$nodes$size), 
      min(graph_data()$nodes$size),
      step = 1,
      ticks = FALSE,
      animate = FALSE,
      width = "100%"
    )
  })

  observeEvent(input$sliderOut, {

    ns <- session$ns

    sigmajsProxy(ns("graph")) %>% 
      sg_filter_gt_p(input$sliderOut, "size")
  })
  
  graph_data <- reactive({
    
    date_query <- .dates2query(input$daterangeOut)
    type_query <- .type2query(input$sitetypesOut)
    segment_query <- .select_segments(input$segmentsOut, type = "text")
    
    query <- paste0(
      "SELECT uuid, ", input$from, " AS src,", input$to, " AS tgt, ", segment_query, " FROM articles ", date_query, type_query, ";"
    )
    
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Computing", value = sample(seq(.1, .9, by = .1), 1))
    
    callback <- function(x){
      filter(x, n > quantile(n, input$cb))
    }
    
    graph <- dbGetQuery(pool, query) %>% 
      tidyr::gather(
        segments,
        value,
        contains("text_")
      ) %>% 
	  filter(value > 0) %>% 
      nethoser::net_con(src, tgt, callback = callback)
    
    nodes <- graph$nodes %>% 
      mutate(
        id = name
      ) %>% 
      select(
        id,
        label = name,
        size = n
      )
    
    edges <- graph$edges %>% 
      mutate(id = 1:dplyr::n()) %>% 
      select(
        id,
        source, 
        target,
        weight = n
      )
    
    list(nodes = nodes, edges = edges)
    
  })
  
  output$graph <- renderSigmajs({
    
    req(
      input$daterangeOut,
      input$sitetypesOut,
      input$segmentsOut
    )
    
    cols <- .get_colors()
    
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Drawing", value = sample(seq(.1, .9, by = .1), 1))
    
    sigmajs() %>% 
      sg_nodes(graph_data()$nodes, id, label, size) %>% 
      sg_edges(graph_data()$edges, id, source, target) %>% 
      sg_force() %>% 
      sg_force_stop(5000) %>% 
      sg_kill() %>% 
      sg_neighbours() %>% 
      sg_drag_nodes() %>% 
      sg_cluster(
        colors = cols$pal
      ) %>% 
      sg_settings(
        edgeColor = "default",
        defaultEdgeColor = "#d3d3d3",
        labelThreshold = 999999
      )
    
  })
  
}