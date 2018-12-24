segmentUI <- function(id){
  
  ns <- NS(id)
  
  tagList(
    div(
      class = "container",
      fluidRow(
        column(
          6, uiOutput(ns("daterange"))
        ),
        column(
          6,
          uiOutput(ns("sitetypes"))
        )
      ),
      uiOutput(ns("segments")),
      hr(),
      div(
        class = "well",
        h3("TREND", id = "trend-headline"),
        p("Number of articles per day and segment."),
        echarts4rOutput(ns("trend"))
      )
    )
  )
}

segment <- function(input, output, session, pool){

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
  
  trend_data <- reactive({
    
    req(input$daterangeOut, input$sitetypesOut, input$segmentsOut)
    dates <- input$daterangeOut
    
    date_query <- .dates2query(input$daterangeOut)
    type_query <- .type2query(input$sitetypesOut)
    segment_query <- .select_segments(input$segmentsOut, type = "text")
    
    query <- paste0(
      "SELECT published, ", segment_query, " FROM 'articles' ", date_query, type_query, ";"
    )
    
    published <- dbGetQuery(pool, query)
    
    published %>%
      mutate(
        published = as.POSIXct(published, origin = "1970-01-01"),
        published = as.Date(published)
      ) %>%
      tidyr::gather(
        segments,
        value,
        contains("text_")
      ) %>% 
      mutate(
        segments = gsub("_text_segment", "", segments),
        segments = tools::toTitleCase(segments)
      ) %>% 
      group_by(published, segments) %>% 
      summarise(n = sum(value)) 
    
  })
  
  output$trend <- renderEcharts4r({
    
    trend_data() %>% 
      group_by(segments) %>% 
      e_charts(published) %>% 
      e_line(
        n,
        smooth = TRUE
      ) %>% 
      e_grid(
        left = 25, 
        right = 10, 
        bottom = 20, 
        top = 5
      ) %>% 
      e_legend(right = 25) %>% 
      e_tooltip(trigger = "axis") %>% 
      e_theme(THEME) %>%
      e_text_style(fontFamily = .font()) %>% 
      e_toolbox_feature(feature = "saveAsImage") 
    
  })
  
}
