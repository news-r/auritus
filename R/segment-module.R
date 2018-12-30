globalVariables(
  c(
    "published",
    "thread_published",
    "crawled",
    "type",
    "thread_site",
    "thread_country",
    "query_name",
    "sent_lag",
    "sent_lag1",
    "sent_lag2",
    "sent_lag3",
    "entities_locations",
    "x",
    "color",
    "segments",
    "type",
    "value",
    "thread_site"
  )
)

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
      fluidRow(
        column(
          8,
          div(
            class = "well",
            h3("TREND", id = "trend-segments-headline"),
            tippy_this("trend-segments-headline", "Number of articles per day and segment."),
            echarts4rOutput(ns("trend"), height = 256),
            br(),
            br(),
            h3("OUTLETS", id = "outlets-segments-headline"),
            tippy_this("outlets-segments-headline", "Top media outlets per segment."),
            echarts4rOutput(ns("outlets"), height = 256)
          )
        ),
        column(
          4,
          div(
            class = "well",
            h3("ARTICLES", id = "articles-segments-headline"),
            tippy_this("articles-segments-headline", "Number of articles that mentiOn each segment."),
            echarts4rOutput(ns("segmentCount"), height = 250)
          ),
          div(
            class = "well",
            h3("NARRATIVE", id = "narrative-segments-headline"),
            tippy_this("narrative-segments-headline", "Proportion of articles that mention the segment where the segment is in the first paragraph of the article."),
            echarts4rOutput(ns("narrative"), height = 250)
          )
        )
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
  
  trend_data <- reactive({
    
    req(input$daterangeOut, input$sitetypesOut, input$segmentsOut)
    dates <- input$daterangeOut
    
    date_query <- .dates2query(input$daterangeOut)
    type_query <- .type2query(input$sitetypesOut)
    segment_query <- .select_segments(input$segmentsOut, type = "text")
    
    query <- paste0(
      "SELECT published, ", segment_query, " FROM articles ", date_query, type_query, ";"
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
  
  narrative_data <- reactive({
    
    req(input$daterangeOut, input$sitetypesOut, input$segmentsOut)
    dates <- input$daterangeOut
    
    date_query <- .dates2query(input$daterangeOut)
    type_query <- .type2query(input$sitetypesOut)
    segment_query_1p <- .select_segments(input$segmentsOut, type = "1p")
    segment_query_text <- .select_segments(input$segmentsOut, type = "total")
    
    query <- paste0(
      "SELECT ", segment_query_1p, ", ", segment_query_text, " FROM articles ", date_query, type_query, ";"
    )
    
    dat <- dbGetQuery(pool, query)
    
    total <- dat %>%
      tidyr::gather(
        segments,
        value,
        contains("total_")
      ) %>% 
      mutate(
        segments = gsub("_total_segment", "", segments),
        segments = tools::toTitleCase(segments)
      ) %>% 
      group_by(segments) %>% 
      summarise(total = sum(value))
    
    dat %>%
      tidyr::gather(
        segments,
        value,
        contains("1p_")
      ) %>% 
      mutate(
        segments = gsub("_1p_segment", "", segments),
        segments = tools::toTitleCase(segments)
      ) %>% 
      group_by(segments) %>% 
      summarise(value = sum(value)) %>% 
      ungroup() %>% 
      left_join(
        total, by = "segments"
      ) %>% 
      mutate(
        value = value / total
      ) %>% 
      mutate(
        value = round(value * 100, 2)
      )
    
  })
  
  n_segment_articles <- reactive({
    
    req(input$daterangeOut, input$sitetypesOut, input$segmentsOut)
    dates <- input$daterangeOut
    
    date_query <- .dates2query(input$daterangeOut)
    type_query <- .type2query(input$sitetypesOut)
    segment_query <- .select_segments(input$segmentsOut, type = "total")
    
    query <- paste0(
      "SELECT ", segment_query, " FROM articles ", date_query, type_query, ";"
    )
    
    dat <- dbGetQuery(pool, query)
    
    dat %>%
      tidyr::gather(
        segments,
        value,
        contains("total_")
      ) %>% 
      mutate(
        segments = gsub("_total_segment", "", segments),
        segments = tools::toTitleCase(segments),
        value = case_when(
          value > 0 ~ 1,
          TRUE ~ 0
        )
      ) %>% 
      group_by(segments) %>% 
      summarise(n = sum(value)) 
    
  })
  
  n_outlets <- reactive({
    
    req(input$daterangeOut, input$sitetypesOut, input$segmentsOut)
    dates <- input$daterangeOut
    
    date_query <- .dates2query(input$daterangeOut)
    type_query <- .type2query(input$sitetypesOut)
    segment_query <- .select_segments(input$segmentsOut, type = "total")
    
    query <- paste0(
      "SELECT ", segment_query, ", thread_site FROM articles ", date_query, type_query, ";"
    )
    
    dat <- dbGetQuery(pool, query) 
    
    dat %>% 
      tidyr::gather(
        segments,
        value,
        contains("total_")
      ) %>% 
      filter(value > 0) %>% 
      count(thread_site, segments) %>% 
      filter(n > 1) %>% 
      arrange(-n) %>% 
      slice(1:100) %>% 
      mutate(
        segments = gsub("_total_segment", "", segments),
        segments = tools::toTitleCase(segments)
      )
    
  })
  
  output$outlets <- renderEcharts4r({
    n_outlets() %>% 
      group_by(segments) %>% 
      e_charts(thread_site) %>% 
      e_bar(n, stack = "stacked") %>% 
      e_grid(
        left = 25, 
        right = 20, 
        bottom = 20, 
        top = 5
      ) %>% 
      e_group("GRP") %>% 
      e_legend(FALSE) %>% 
      e_tooltip(trigger = "axis") %>% 
      e_theme(THEME) %>%
      e_text_style(fontFamily = .font()) %>% 
      e_toolbox_feature(feature = "saveAsImage") 
  })
  
  output$segmentCount <- renderEcharts4r({
    
    mx <- max(n_segment_articles()$n)
    mx <- mx + (mx * .1)
    mx <- round(mx)
    
    N <- nrow(n_segment_articles())
    
    n_segment_articles() %>% 
      e_charts(segments) -> base
    
    if(N > 2)
      base <- base %>% 
        e_radar(n, max = mx, legend = FALSE, name = "articles", itemStyle = list(normal = list(areaStyle = list(type = "default")))) 
    else 
      base <- base %>% e_bar(n, legend = FALSE, name = "articles") 
      
    base %>% 
      e_tooltip(trigger = "axis") %>% 
      e_grid(
        left = 25, 
        right = 10, 
        bottom = 20, 
        top = 5
      ) %>% 
      e_tooltip() %>% 
      e_theme(THEME) %>%
      e_text_style(fontFamily = .font()) %>% 
      e_toolbox_feature(feature = "saveAsImage") 
    
  })
  
  output$trend <- renderEcharts4r({
    
    rng <- range(trend_data()$n)
    
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
        top = 20
      ) %>% 
      e_y_axis(
        min = rng[1],
        max = rng[2]
      ) %>% 
      e_legend(right = 25) %>% 
      e_group("GRP") %>% 
      e_connect_group("GRP") %>% 
      e_tooltip(trigger = "axis") %>% 
      e_theme(THEME) %>%
      e_text_style(fontFamily = .font()) %>% 
      e_toolbox_feature(feature = "saveAsImage") 
    
  })
  
  output$narrative <- renderEcharts4r({
    
    mx <- max(narrative_data()$value)
    mx <- mx + (mx * .5)
    
    N <- nrow(narrative_data())
    
    narrative_data() %>% 
      e_charts(segments) -> base
    
    if(N > 2)
      base <- base %>% 
        e_radar(value, max = mx, legend = FALSE, name = "articles", itemStyle = list(normal = list(areaStyle = list(type = "default")))) 
    else 
      base <- base %>% e_bar(value, legend = FALSE, name = "articles") 
    
    base %>% 
      e_tooltip(trigger = "axis") %>% 
      e_grid(
        left = 25, 
        right = 10, 
        bottom = 20, 
        top = 5
      ) %>% 
      e_tooltip() %>% 
      e_theme(THEME) %>%
      e_text_style(fontFamily = .font()) %>% 
      e_toolbox_feature(feature = "saveAsImage") 
    
  })
  
}
