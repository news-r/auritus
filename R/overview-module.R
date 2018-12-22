#' Overview
#' @param id namespace id.
overviewUI <- function(id){

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
      hr(),
      fluidRow(
        column(3, displayUI(ns("narticles"))),
        column(3, displayUI(ns("noutlets"))),
        column(3, displayUI(ns("nshares"))),
        column(3, displayUI(ns("sentiment")))
      ),
      echarts4rOutput(ns("siteType"), height = 50),
      br(),
      fluidRow(
        column(
          6,
          div(
            class = "well",
            h3("TREND", id = "trend-headline"),
            tippy_this("trend-headline", "Number of articles per day."),
            echarts4rOutput(ns("trend"))
          )
        ),
        column(
          6,
          div(
            class = "well",
            h3("REACH", id = "map-headline"),
            tippy_this("map-headline", "Number of articles per country."),
            echarts4rOutput(ns("map"))
          )
        )
      ),
      br(),
      fluidRow(
        column(3, displayUI(ns("language"))),
        column(3, displayUI(ns("topmedia"))),
        column(3, displayUI(ns("nwords"))),
        column(3, displayUI(ns("lexdiv")))
      )
    )
  )

}

overview <- function(input, output, session, pool){

  # common
  THEME <- .get_theme()
  hide <- list(show = FALSE)

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
      label = "Label", 
      choices = choices, 
      selected = choices,
      justified = TRUE, 
      width = "100%",
      checkIcon = list(yes = icon("ok", lib = "glyphicon"))
    )
  })

  n_articles <- reactive({

    req(input$daterangeOut, input$sitetypesOut)
    dates <- input$daterangeOut

    base_query <- "SELECT COUNT(uuid) AS n FROM articles"

    date_query <- .dates2query(input$daterangeOut)
    
    type_query <- .type2query(input$sitetypesOut)

    query <- paste(base_query, date_query, type_query, ";")

    N <- dbGetQuery(pool, query) %>%
      pull(n)

    return(N)

  })

  n_outlets <- reactive({

    req(input$daterangeOut, input$sitetypesOut)
    dates <- input$daterangeOut

    base_query <- "SELECT thread_site, COUNT(DISTINCT thread_site) AS n FROM articles"
    
    date_query <- .dates2query(input$daterangeOut)

    type_query <- .type2query(input$sitetypesOut)
    
    query <- paste(
      base_query, date_query, type_query, ";"
    )

    N <- dbGetQuery(pool, query) %>%
      pull(n)

    return(N)
  })

  n_shares <- reactive({

    req(input$daterangeOut, input$sitetypesOut)
    dates <- input$daterangeOut

    base_query <- "SELECT COUNT(thread_social_facebook_shares) AS n FROM articles"

    date_query <- .dates2query(input$daterangeOut)
    
    type_query <- .type2query(input$sitetypesOut)

    query <- paste(base_query, date_query, type_query, ";")

    N <- dbGetQuery(pool, query) %>%
      pull(n)

    return(N)

  })
  
  sentiment <- reactive({
    
    req(input$daterangeOut, input$sitetypesOut)
    dates <- input$daterangeOut
    
    base_query <- "SELECT AVG(sentiment) AS sentiment FROM articles"
    
    date_query <- .dates2query(input$daterangeOut)
    
    type_query <- .type2query(input$sitetypesOut)
    
    query <- paste(base_query, date_query, type_query, ";")
    
    N <- dbGetQuery(pool, query) %>%
      pull(sentiment) %>% 
      round()
    
    return(N)
    
  })
  
  language <- reactive({
    
    req(input$daterangeOut, input$sitetypesOut)
    dates <- input$daterangeOut
    
    base_query <- "SELECT language, COUNT(language) AS n FROM articles"
    
    date_query <- .dates2query(input$daterangeOut)
    
    type_query <- .type2query(input$sitetypesOut)
    
    query <- paste(
      base_query, date_query, type_query, ";"
    )
    
    N <- dbGetQuery(pool, query) %>%
      pull(language)
    
    return(N)
  })
  
  topmedia <- reactive({
    
    req(input$daterangeOut, input$sitetypesOut)
    dates <- input$daterangeOut
    
    base_query <- "SELECT thread_site, COUNT(thread_site) AS n FROM articles"
    
    date_query <- .dates2query(input$daterangeOut)
    
    type_query <- .type2query(input$sitetypesOut)
    
    query <- paste(base_query, date_query, type_query, ";")
    
    N <- dbGetQuery(pool, query) %>%
      pull(thread_site)
    
    N <- gsub("\\..*", "", N)
    
    return(N)
    
  })
  
  nwords <- reactive({
    
    req(input$daterangeOut, input$sitetypesOut)
    dates <- input$daterangeOut
    
    base_query <- "SELECT AVG(nwords) AS nwords FROM articles"
    
    date_query <- .dates2query(input$daterangeOut)
    
    type_query <- .type2query(input$sitetypesOut)
    
    query <- paste(base_query, date_query, type_query, ";")
    
    N <- dbGetQuery(pool, query) %>%
      pull(nwords) %>% 
      round()
    
    return(N)
    
  })
  
  lexdiv <- reactive({
    
    req(input$daterangeOut, input$sitetypesOut)
    dates <- input$daterangeOut
    
    base_query <- "SELECT AVG(lexdiv) AS lexdiv FROM articles"
    
    date_query <- .dates2query(input$daterangeOut)
    
    type_query <- .type2query(input$sitetypesOut)
    
    query <- paste(base_query, date_query, type_query, ";")
    
    N <- dbGetQuery(pool, query) %>%
      pull(lexdiv) %>% 
      round()
    
    return(N)
    
  })

  callModule(display, "narticles", heading = "ARTICLES", react = n_articles, tooltip = "Number of articles")
  callModule(display, "noutlets", heading = "MEDIA OUTLETS", react = n_outlets, tooltip = "Number of media outlets reached")
  callModule(display, "nshares", heading = "FACEBOOK SHARES", react = n_shares, tooltip = "Number of Facebook shares")
  callModule(display, "sentiment", heading = "SENTIMENT", react = sentiment, tooltip = "Average setniment")
  callModule(display, "language", heading = "TOP LANGUAGE", react = language, tooltip = "Most popular language")
  callModule(display, "topmedia", heading = "TOP OUTLET", react = topmedia, tooltip = "Most popular outlet")
  callModule(display, "nwords", heading = "ARTICLE LENGTH", react = nwords, tooltip = "Average number of words per article")
  callModule(display, "lexdiv", heading = "SOPHISTICATION", react = lexdiv, tooltip = "How sophsiticated the language of articles are (average lexical diversity)")

  country <- reactive({

    req(input$daterangeOut, input$sitetypesOut)

    date_query <- .dates2query(input$daterangeOut)
    
    type_query <- .type2query(input$sitetypesOut)

    query <- paste(
      "SELECT thread_country, COUNT(thread_country) AS n FROM articles",
      date_query, type_query,
      "AND thread_country <> '' GROUP BY thread_country ORDER BY count(thread_country) DESC;"
    )

    country <- dbGetQuery(pool, query) %>%
      e_country_names(thread_country, thread_country)

  })

  trend_data <- reactive({

    req(input$daterangeOut, input$sitetypesOut)
    dates <- input$daterangeOut

    date_query <- .dates2query(input$daterangeOut)
    
    type_query <- .type2query(input$sitetypesOut)

    query <- paste0(
      "SELECT published FROM 'articles' ", date_query, type_query, ";"
    )

    published <- dbGetQuery(pool, query)

    published %>%
      mutate(
        published = as.POSIXct(published, origin = "1970-01-01"),
        published = as.Date(published)
      ) %>%
      count(published)

  })

  type_data <- reactive({

    req(input$daterangeOut, input$sitetypesOut)

    date_query <- .dates2query(input$daterangeOut)
    
    type_query <- .type2query(input$sitetypesOut)

    query <- paste0(
      "SELECT thread_site_type AS type, COUNT(thread_site_type) AS n FROM 'articles' ", 
      date_query, type_query,
      "GROUP BY thread_site_type ORDER BY count(thread_site_type) DESC;"
    )

    dbGetQuery(pool, query)

  })

  output$siteType <- renderEcharts4r({

    type_data() %>%
      mutate(
        x = "articles",
        n = n / sum(n),
        n = round(n * 100, 1)
      ) %>% 
      group_by(type) %>% 
      e_charts(x, height = 50) %>%
      e_bar(
        n,
        radius = c("50%", "90%"),
        stack = "stack"
      ) %>%
      e_labels(
        position = "inside",
        formatter = "{a}",
        fontSize = 8,
        fontWeight = "bold"
      ) %>% 
      e_grid(
        top = 0,
        bottom = 0,
        left = 0,
        right = 0
      ) %>% 
      e_x_axis(
        show = FALSE
      ) %>% 
      e_y_axis(
        show = FALSE,
        max = 100
      ) %>% 
      e_legend(FALSE) %>% 
      e_flip_coords() %>%
      e_theme(THEME) %>%
      e_text_style(fontFamily = .font()) %>% 
      e_tooltip(
        formatter = htmlwidgets::JS("function(params){return(params.value[0] + '%')}")
      )

  })

  output$trend <- renderEcharts4r({

    trend_data() %>%
      e_charts(published, dispose = FALSE) %>%
      e_area(
        n,
        name = "articles",
        legend = FALSE,
        smooth = TRUE,
        smoothMonotone = "x"
      ) %>%
      e_tooltip("axis") %>%
      e_x_axis(name = "Date", splitLine = hide) %>%
      e_y_axis(name = "Articles", splitLine = hide) %>%
      e_datazoom() %>%
      e_theme(THEME) %>%
      e_text_style(fontFamily = .font())

  })

  output$map <- renderEcharts4r({

    country() %>%
      e_charts(thread_country, dispose = FALSE) %>%
      e_map_3d(n, name = "Articles by countries") %>%
      e_visual_map(n, orient = "horizontal", bottom = "5%", right = "5%") %>%
      e_theme(THEME) %>%
      e_text_style(fontFamily = .font()) %>%
      e_tooltip()
  })

}
