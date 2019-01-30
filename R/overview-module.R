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
      br(),
      fluidRow(
        column(3, displayUI(ns("language"))),
        column(3, displayUI(ns("topmedia"))),
        column(3, displayUI(ns("nwords"))),
        column(3, displayUI(ns("lexdiv")))
      ),
      br(),
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
      div(
        class = "well",
        h3("SENTIMENT", id = "sentiment-headline"),
        tippy_this("sentiment-headline", "Three day rolling sentiment average."),
        echarts4rOutput(ns("sentimentChart"), height = 230)
      ),
      br(),
      fluidRow(
        column(
          6,
          div(
            class = "well",
            h3("OUTLETS", id = "outlets-headline"),
            tippy_this("outlets-headline", "Top media outlets by number of articles."),
            echarts4rOutput(ns("outlets"), height = 400)
          )
        ),
        column(
          6,
          div(
            class = "well",
            h3("PLACES", id = "locations-headline"),
            tippy_this("locations-headline", "Locations mentioned in the articles."),
            echarts4rOutput(ns("locations"), height = 400)
          )
        )
      ),
      div(
        class = "well",
        h3("SOPHISTICATION", id = "sophistication-headline"),
        tippy_this("sophistication-headline", "Daily average language sophistication."),
        echarts4rOutput(ns("lexdivChart"), height = 230)
      )
    )
  )

}

overview <- function(input, output, session, pool){

  # common
  THEME <- .get_theme()
  hide <- list(show = FALSE)
  avg <- list(
    type = "average",
    name = "AVG"
  )

  output$daterange <- renderUI({

    ns <- session$ns

    query <- "SELECT MIN(published), MAX(published) FROM articles;"
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

    base_query <- "SELECT COUNT(DISTINCT thread_site) AS n FROM articles"
    
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

    base_query <- "SELECT SUM(thread_social_facebook_shares) AS n FROM articles"

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
      base_query, date_query, type_query, "GROUP BY language;"
    )
    
    N <- dbGetQuery(pool, query) %>%
      pull(language)
    
    return(N)
  })
  
  topmedia <- reactive({
    
    req(input$daterangeOut, input$sitetypesOut)
    
    base_query <- "SELECT thread_site, COUNT(thread_site) AS n FROM articles"
    date_query <- .dates2query(input$daterangeOut)
    type_query <- .type2query(input$sitetypesOut)
    
    query <- paste(base_query, date_query, type_query, 
                   "GROUP BY thread_site  HAVING COUNT(thread_site) > 1 ORDER BY count(thread_site) DESC LIMIT 1;")
    
    dbGetQuery(pool, query) %>%
      mutate(
        thread_site = gsub("\\..*", "", thread_site)
      )
    
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

  callModule(display, "narticles", heading = "ARTICLES", react = n_articles, tooltip = "Total number of articles")
  callModule(display, "noutlets", heading = "MEDIA OUTLETS", react = n_outlets, tooltip = "Number of media outlets reached")
  callModule(display, "nshares", heading = "FACEBOOK SHARES", react = n_shares, tooltip = "Number of Facebook shares")
  callModule(display, "sentiment", heading = "SENTIMENT", react = sentiment, tooltip = "Average sentiment score")
  callModule(display, "language", heading = "TOP LANGUAGE", react = language, tooltip = "Most popular language by number of articles")
  callModule(display, "topmedia", heading = "TOP OUTLET", react = topmedia, tooltip = "Most popular outlet by number of articles")
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
      "SELECT query_name, published FROM articles ", date_query, type_query, ";"
    )

    published <- dbGetQuery(pool, query)

    published %>%
      mutate(
        published = as.POSIXct(published, origin = "1970-01-01"),
        published = as.Date(published)
      ) %>%
      count(query_name, published)

  })

  lexdiv_data <- reactive({

    req(input$daterangeOut, input$sitetypesOut)
    dates <- input$daterangeOut

    date_query <- .dates2query(input$daterangeOut)
    
    type_query <- .type2query(input$sitetypesOut)

    query <- paste0(
      "SELECT query_name, published, lexdiv FROM articles ", date_query, type_query, ";"
    )

    published <- dbGetQuery(pool, query)

    published %>%
      mutate(
        published = as.POSIXct(published, origin = "1970-01-01"),
        published = as.Date(published)
      ) %>%
      group_by(published) %>% 
      summarise(
        lexdiv = mean(lexdiv, na.rm = TRUE)
      ) %>% 
      ungroup()

  })

  type_data <- reactive({

    req(input$daterangeOut, input$sitetypesOut)

    date_query <- .dates2query(input$daterangeOut)
    
    type_query <- .type2query(input$sitetypesOut)

    query <- paste0(
      "SELECT thread_site_type AS type, COUNT(thread_site_type) AS n FROM articles ", 
      date_query, type_query,
      "GROUP BY thread_site_type ORDER BY count(thread_site_type) DESC;"
    )

    dbGetQuery(pool, query)

  })
  
  sentiment_chart <- reactive({
    
    req(input$daterangeOut, input$sitetypesOut)
    
    date_query <- .dates2query(input$daterangeOut)
    type_query <- .type2query(input$sitetypesOut)
    
    query <- paste0(
      "SELECT published, sentiment FROM articles ", 
      date_query, type_query,
      ";"
    )
    
    dbGetQuery(pool, query) %>% 
      mutate(
        published = as.Date(published)
      ) %>% 
      group_by(published) %>% 
      summarise(sentiment = sum(sentiment) / n()) %>% 
      ungroup() %>% 
      mutate(
        sent_lag1 = lag(sentiment, 1),
        sent_lag2 = lag(sentiment, 2),
        sent_lag3 = lag(sentiment, 3),
        sent_lag = (sent_lag1 + sent_lag2 + sent_lag3) / 3,
        sent_lag = round(sent_lag)
      ) %>% 
      filter(!is.na(sent_lag))
    
  })
  
  media_data <- reactive({
    
    req(input$daterangeOut, input$sitetypesOut)
    dates <- input$daterangeOut
    
    base_query <- "SELECT thread_site, COUNT(thread_site) AS n FROM articles"
    date_query <- .dates2query(input$daterangeOut)
    type_query <- .type2query(input$sitetypesOut)
    
    query <- paste(base_query, date_query, type_query, 
                   "GROUP BY thread_site  HAVING COUNT(thread_site) > 1 ORDER BY count(thread_site) DESC LIMIT 150;")
    
    dbGetQuery(pool, query) %>%
      mutate(
        thread_site = gsub("\\..*", "", thread_site)
      )
    
  })
  
  location_data <- reactive({
    
    req(input$daterangeOut, input$sitetypesOut)
    dates <- input$daterangeOut
    
    base_query <- "SELECT entities_locations FROM articles"
    date_query <- .dates2query(input$daterangeOut)
    type_query <- .type2query(input$sitetypesOut)
    
    query <- paste(base_query, date_query, type_query, 
                   "AND entities_locations <> '';")
    
    dbGetQuery(pool, query) %>% 
      tidyr::separate_rows(entities_locations, sep = ",") %>% 
      count(entities_locations, sort = T) %>% 
      slice(1:150)
    
  })

  output$sentimentChart <- renderEcharts4r({
    
    rng <- range(sentiment_chart()$sent_lag)
    
    sentiment_chart() %>%
      e_charts(published) %>% 
      e_area(
        sent_lag, 
        name = "3 day sentiment average", 
        legend = FALSE,
        smooth = TRUE
      ) %>% 
      e_visual_map(sent_lag, show = FALSE) %>% 
      e_tooltip(trigger = "axis") %>% 
      e_y_axis(
        axisLine = hide, 
        axisTick = hide, 
        axisLabel = hide,
        min = rng[1],
        max = rng[2]
      ) %>% 
      e_grid(
        left = 10, 
        right = 10, 
        bottom = 20, 
        top = 5
      ) %>% 
      e_mark_line(
        data = avg,
        precision = 0,
        label = list(
          position = "middle"
        )
      ) %>% 
      e_theme(THEME) %>%
      e_text_style(fontFamily = .font()) %>% 
      e_toolbox_feature(feature = "saveAsImage") 
    
  })

  output$lexdivChart <- renderEcharts4r({
    
    rng <- range(lexdiv_data()$lexdiv)
    
    lexdiv_data() %>%
      e_charts(published) %>% 
      e_area(
        lexdiv, 
        name = "Sophistication", 
        legend = FALSE,
        smooth = TRUE
      ) %>% 
      e_visual_map(lexdiv, show = FALSE) %>% 
      e_tooltip(trigger = "axis") %>% 
      e_y_axis(
        axisLine = hide, 
        axisTick = hide, 
        axisLabel = hide,
        min = rng[1],
        max = rng[2]
      ) %>% 
      e_grid(
        left = 10, 
        right = 10, 
        bottom = 20, 
        top = 5
      ) %>% 
      e_mark_line(
        data = avg,
        precision = 0,
        label = list(
          position = "middle"
        )
      ) %>% 
      e_theme(THEME) %>%
      e_text_style(fontFamily = .font()) %>% 
      e_toolbox_feature(feature = "saveAsImage") 
    
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
      ) %>% 
      e_toolbox_feature(feature = "saveAsImage") 

  })

  output$trend <- renderEcharts4r({

    trend_data() %>%
      group_by(query_name) %>% 
      e_charts(published, dispose = FALSE) %>%
      e_area(
        n,
        smooth = TRUE,
        smoothMonotone = "x"
      ) %>%
      e_tooltip("axis") %>%
      e_x_axis(
        name = "Date", 
        splitLine = hide
      ) %>%
      e_y_axis(
        name = "Articles", 
        splitLine = hide
      ) %>%
      e_grid(
        left = 25, 
        right = 10, 
        bottom = 20, 
        top = 20
      ) %>% 
      e_legend(right = 25) %>% 
      e_mark_line(
        data = avg,
        precision = 0,
        label = list(
          position = "middle"
        )
      ) %>%
      e_theme(THEME) %>%
      e_text_style(fontFamily = .font()) %>% 
      e_toolbox_feature(feature = "saveAsImage") 

  })

  output$map <- renderEcharts4r({

    country() %>%
      e_charts(thread_country, dispose = FALSE) %>%
      e_map_3d(n, name = "Number of articles") %>%
      e_visual_map(n, show = FALSE) %>%
      e_theme(THEME) %>%
      e_text_style(fontFamily = .font()) %>% 
      e_tooltip() %>% 
      e_toolbox_feature(feature = "saveAsImage") 
    
  })
  
  output$outlets <- renderEcharts4r({
    
    media_data() %>% 
      e_color_range(n, color, colors = c("#8adbdb", "#516d8a")) %>% 
      e_charts() %>% 
      e_cloud(thread_site, n, color, shape = "circle") %>% 
      e_grid(
        left = 25, 
        right = 10, 
        bottom = 20, 
        top = 20
      ) %>% 
      e_theme(THEME) %>%
      e_text_style(fontFamily = .font()) %>% 
      e_tooltip() %>% 
      e_toolbox_feature(feature = "saveAsImage") 
    
  })
  
  output$locations <- renderEcharts4r({
    
    location_data() %>% 
      e_color_range(n, color, colors = c("#8adbdb", "#516d8a")) %>% 
      e_charts() %>% 
      e_cloud(entities_locations, n, color, shape = "circle") %>% 
      e_grid(
        left = 25, 
        right = 10, 
        bottom = 20, 
        top = 20
      ) %>% 
      e_theme(THEME) %>%
      e_text_style(fontFamily = .font()) %>% 
      e_tooltip() %>% 
      e_toolbox_feature(feature = "saveAsImage") 
    
  })
  
  observeEvent(input$outlets_clicked_data, {
    
    sel <- input$outlets_clicked_data$name
    
    dates <- input$daterangeOut
    
    date_query <- .dates2query(input$daterangeOut)
    type_query <- .type2query(input$sitetypesOut)
    outlet_query <- paste0("AND thread_site LIKE '%", sel, "%'")
    
    query <- paste0(
      "SELECT thread_main_image, thread_title, thread_url FROM 'articles' ", 
      date_query, type_query, outlet_query, " ORDER BY lexdiv DESC;"
    )
    
    dat <- dbGetQuery(pool, query)
    
    tgs <- tagList()
    for(i in 1:nrow(dat)){
      art <- tags$li(
        class = "list-group-item",
        tags$img(
          src = dat$thread_main_image[i],
          height = 70
        ),
        tags$a(
          href = dat$thread_url[i],
          dat$thread_title[i],
          target = "blank"
        )
      )
      tgs <- tagAppendChild(tgs, art)
    }
    
    showModal(
      modalDialog(
        tags$ul(
          class = "list-group",
          tgs
        ),
        title = paste("Articles from", sel),
        easyClose = TRUE
      )
    )
    
  })
  
  observeEvent(input$locations_clicked_data, {
    
    sel <- input$locations_clicked_data$name
    
    dates <- input$daterangeOut
    
    date_query <- .dates2query(input$daterangeOut)
    type_query <- .type2query(input$sitetypesOut)
    outlet_query <- paste0("AND entities_locations LIKE '%", sel, "%'")
    
    query <- paste0(
      "SELECT thread_main_image, thread_title, thread_url FROM 'articles' ", 
      date_query, type_query, outlet_query, " ORDER BY lexdiv DESC;"
    )
    
    dat <- dbGetQuery(pool, query)
    
    tgs <- tagList()
    for(i in 1:nrow(dat)){
      art <- tags$li(
        class = "list-group-item",
        tags$img(
          src = dat$thread_main_image[i],
          height = 70
        ),
        tags$a(
          href = dat$thread_url[i],
          dat$thread_title[i],
          target = "blank"
        )
      )
      tgs <- tagAppendChild(tgs, art)
    }
    
    showModal(
      modalDialog(
        tags$ul(
          class = "list-group",
          tgs
        ),
        title = paste("Articles from", sel),
        easyClose = TRUE
      )
    )
    
  })

}
