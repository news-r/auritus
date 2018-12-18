#' Overview
#' @param id namespace id.
overviewUI <- function(id){

  ns <- NS(id)

  tagList(
    div(
      class = "container",
      fluidRow(
        column(
          4, uiOutput(ns("daterange"))
        )
      ),
      hr(),
      fluidRow(
        column(4, displayUI(ns("narticles"))),
        column(4, displayUI(ns("noutlets"))),
        column(4, displayUI(ns("nshares")))
      ),
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
        column(
          6,
          div(
            class = "well",
            h3("TYPES", id = "type-headline"),
            tippy_this("type-headline", "Number of articles per site type."),
            echarts4rOutput(ns("siteType"))
          )
        )
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

  n_articles <- reactive({

    req(input$daterangeOut)
    dates <- input$daterangeOut

    base_query <- "SELECT COUNT(uuid) AS n FROM articles"

    date_query <- paste0(
      "WHERE published >= '", dates[1], " 00:00:00' AND published <= '", dates[2], " 23:59:59';"
    )

    query <- paste(base_query, date_query)

    N <- dbGetQuery(pool, query) %>%
      pull(n)

    return(N)

  })

  n_outlets <- reactive({

    req(input$daterangeOut)
    dates <- input$daterangeOut

    base_query <- "SELECT thread_site, COUNT(DISTINCT thread_site) AS n FROM articles"
    date_query <- paste0(
      "WHERE published >= '", dates[1], " 00:00:00' AND published <= '", dates[2], " 23:59:59';"
    )

    query <- paste(
      base_query, date_query
    )

    N <- dbGetQuery(pool, query) %>%
      pull(n)

    return(N)
  })

  n_shares <- reactive({

    req(input$daterangeOut)
    dates <- input$daterangeOut

    base_query <- "SELECT COUNT(thread_social_facebook_shares) AS n FROM articles"

    date_query <- paste0(
      "WHERE published >= '", dates[1], " 00:00:00' AND published <= '", dates[2], " 23:59:59';"
    )

    query <- paste(base_query, date_query)

    N <- dbGetQuery(pool, query) %>%
      pull(n)

    return(N)

  })

  callModule(display, "narticles", heading = "ARTICLES", react = n_articles, tooltip = "Number of articles")
  callModule(display, "noutlets", heading = "MEDIA OUTLETS", react = n_outlets, tooltip = "Number of media outlets reached")
  callModule(display, "nshares", heading = "FACEBOOK SHARES", react = n_shares, tooltip = "Number of Facebook shares")

  country <- reactive({

    req(input$daterangeOut)
    dates <- input$daterangeOut

    date_query <- paste0(
      "AND published >= '", dates[1], " 00:00:00' AND published <= '", dates[2], " 23:59:59'"
    )

    query <- paste(
      "SELECT thread_country, COUNT(thread_country) AS n FROM articles WHERE thread_country <> ''",
      date_query,
      "GROUP BY thread_country ORDER BY count(thread_country) DESC;"
    )

    country <- dbGetQuery(pool, query) %>%
      e_country_names(thread_country, thread_country)

  })

  trend_data <- reactive({

    req(input$daterangeOut)
    dates <- input$daterangeOut

    date_query <- paste0(
      "WHERE published >= '", dates[1], " 00:00:00' AND published <= '", dates[2], " 23:59:59'"
    )

    query <- paste0(
      "SELECT published FROM 'articles' ", date_query, ";"
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

    req(input$daterangeOut)
    dates <- input$daterangeOut

    date_query <- paste0(
      "WHERE published >= '", dates[1], " 00:00:00' AND published <= '", dates[2], " 23:59:59'"
    )

    query <- paste0(
      "SELECT thread_site_type AS type, COUNT(thread_site_type) AS n FROM 'articles' ", date_query,
      "GROUP BY thread_site_type ORDER BY count(thread_site_type) DESC;"
    )

    dbGetQuery(pool, query)

  })

  output$siteType <- renderEcharts4r({

    type_data() %>%
      e_charts(type) %>%
      e_pie(
        n,
        name = "Articles of type",
        radius = c("50%", "90%")
      )  %>%
      e_legend(bottom = 5) %>%
      e_theme(THEME) %>%
      e_text_style(fontFamily = .font())

  })

  output$trend <- renderEcharts4r({

    trend_data() %>%
      e_charts(published, dispose = TRUE) %>%
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
      e_charts(thread_country, dispose = TRUE) %>%
      e_map_3d(n, name = "Articles by countries") %>%
      e_visual_map(n, orient = "horizontal", bottom = "5%", right = "5%") %>%
      e_theme(THEME) %>%
      e_text_style(fontFamily = .font()) %>%
      e_tooltip()
  })

}
