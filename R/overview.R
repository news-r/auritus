#' Overview
#' @param id namespace id.
overviewUI <- function(id){

  ns <- NS(id)

  tagList(
    div(
      class = "",
      fluidRow(
        column(4, displayUI(ns("narticles")))
      ),
      hr(),
      fluidRow(
        column(
          6,
          div(
            class = "thumbnail",
            div(
              class = "caption",
              h3("TREND", id = "trend-headline"),
              tippy_this("trend-headline", "Number of articles per day.")
            ),
            echarts4rOutput(ns("trend"))
          )
        ),
        column(
          6,
          div(
            class = "thumbnail",
            div(
              class = "caption",
              h3("GEOGRAPHIC SPREAD", id = "map-headline"),
              tippy_this("map-headline", "Number of articles per country.")
            ),
            echarts4rOutput(ns("map"))
          )
        )
      )
    )
  )

}

overview <- function(input, output, session, pool){

  output$dateRange <- renderUI({
  })

  n_articles <- reactive({

    if(is.null(pool)){

      N <- nrow(get_articles())

    } else {
      N <- dbGetQuery(pool, "SELECT COUNT(uuid) AS n FROM articles;") %>%
        pull(n)

    }

    return(N)

  })

  callModule(display, "narticles", "ARTICLES", n_articles(), "Number of articles")

  # common
  DB <- .get_db()
  THEME <- .get_theme()
  hide <- list(show = FALSE)

  country <- reactive({

    if(is.null(pool)){
      country <- get_articles()

      country <- country %>%
        count(thread_country, sort = T) %>%
        filter(thread_country != "")
    } else {
      query <- "SELECT thread_country, COUNT(thread_country) AS n FROM articles WHERE thread_country <> '' GROUP BY thread_country ORDER BY count(thread_country) DESC;"
      country <- dbGetQuery(pool, query)
    }

    country %>%
      e_country_names(thread_country, thread_country)

  })

  trend_data <- reactive({

    if(is.null(pool)){

      res <- get_articles() %>%
        mutate(published = as.Date(published))

    } else {
      dates <- dbGetQuery(pool, "SELECT published FROM 'articles';")

      res <- dates %>%
        mutate(
          published = as.POSIXct(published, origin = "1970-01-01"),
          published = as.Date(published)
        )
    }

    res %>%
      count(published)

  })

  output$trend <- renderEcharts4r({

    trend_data() %>%
      e_charts(published) %>%
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
      e_charts(thread_country) %>%
      e_map_3d(n, name = "Articles by countries") %>%
      e_visual_map(n, orient = "horizontal", bottom = "5%", right = "5%") %>%
      e_theme(THEME) %>%
      e_text_style(fontFamily = .font())
  })

}
