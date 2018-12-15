#' Overview
#' @param id namespace id.
overviewUI <- function(id){

  ns <- NS(id)

  tagList(
    bulmaContainer(
      bulmaColumns(
        bulmaColumn(
          echarts4rOutput(ns("trend"))
        )
      )
    )
  )
}

overview <- function(input, output, session){

  # common
  DB <- .get_db()
  THEME <- .get_theme()
  hide <- list(show = FALSE)

  trend_data <- reactive({

    if(length(DB) == 1){

      res <- load_data() %>%
        mutate(published = as.Date(published))

    } else {
      args <- .db_con(DB)
      con <- do.call(dbConnect, args)
      dates <- dbGetQuery(con, "SELECT published FROM 'articles';")
      dbDisconnect(con)

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
      e_theme(THEME) %>%
      e_tooltip("axis") %>%
      e_x_axis(name = "Date", splitLine = hide) %>%
      e_y_axis(name = "Articles", splitLine = hide) %>%
      e_title("Articles", "Number of articles per day")

  })

}
