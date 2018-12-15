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

  THEME <- .get_theme()

  hide <- list(show = FALSE)

  output$trend <- renderEcharts4r({

    articles() %>%
      mutate(published = as.Date(published)) %>%
      count(published) %>%
      e_charts(published) %>%
      e_bar(
        n,
        name = "articles",
        legend = FALSE
      ) %>%
      e_theme(THEME) %>%
      e_tooltip("axis") %>%
      e_x_axis(name = "Date", splitLine = hide) %>%
      e_y_axis(name = "Articles", splitLine = hide) %>%
      e_title("Articles", "Number of articles per day")

  })

}
