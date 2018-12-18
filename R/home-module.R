#' Homepage
#' @param id Namespace id
homeUI <- function(id){

  ns <- NS(id)

  tagList(
    br(),
    br(),
    div(
      class = "container",
      br(),
      br(),
      img(
        width = "70%",
        src = "https://auritus.io/logo.png",
        alt = "auritus"
      ),
      br(),
      br(),
      h2("Free, Open-Source News Monitoring Platform."),
      br(),
      br(),
      fluidRow(
        column(
          4,
          tags$a(
            class = "btn btn-default",
            href = "https://github.com/JohnCoene/auritus",
            target = "_blank",
            icon("github"),
            "Github"
          ),
          tags$a(
            class = "btn btn-danger",
            href = "https://auritus.io/",
            target = "_blank",
            icon("desktop"),
            "Website"
          )
        ),
        column(
          8,
          uiOutput(ns("recent"))
        )
      )
    )
  )

}

home <- function(input, output, session, pool){

  output$recent <- renderUI({

    latest <- dbGetQuery(pool, "SELECT MAX(published) FROM 'articles';") %>%
      unlist() %>%
      unname() %>%
      as.Date()

    h4(
      class = "pull-right",
      "Latest article was published on",
      format(latest, "%d %B %Y")
    )
  })

}
