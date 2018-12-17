#' Homepage
#' @param id Namespace id
homeUI <- function(id){

  ns <- NS(id)

  tagList(
    br(),
    br(),
    div(
      class = "container",
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

  DB <- .get_db()

  latest <- reactive({

    if(is.null(pool)){

      dat <- get_articles()

      mx <- max(dat$published)
    } else {
      mx <- dbGetQuery(pool, "SELECT MAX(published) FROM 'articles';")

      if(inherits(mx[[1]], "numeric"))
        mx <- as.POSIXct(mx[[1]], origin = "1970-01-01 12:00")
    }

    return(mx)

  })

  output$recent <- renderUI({

    h4(
      class = "pull-right",
      "Most recent article in the bank was published on",
      format(latest(), "%d %B %Y")
    )
  })

}
