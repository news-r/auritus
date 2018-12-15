#' Homepage
#' @param id Namespace id
homeUI <- function(id){

  ns <- NS(id)

  tagList(
    bulmaHero(
      fullheight = TRUE,
      color = "light",
      bulmaHeroBody(
        bulmaContainer(
          bulmaFigure(
            src = "https://auritus.io/logo.png",
            alt = "auritus"
          ),
          br(),
          br(),
          bulmaTitle("Free, Open-Source News Monitoring Platform."),
          br(),
          bulmaColumns(
            bulmaColumn(
              width = 4,
              bulmaButton(
                bulmaIcon(
                  "fa fa-github"
                ),
                href = "https://github.com/JohnCoene/auritus",
                target = "_blank",
                span("Github")
              ),
              bulmaButton(
                bulmaIcon(
                  "fa fa-desktop"
                ),
                href = "https://auritus.io/",
                target = "_blank",
                span("Website")
              )
            ),
            bulmaColumn(
              width = 8,
              uiOutput(ns("recent"))
            )
          )
        )
      )
    )
  )

}

home <- function(input, output, session){

  DB <- .get_db()

  max <- reactive({

    if(length(DB) == 1){
      mx <- max(load_data()$published)
    } else {
      args <- .db_con(DB)
      con <- do.call(dbConnect, args)
      mx <- dbGetQuery(con, "SELECT MAX(published) FROM 'articles';")
      dbDisconnect(con)
    }

    if(inherits(mx, "numeric"))
      mx <- as.POSIXct(mx[[1]], origin = "1970-01-01 12:00")

    return(mx)

  })

  output$recent <- renderUI({

    p(
      class = "is-pulled-right",
      "Most recent article in the bank was published",
      format(mx, "%d %B %Y")
    )
  })

}
