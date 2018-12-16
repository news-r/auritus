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

  latest <- reactive({

    if(length(DB) == 1){
      
      dat <- load_data()
      
      mx <- max(dat$published)
    } else {
      args <- .db_con(DB)
      con <- do.call(dbConnect, args)
      mx <- dbGetQuery(con, "SELECT MAX(published) FROM 'articles';")
      dbDisconnect(con)
      
      if(inherits(mx[[1]], "numeric"))
        mx <- as.POSIXct(mx[[1]], origin = "1970-01-01 12:00")
    }

    return(mx)

  })

  output$recent <- renderUI({

    p(
      class = "is-pulled-right",
      "Most recent article in the bank was published",
      format(latest(), "%d %B %Y")
    )
  })

}
