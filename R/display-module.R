#' Disaply level
#' @param id A namesapce id.
displayUI <- function(id){

  ns <- NS(id)

  div(
    id = ns("tooltip"),
    uiOutput(ns("heading")),
    uiOutput(ns("value")),
    tippyOutput(ns("tip"))
  )

}

#' Display server
#' @param input,output,session Usual Shiny server arguments.
#' @param heading Text heading.
#' @param react the reactive.
#' @param tooltip The tooltip.
display <- function(input, output, session, heading, react, tooltip){

  output$heading <- renderUI({
    p(heading, class = "text-center text-danger", style = "margin-bottom:0;font-size:.7em;")
  })

  output$value <- renderUI({
    h3(prettyNum(react(), big.mark = ","), class = "text-center", style = "margin-top:0;")
  })

  output$tip <- renderTippy({
    ns <- session$ns
    tippy_this(ns("tooltip"), tooltip)
  })

}
