displayUI <- function(id){

  ns <- NS(id)

  div(
    id = ns("tooltip"),
    uiOutput(ns("heading")),
    h3(countup::countupOutput(ns("value")), style = "text-align:center;"),
    tippyOutput(ns("tip"))
  )

}

display <- function(input, output, session, heading, value, tooltip){

  output$heading <- renderUI({
    h5(heading, style = "text-align:center;")
  })

  output$value <- countup::renderCountup({
    countup::countup(value, duration = 3)
  })

  output$tip <- renderTippy({
    ns <- session$ns
    tippy_this(ns("tooltip"), tooltip)
  })

}
