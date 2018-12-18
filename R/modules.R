displayUI <- function(id){

  ns <- NS(id)

  div(
    id = ns("tooltip"),
    uiOutput(ns("heading")),
    uiOutput(ns("value")),
    tippyOutput(ns("tip"))
  )

}

display <- function(input, output, session, heading, react, tooltip){

  output$heading <- renderUI({
    h5(heading, style = "text-align:center;")
  })

  output$value <- renderUI({
    h3(react(), style = "text-align:center;")
  })

  output$tip <- renderTippy({
    ns <- session$ns
    tippy_this(ns("tooltip"), tooltip)
  })

}
