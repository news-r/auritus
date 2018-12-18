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
    p(heading, class = "text-center text-danger", style = "margin-bottom:0;")
  })

  output$value <- renderUI({
    h3(react(), class = "text-center", style = "margin-top:0;")
  })

  output$tip <- renderTippy({
    ns <- session$ns
    tippy_this(ns("tooltip"), tooltip)
  })

}
