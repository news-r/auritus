displayUI <- function(id){

  ns <- NS(id)

  tagList(
    uiOutput(ns("lvl")),
    tippyOutput(ns("tip"))
  )

}

display <- function(input, output, session, heading, value, tooltip){

  output$lvl <- renderUI({
    bulmaLevelItem(
      heading,
      value
    )
  })

  output$tip <- renderTippy({
    ns <- session$ns
    tippy_this(ns("lvl"), tooltip)
  })

}
