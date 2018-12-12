#' Homepage
#' @param id Namespace id
homeUI <- function(id){
  
  ns <- NS(id)
  
  tagList(
    div(
      class = "container",
      div(
        class = "jumbotron",
        h1("auritus", class = "auritus"),
        br(),
        p("Free, Open-source Public Relations Analytics Platform.")
      )
    )
  )
  
}