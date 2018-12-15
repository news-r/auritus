#' Homepage
#' @param id Namespace id
homeUI <- function(id){
  
  ns <- NS(id)
  
  tagList(
    bulmaHero(
      fullheight = TRUE,
      color = "primary",
      bulmaHeroBody(
        bulmaContainer(
          bulmaTitle("auritus"),
          bulmaSubtitle("Free, Open-Source News Monitoring Platform."),
          br()
        )
      )
    )
  )
  
}