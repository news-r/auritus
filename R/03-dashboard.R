#' Launch Auritus
#' 
#' Launch Auritus.
#'
#' @export
launch_auritus <- function(){
  
  config <- "_auritus.yml"
  
  if(!file.exists(config)){
    cat(
      crayon::red(cli::symbol$cross), "No", crayon::underline("_auritus.yml"), "configuration file."
    )
    return(NULL)
  }
  
  settings <- yaml::read_yaml(config)
  settings_list <- names(settings)
  
  if(!"theme" %in% settings_list){
    cat(
      crayon::yellow(cli::symbol$warning), "No theme set in _autitus.yml, defaulting to",
      crayon::underline("paper.\n")
    )
    theme <- "paper"
  } else {
    theme <- settings$theme
  }
  
  ui <- navbarPage(
    title = "auritus",
    windowTitle = "auritus",
    id = "navbarTabs",
    theme = shinythemes::shinytheme(theme),
    tabPanel(
      "Home",
      icon = icon("home"),
      homeUI("home")
    )
  )
  
  server <- function(input, output, session){
    
  }
  
  shinyApp(ui, server)
  
  
}
