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
  
  if(!"theme" %in% names(settings[["style"]])){
    cat(
      crayon::yellow(cli::symbol$warning), "No theme set in _autitus.yml, defaulting to",
      crayon::underline("paper.\n")
    )
    theme <- "paper"
  } else {
    theme <- settings[["style"]][["theme"]]
  }
  
  if(!"font" %in% names(settings[["style"]])){
    cat(
      crayon::yellow(cli::symbol$warning), "No font set in _autitus.yml, defaulting to",
      crayon::underline("Raleway.\n")
    )
    
    font <- "Raleway"
  } else {
    font <- settings[["style"]][["font"]]
  }
  
  font_name <- gsub("[[:space:]]", "+", font)
  
  ui <- navbarPage(
    title = "auritus",
    windowTitle = "auritus",
    id = "navbarTabs",
    inverse = settings[["style"]][["inverse"]] %||% FALSE,
    theme = shinythemes::shinytheme(theme),
    header = tagList(
      tags$link(
        href = paste0("https://fonts.googleapis.com/css?family=", font_name),
        rel = "stylesheet"
      ),
      tags$style(
        paste0("*{font-family: '", font, "', sans-serif;}")
      )
    ),
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
