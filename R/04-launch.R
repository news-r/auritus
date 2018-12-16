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
      crayon::yellow(cli::symbol$warning), "No theme set in _autitus.yml, setting to",
      crayon::underline("default.\n")
    )
    theme <- "default"
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

  if(!"chart_theme" %in% names(settings[["style"]])){
    cat(
      crayon::yellow(cli::symbol$warning), "No chart theme set in _autitus.yml, setting to",
      crayon::underline("default.\n")
    )

    echarts4r_theme <- "default"
  } else {
    echarts4r_theme <- settings[["style"]][["chart_theme"]]
  }

  options(
    echarts4r_theme = echarts4r_theme,
    database_settings = settings$database %||% "local",
    font = font
  )

  font_name <- gsub("[[:space:]]", "+", font)

  # head
  head <- tagList(
    tags$link(
      href = paste0("https://fonts.googleapis.com/css?family=", font_name),
      rel = "stylesheet"
    ),
    tags$link(
      rel = "stylesheet",
      href = paste0(
        "https://unpkg.com/bulmaswatch/", theme,"/bulmaswatch.min.css"
      )
    ),
    tags$style(
      paste0("*{font-family: '", font, "', sans-serif;}")
    )
  )

  # add google analytics if present
  if("ganalytics" %in% names(settings$tracking)){

    ga_id <- settings$tracking$ganalytics

    ga_tag <- tagList(
      tags$script(
        async = NA,
        src = paste0("https://www.googletagmanager.com/gtag/js?id={{", ga_id, "}}")
      ),
      tags$script(
        paste0(
          "window.dataLayer = window.dataLayer || [];
          function gtag(){dataLayer.push(arguments);}
          gtag('js', new Date());
          gtag('config', '{{", ga_id, "}}');"
        )
      )
    )

    head <- tagAppendChild(head, ga_tag)
  }

  ui <- bulmaPage(
    bulmaNavbar(
      color = "light",
      bulmaNavbarBrand(
        bulmaNavbarItem(
          href = "#Home",
          img(
            src = "https://auritus.io/logo.png",
            height="28"
          )
        ),
        bulmaNavbarBurger()
      ),
      bulmaNavbarEnd(
        bulmaNavbarItem(
          "Overview"
        )
      )
    ),
    tags$head(head),
    bulmaNav(
      "Home",
      homeUI("home")
    ),
    bulmaNav(
      "Overview",
      overviewUI("overview")
    )
  )

  server <- function(input, output, session){

    callModule(home, "home")
    callModule(overview, "overview")
  }

  shinyApp(ui, server)


}
