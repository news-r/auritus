#' Setup Auritus
#'
#' Auritus initial configuration.
#'
#' @inheritParams crawl
#'
#' @note Read the documentation first.
#'
#' @importFrom utils browseURL installed.packages
#' @import DBI
#' @import tippy
#' @import shiny
#' @import dplyr
#' @import echarts4r
#' @import sigmajs
#'
#' @export
setup_auritus <- function(days = 30L, quiet = FALSE, pages = 50L){

  config <- "_auritus.yml"

  if(!file.exists(config)){
    msg <- cat(
      crayon::red(cli::symbol$cross), " No ", crayon::underline(config), " configuration file, see ", crayon::underline("init_auritus"), ".",
      sep = ""
    )
    return(msg)
  }

  settings <- yaml::read_yaml(config)
  settings_list <- names(settings)

  # stop if no query present
  if(!"queries" %in% settings_list){
    msg <- cat(crayon::red(cli::symbol$cross), "_auritus.yml is missing the required", crayon::underline("queries"), "parameter!\n")
    return(msg)
  }

  # stop if no token present
  if(!"token" %in% settings_list){
    msg <- cat(crayon::red(cli::symbol$cross), "_auritus.yml is missing the required", crayon::underline("token"), "parameter!\n")

    if(interactive())
      browseURL("http://webhose.io/")

    return(msg)
  }

  if(is.null(settings$token)){
    cat(
      crayon::red(cli::symbol$cross), " No token specified in ", crayon::underline("_auritus.yml"), ".\n", sep = ""
    )
  }

  # warn if no segment
  if(!"segments" %in% settings_list)
    cat(crayon::yellow(cli::symbol$warning), "_auritus.yml does not contain", crayon::underline("segments."), "\n")

  # warn if no analytics
  if(!"tracking" %in% settings_list)
    cat(crayon::yellow(cli::symbol$warning), "_auritus.yml does not contain any web", crayon::underline("tracking"), "service.\n")

  # warn if no database
  if(!"database" %in% settings_list){
    msg <- cat(crayon::red(cli::symbol$cross), "_auritus.yml has no ", crayon::underline("database"), " specified.\n", sep = "")
    return(msg)
  } else {

    db <- settings$database

    if(!"type" %in% names(db)){
      msg <- cat(
        crayon::red(cli::symbol$cross), "The", crayon::underline("type"), "of database must be specified in", crayon::underline(config), "\n"
      )
      return(msg)
    }

    if(!db$type %in% c("MySQL", "SQLite", "PostgreSQL", "MariaDB")){
      msg <- cat(
        sep = "",
        crayon::red(cli::symbol$cross), " Invalid database ", crayon::underline("type"), ", valid types are:\n",
        crayon::yellow(cli::symbol$pointer), " MySQL\n",
        crayon::yellow(cli::symbol$pointer), " SQLite\n",
        crayon::yellow(cli::symbol$pointer), " Postgres\n",
        crayon::yellow(cli::symbol$pointer), " MariaDB\n"
      )
      return(msg)
    }

    pkg <- paste0("R", db$type)

    if(!pkg %in% rownames(installed.packages())){
      msg <- cat(
        crayon::yellow(cli::symbol$warning),
        "The required", crayon::underline(pkg), "package is not installed."
      )
      return(msg)
    }

    # check connection
    db <- settings$database

    db$drv <- .type2drv(db$type)
    db$type <- NULL # remove type before call


    can_connect <- do.call(dbCanConnect, db)

    if(!isTRUE(can_connect)){
      msg <- cat(
        crayon::red(cli::symbol$cross), " Cannot connect to the ", crayon::underline("database"), ", check your settings in ", crayon::underline("_auritus.yml"), ".\n",
        sep = ""
      )
      return(msg)
    } else {

      cat(
        crayon::green(cli::symbol$tick), "Can connect to database.\n"
      )

      initial_crawl <- "none"
      while (!tolower(initial_crawl) %in% "y" & !tolower(initial_crawl) %in% "n") {
        initial_crawl <- readline(
          paste(cli::symbol$fancy_question_mark, "Do you want to run an initial", crayon::underline(days), "day crawl of", crayon::underline(pages), "pages? (y/n)")
        )
      }

      if(initial_crawl == "y"){
        crawl_data(days = days, quiet = quiet, pages = pages)
      } else {
        msg <- cat(crayon::yellow(cli::symbol$warning), "Not crawling data, manually run", crayon::underline("initial-crawl"), "before launching auritus.\n")
        return(msg)
      }

    }

  }

}
