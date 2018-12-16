#' Setup Auritus
#'
#' Auritus initial configuration.
#'
#' @inheritParams crawl
#'
#' @note Read the documentation first.
#'
#' @importFrom utils browseURL
#' @import DBI
#' @import tippy
#' @import shiny
#' @import dplyr
#' @import echarts4r
#' @import shinybulma
#'
#' @export
setup_auritus <- function(days = 30L, quiet = FALSE, pages = 3L){

  config <- "_auritus.yml"

  if(!file.exists(config)){
    cat(
      crayon::red(cli::symbol$cross), " No ", crayon::underline(config), " configuration file, see ", crayon::underline("init_auritus"), ".",
      sep = ""
    )
    return(NULL)
  }

  settings <- yaml::read_yaml(config)
  settings_list <- names(settings)

  # stop if no query present
  if(!"queries" %in% settings_list){
    cat(crayon::red(cli::symbol$cross), "_auritus.yml is missing the required", crayon::underline("queries"), "parameter!\n")
    return(NULL)
  }

  # stop if no token present
  if(!"token" %in% settings_list){
    cat(crayon::red(cli::symbol$cross), "_auritus.yml is missing the required", crayon::underline("token"), "parameter!\n")

    if(interactive())
      browseURL("http://webhose.io/")

    return(NULL)
  }

  # warn if no segment
  if(!"segments" %in% settings_list)
    cat(crayon::yellow(cli::symbol$warning), "_auritus.yml does not contain", crayon::underline("segments."), "\n")

  # warn if no analytics
  if(!"tracking" %in% settings_list)
    cat(crayon::yellow(cli::symbol$warning), "_auritus.yml does not contain any web", crayon::underline("tracking"), "service.\n")

  # warn if no database
  if(!"database" %in% settings_list){
    cat(crayon::yellow(cli::symbol$warning), "_auritus.yml has no", crayon::underline("database"), "setup.\n")

    # prompt user
    db_answer <- "none"
    while (!tolower(db_answer) %in% "y" & !tolower(db_answer) %in% "n") {
      db_answer <- readline(
        cat(cli::symbol$fancy_question_mark, "Do you want to store the data locally? (y/n) ")
      )
    }

    # create directory if yes
    if(tolower(db_answer) == "y"){

      # prompt user to create data directory
      create_answer <- "none"
      while (!tolower(create_answer) %in% "y" & !tolower(create_answer) %in% "n") {
        cat(crayon::yellow(cli::symbol$warning), "This is not advised if you expect a", crayon::underline("large amount"), "of news coverage for your queries.\n")
        create_answer <- readline(
          cat(cli::symbol$fancy_question_mark, "May I create a", crayon::underline("data"), "directory to store the data? (y/n) ")
        )
      }

      # if yes prompt user
      # otherwise stop
      if(tolower(create_answer) == "y"){

        # if directory exists prompt whether to override
        if(dir.exists("data")){
          cat(crayon::red(cli::symbol$cross), "Delete the already present", crayon::underline("data"), "directory.\n")
          return(NULL)
        }

        dir <- dir.create("data")

        # if successfully created prompt initial crawl
        if(isTRUE(dir)){
          cat(crayon::green(cli::symbol$tick), "Directory successfully created!", "\n")

          initial_crawl <- "none"
          while (!tolower(initial_crawl) %in% "y" & !tolower(initial_crawl) %in% "n") {
            initial_crawl <- readline(
              cat(cli::symbol$fancy_question_mark, "Do you want to run an initial", crayon::underline(days), "day crawl of", crayon::underline(pages), "pages? (y/n) ")
            )
          }

          if(initial_crawl == "y"){
            crawled <- tryCatch(
              crawl_data(days = days, quiet = quiet, pages = pages),
              error = function(e) e
            )

            if(inherits(crawled, "error")){
              cat(crayon::red(cli::symbol$cross), "Crawl error, deleting", crayon::underline("data"), "directory.")
              unlink("data", recursive = TRUE)
            }

          } else {
            cat(crayon::yellow(cli::symbol$warning), "Not crawling data, manually run your initial crawl with", crayon::underline("crawl_data"), "before launching auritus.\n")

            return(NULL)
          }

        }

      } else { # else stop
        cat(crayon::red(cli::symbol$cross), "May not create", crayon::underline("folder"), "and no", crayon::underline("database"), "present.\n")
        return(NULL)
      }

    } else { # else stop

      cat(crayon::red(cli::symbol$cross), "May not store data locally and no", crayon::underline("database"), "present.\n")
      return(NULL)

    }
  } else {

    db <- settings$database

    if(!"type" %in% names(db)){
      cat(
        crayon::red(cli::symbol$cross), "The", crayon::underline("type"), "of database must be specified in", crayon::underline(config), "\n"
      )
      return(NULL)
    }

    if(!db$type %in% c("MySQL", "SQLite", "PostgreSQL", "MariaDB")){
      cat(
        sep = "",
        crayon::red(cli::symbol$cross), " Invalid database ", crayon::underline("type"), ", valid types are:\n",
        crayon::yellow(cli::symbol$pointer), " MySQL\n",
        crayon::yellow(cli::symbol$pointer), " SQLite\n",
        crayon::yellow(cli::symbol$pointer), " Postgres\n",
        crayon::yellow(cli::symbol$pointer), " MariaDB\n"
      )
      return(NULL)
    }

    pkg <- paste0("R", db$type)

    if(!pkg %in% rownames(installed.packages())){
      cat(
        crayon::yellow(cli::symbol$warning),
        "The required", crayon::underline(pkg), "package is not installed."
      )
      return(NULL)
    }

    # check connection
    db <- settings$database

    db$drv <- .type2drv(db$type)
    db$type <- NULL # remove type before call


    can_connect <- do.call(dbCanConnect, db)

    if(!isTRUE(can_connect)){
      cat(
        crayon::red(cli::symbol$cross), " Cannot connect to the ", crayon::underline("database"), ", check your settings in ", crayon::underline("_auritus.yml"), ".\n",
        sep = ""
      )
      return(NULL)
    } else {

      cat(
        crayon::green(cli::symbol$tick), "Can connect to database.\n"
      )

      initial_crawl <- "none"
      while (!tolower(initial_crawl) %in% "y" & !tolower(initial_crawl) %in% "n") {
        initial_crawl <- readline(
          cat(cli::symbol$fancy_question_mark, "Do you want to run an initial", crayon::underline(days), "day crawl of", crayon::underline(pages), "pages? (y/n)")
        )
      }

      if(initial_crawl == "y"){
        crawl_data(days = days, quiet = quiet, pages = pages)

      } else {
        cat(crayon::yellow(cli::symbol$warning), "Not crawling data, manually run", crayon::underline("initial-crawl"), "before launching auritus.\n")

        return(NULL)
      }

    }

  }

}
