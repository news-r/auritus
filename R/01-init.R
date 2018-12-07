#' Setup Auritus
#'
#' Auritus initial configuration.
#'
#' @inheritParams crawl
#'
#' @note Read the documentation first.
#'
#' @importFrom utils browseURL
#' @import shiny
#' @import dplyr
#' @export
setup_auritus <- function(days = 30L, quiet = FALSE, pages = 3L){

  settings <- yaml::read_yaml("_auritus.yml")
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
  if(!"analytics" %in% settings_list)
    cat(crayon::yellow(cli::symbol$warning), "_auritus.yml does not contain any", crayon::underline("analytics"), "tracking service.\n")

  # warn if no database
  if(!"database" %in% settings_list){
    cat(crayon::yellow(cli::symbol$warning), "_auritus.yml has no", crayon::underline("database"), "setup.\n")

    # prompt user
    db_answer <- "none"
    while (!tolower(db_answer) %in% "y" & !tolower(db_answer) %in% "n") {
      db_answer <- readline("Do you want to store the data locally? (y/n)")
    }

    # create directory if yes
    if(tolower(db_answer) == "y"){

      # prompt user to create data directory
      create_answer <- "none"
      while (!tolower(create_answer) %in% "y" & !tolower(create_answer) %in% "n") {
        cat(crayon::yellow(cli::symbol$warning), "This is not advised if you expect a", crayon::underline("large amount"), "of news coverage for your queries.\n")
        create_answer <- readline("May I create a 'data' directory to store the data? (y/n)")
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
              paste("Do you want to run an initial", days, "day crawl of", pages, "pages? (y/n)")
            )
          }

          if(initial_crawl == "y"){
            crawl_auritus(days = days, quiet = quiet, pages = pages)
          } else {
            cat(crayon::yellow(cli::symbol$warning), "Not crawling data, manually run", crayon::underline("initial-crawl"), "before launching auritus.\n")

            # delete directory if fail
            rm_dir <-unlink("data", recursive = TRUE)

            if(rm_dir == 0)
              cat(crayon::red(cli::symbol$cross), "Could not delete 'data' directory.\n")
            else
              cat(crayon::green(cli::symbol$tick), "Successfully deleted 'data' directory!\n")

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
  }

}
