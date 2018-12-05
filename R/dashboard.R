#' Setup Auritus
#'
#' Auritus initial configuration.
#'
#' @note Read the documentation first.
#'
#' @export
setup_auritus <- function(){

  settings <- yaml::read_yaml("_auritus.yml")
  settings_list <- names(settings)

  # stop if no query present
  if(!"queries" %in% settings_list){
    cat(crayon::red(cli::symbol$cross), "_auritus.yml is missing the", crayon::underline("queries"), "parameter!\n")
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
    cat(crayon::yellow(cli::symbol$warning), "_auritus.yml has not", crayon::underline("database"), "setup.\n")

    # prompt user
    db_answer <- "none"
    while (!tolower(db_answer) %in% "y" & !tolower(db_answer) %in% "n") {
      db_answer <- readline("Do you want to store the data locally? (y/n)")
    }

    # create directory if yes
    if(tolower(db_answer) == "y"){

      # prompt user
      create_answer <- "none"
      while (!tolower(create_answer) %in% "y" & !tolower(create_answer) %in% "n") {
        create_answer <- readline("May I create a 'data' directory to store the data? (y/n)")
      }

      # if yes prompt user
      # otherwise stop
      if(tolower(create_answer) == "y")
        dir.create("data")
      else {
        cat(crayon::red(cli::symbol$cross), "May not create", crayon::underline("folder"), "and no", crayon::underline("database"), "present.\n")
        return(NULL)
      }

    } else{

      cat(crayon::red(cli::symbol$cross), "May not store data locally and no", crayon::underline("database"), "present.\n")
      return(NULL)

    }
  }

}
