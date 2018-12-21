#' Initialise auritus
#'
#' @export
init_auritus <- function(){

  config <- "_auritus.yml"

  if(file.exists(config)){
    cat(
      crayon::red(cli::symbol$cross), " The configuration file,", crayon::underline(config), " already exists.\n",
      sep = ""
    )


  } else {
    file.copy(
      system.file("templates/auritus.yml", package = "auritus"),
      "_auritus.yml"
    )

    cat(
      crayon::green(cli::symbol$tick), "Copied", crayon::underline(config)
    )
  }

}
