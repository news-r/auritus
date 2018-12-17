globalVariables("query_id")

#' @title Load data
#'
#' @description Load data collected with \code{\link{crawl_data}} or \code{\link{setup_auritus}}.
#'
#' @examples
#' \dontrun{data <- get_articles()}
#'
#' @name load
#' @export
get_articles <- function(){

  ex <- file.exists("data/articles.RData")

  out <- NULL
  if(isTRUE(ex))
    out <- get(load("data/articles.RData"))
  else {
    cat(crayon::red(cli::symbol$cross), "No data, see", crayon::underline("setup_auritus"), "\n")
  }

  return(out)
}

#' @rdname load
#' @export
get_text <- function(){

  ex <- file.exists("data/text.RData")

  out <- NULL
  if(isTRUE(ex))
    out <- get(load("data/text.RData"))
  else {
    cat(crayon::red(cli::symbol$cross), "No data crawled, see", crayon::underline("setup_auritus"))
  }

  return(out)
}

#' @rdname load
#' @export
get_data <- function(){

  get_articles() %>%
    left_join(
      get_text(),
      by = "uuid"
    )

}

#' @title Segment
#'
#' @description Re-segments all the data using segments in \code{_auritus.yml}.
#'
#' @name segment
#' @export
segment_data <- function(){

  config <- "_auritus.yml"

  if(!file.exists(config)){

    cat(
      crayon::red(cli::symbol$cross), "No", crayon::underline("_auritus.yml"), "configuration file."
    )
    return(NULL)
  }

  db <- "data/articles.RData"

  articles <- get_articles()
  text <- get_text()
  articles <- articles %>%
    left_join(text, by = "uuid")

  rm(text)

  settings <- yaml::read_yaml(config)
  settings_list <- names(settings)

  # prompt user
  answer <- "none"
  while (!tolower(answer) %in% "y" & !tolower(answer) %in% "n") {
    answer <- readline("Are you sure you want re-segment all the data? (y/n) ")
  }

  if(tolower(answer) == "n"){
    return(NULL)
  }

  if(!"segments" %in% settings_list){
    cat(
      crayon::red(cli::symbol$cross), "No segments in", crayon::underline("_auritus.yml")
    )
    return(NULL)
  }

  segments <- .segments2df(settings)

  cat(crayon::yellow(cli::symbol$warning), "The following segments will be applied:\n")
  for(i in 1:nrow(segments)){
    cat(
      cli::symbol$pointer, segments$name[i], "with regex", crayon::underline(segments$regex[i]), "applies to query", segments$query[i], "\n"
    )
  }

  segment_cols <- grepl("_segment$", names(articles))
  articles <- articles[, !segment_cols]

  ids <- unique(articles$query_id)

  segments <- .segments2df(settings)

  segmented <- data.frame()
  for(id in ids){
    art <- filter(articles, query_id == id)
    art <- .segment(art, segments, id)
    segmented <- rbind.data.frame(segmented, art)
  }

  text <- tibble(
    uuid = segmented$uuid,
    text = segmented$text
  )

  segmented$text <- NULL

  articles <- segmented

  save(articles, file = "data/articles.RData")
  save(text, file = "data/text.RData")

}
