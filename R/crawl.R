#' Initial Crawl
#'
#' Make the initial data crawl.
#'
#' @param days Number of days from today (\code{Sys.Date()}) to craw articles.
#' @param quiet Whether to print helpful messages in the console (default recommended), passed to \link[webhoser]{wh_news}.
#' @param pages Number of pages of data to crawl, defaults to \code{3L} to crawl all 3 pages of data, set to \code{Inf} (infinite) to collect all data available under your plan (at your own rirks).
#'
#' @name initial-crawl
#' @export
initial_webhose_crawl <- function(days = 30L, quiet = FALSE, pages = 3L){

  if(days > 30L){
    cat(crayon::yellow(cli::symbol$warning), "Maximum 30 days backcrawl available, setting it to", crayon::yellow("30."))
    days <- 30L
  }

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

  file <- "data/articles.RData"
  if(file.exists(file)){
    cat(crayon::red(cli::symbol$cross), "cannot save data file already exists")
  }

  # loop over queries
  data <- data.frame()
  for(i in 1:length()){

    query <- webhoser::wh_news(
      settings$token,
      settings$queries,
      ts = Sys.Date() - days,
      quiet = quiet
    ) %>%
      webhoser::wh_paginate(pages)

    data <- rbind.data.frame(data, query)

  }

  # save
  save(data, file = file)

  cat(crayon::green(cli::symbol$tick), nrow(data), "articles collected.")

}
