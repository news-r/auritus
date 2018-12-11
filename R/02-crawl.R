#' Initial Crawl
#'
#' Make the initial data crawl.
#'
#' @param days Number of days from today (\code{Sys.Date()}) to craw articles.
#' @param quiet Whether to print helpful messages in the console (default recommended), passed to \link[webhoser]{wh_news}.
#' @param pages Number of pages of data to crawl, defaults to \code{3L} to crawl all 3 pages of data, set to \code{Inf} (infinite) to collect all data available under your plan (at your own rirks).
#' 1 page of results = 1 query = 100 articles (you have 1,000 free queries per month).
#' @param append If data has been previously crawled and stored, whether to append new results to it (set to \code{TRUE}).
#' @param apply_segments If TRUE applies the \code{segments} from \code{_auritus.yml}.
#' @param since_last If \code{TRUE} crawls data since the most recently published article in dataset (recommended). Only applies if \code{append} is \code{TRUE} (and data already exists).
#' @param ... Any other parameter to pass to \link[webhoser]{wh_news}.
#' 
#' @name crawl
#' @export
crawl_auritus <- function(days = 30L, quiet = FALSE, pages = 3L, append = FALSE, 
                          apply_segments = TRUE, since_last = TRUE, ...){
  
  config <- "_auritus.yml"
  
  if(!file.exists(config)){
    cat(
      crayon::red(cli::symbol$cross), "No", crayon::underline("_auritus.yml"), "configuration file."
    )
  }

  TS <- Sys.Date() - days

  if(pages <= 1){
    cat(crayon::yellow(cli::symbol$warning), "Minimum 2 pages of crawl, setting the pages parameter to", crayon::yellow("2."), "\n")
    pages <- 2
  }

  # minus one
  pages <- pages - 1L

  if(days > 30L){
    cat(crayon::yellow(cli::symbol$warning), "Maximum 30 days backcrawl available, setting it to", crayon::yellow("30."), "\n")
    days <- 30L
  }

  settings <- yaml::read_yaml(config)
  settings_list <- names(settings)

  # stop if no query present
  if(!"queries" %in% settings_list){
    cat(crayon::red(cli::symbol$cross), "_auritus.yml is missing the required", crayon::underline("queries"), "parameter!\n")
    return(NULL)
  }

  if(length(settings$queries[[1]]) == 0){
    cat(crayon::red(cli::symbol$cross), "No queries in _auritus.yml.\n")
  }

  # stop if no token present
  if(!"token" %in% settings_list){
    cat(crayon::red(cli::symbol$cross), "_auritus.yml is missing the required", crayon::underline("token"), "parameter!\n\n",
        "Create one for", crayon::green("free"), "at", crayon::blue("webhoser.io"))

    if(interactive())
      browseURL("http://webhose.io/")

    return(NULL)
  }

  # initialise data
  articles <- data.frame()

  dir <- "data"
  if(!dir.exists(dir)){
    cat(crayon::red(cli::symbol$cross), "No data folder present, see", crayon::underline("setup_auritus"), ".\n")
    return(NULL)
  }

  # check append __before__ running query to avoir waste
  fl <- paste0(dir, "/articles.RData")
  if(file.exists(fl)){

    if(!isTRUE(append)) {
      cat(crayon::red(cli::symbol$cross), "Data file already exists, set the", crayon::underline("append"), "parameter to TRUE to add data to it.\n")
      return(NULL)
    } else {
      cat(crayon::green(cli::symbol$tick), "Data file found, data will be appended.\n")
      articles <- get(load(fl))

      if(isTRUE(since_last)){
        days <- max(articles$thread.published)
      }

    }

  }

  if("segments" %in% settings_list){

    segments <- .segments2df(settings)

    cat(crayon::yellow(cli::symbol$warning), "The following segments will be applied:\n")
    for(i in 1:nrow(segments)){
      cat(
        cli::symbol$pointer, segments$name[i], "with regex", crayon::underline(segments$regex[i]), "applies to query", segments$query[i], "\n"
      )
    }

    # give user a change to stop process
    Sys.sleep(3)
  }

  cat(
    cli::rule(left = crayon::green("Crawling webhose.io"))
  )

  # loop over queries
  for(i in 1:length(settings$queries)){

    q <- as.data.frame(settings$queries[[1]][[i]])

    query <- webhoser::wh_news(
      settings$token,
      q$search,
      ts = TS,
      quiet = quiet,
      highlight = TRUE
    ) %>%
      webhoser::wh_paginate(pages) %>%
      webhoser::wh_collect() %>%
      mutate(
        query_id = q$id
      )

    # dedupe
    query <- query %>%
      filter(!query$uuid %in% articles$uuid)

    query <- .preproc_crawl(query)

    if("segments" %in% settings_list){
      query <- .segment(query, segments, q$id[i])
    }

    articles <- rbind.data.frame(articles, query)

  }

  # save
  save(articles, file = fl)

  cat(crayon::green(cli::symbol$tick), nrow(articles), "articles in the bank.")

}
