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
#' @param since_last If \code{TRUE} crawls data since the most recently crawled article in dataset (recommended). Only applies if \code{append} is \code{TRUE} (and data already exists).
#' @param pause Time in seconds, to wait before crawling.
#' @param overwrite Set to \code{TRUE} to overwrite the database.
#' @param ... Any other parameter to pass to \link[webhoser]{wh_news}.
#'
#' @name crawl
#' @export
crawl_data <- function(days = 30L, quiet = FALSE, pages = 50L, append = TRUE,
                       apply_segments = TRUE, since_last = TRUE, pause = 5, overwrite = FALSE, ...){

  config <- "_auritus.yml"

  if(isTRUE(overwrite)){
    overwrite_answer <- "none"
    while (!tolower(overwrite_answer) %in% "y" & !tolower(overwrite_answer) %in% "n") {
      overwrite_answer <- readline(
        cat("Do you really want to", crayon::underline("overwrite"), "your datatabse? (y/n)")
      )
    }

    if(tolower(overwrite_answer) == "n")
      return(NULL)
  }

  if(!file.exists(config)){
    msg <- cat(
      crayon::red(cli::symbol$cross), "No", crayon::underline("_auritus.yml"), "configuration file."
    )
    return(msg)
  }

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

  TS <- Sys.Date() - days

  settings <- yaml::read_yaml(config)
  settings_list <- names(settings)

  # stop if no query present
  if(!"queries" %in% settings_list){
    msg <- cat(crayon::red(cli::symbol$cross), "_auritus.yml is missing the required", crayon::underline("queries"), "parameter!\n")
    return(msg)
  }

  if(length(settings$queries[[1]]) == 0){
    cat(crayon::red(cli::symbol$cross), "No queries in _auritus.yml.\n")
  }

  # stop if no token present
  if(!"token" %in% settings_list){
    msg <- cat(crayon::red(cli::symbol$cross), "_auritus.yml is missing the required", crayon::underline("token"), "parameter!\n\n",
        "Create one for", crayon::green("free"), "at", crayon::blue("webhose.io"))

    Sys.sleep(1)

    if(interactive())
      browseURL("http://webhose.io/")

    return(msg)
  }

  # initialise data
  articles <- data.frame()

  # check connection
  db <- settings$database
  db$drv <- .type2drv(db$type)
  db$type <- NULL # remove type before call

  can_connect <- do.call(DBI::dbCanConnect, db)

  if(!isTRUE(can_connect)){
    msg <- cat(
      crayon::red(cli::symbol$cross), " Cannot connect to the ", crayon::underline("database"), ", check your settings in ", crayon::underline("_auritus.yml"), ".\n",
      sep = ""
    )
    return(msg)
  }

  if(isTRUE(since_last)){
    con <- do.call(DBI::dbConnect, db)
    ts_query <- tryCatch(
      dbGetQuery(con, "SELECT MAX(crawled) FROM 'articles';"),
      error = function(e) e
    )
    dbDisconnect(con)

    if(inherits(ts_query, "numeric"))
      ts_query <- as.POSIXct(ts_query[[1]], origin = "1970-01-01 12:00")

    if(!inherits(ts_query, "error"))
      TS <- as.numeric(ts_query)
  }

  if("segments" %in% settings_list){

    segments <- .segments2df(settings)

    cat(crayon::yellow(cli::symbol$warning), "The following segments will be applied:\n\n")
    for(i in 1:nrow(segments)){
      cat(
        cli::symbol$pointer, segments$name[i], "with regex", crayon::underline(segments$regex[i]), "applies to query with id", segments$query[i], "\n"
      )
    }

    # give user a change to stop process
    cat(
      "\n",
      crayon::yellow(cli::symbol$warning), " Hit ", crayon::underline("CTRL + C"), ", or ", crayon::underline("ESC"), " to cancel.\n",
      sep = ""
    )
    Sys.sleep(pause)
  }

  cat(
    cli::rule(left = crayon::green("Crawling webhose.io"))
  )
  cat("\n")

  # loop over queries
  for(i in 1:length(settings$queries)){

    q <- as.data.frame(settings$queries[[1]][[i]])

    if(is.null(q$name))
      q$name <- q$id

    query <- webhoser::wh_news(
      settings$token,
      q$search,
      ts = as.numeric(TS),
      quiet = quiet,
      highlight = TRUE
    ) %>%
      webhoser::wh_paginate(pages) %>%
      webhoser::wh_collect(flatten = TRUE) %>%
      mutate(
        query_id = q$id,
        query_name = q$name
      ) %>% 
      webhoserx::whe_lexdiv() %>% 
      webhoserx::whe_paragraphs() %>% 
      webhoserx::whe_pre_gram(flatten = TRUE) %>% 
      webhoserx::whe_quotes() %>% 
      webhoserx::whe_sentences() %>% 
      webhoserx::whe_topmedia() %>% 
      webhoserx::whe_words() %>% 
      webhoserx::whe_sentiment()

    # dedupe
    query <- query %>%
      filter(!query$uuid %in% articles$uuid)

    query <- .preproc_crawl(query)

    if("segments" %in% settings_list){
      query <- .segment(query, segments, q$id[i])
    }

    articles <- plyr::rbind.fill(articles, query)

  }

  # rename
  names(articles) <- gsub("\\.", "_", names(articles))


  # dates as strings if SQLite
  if(settings$database$type == "SQLite"){
    articles <- articles %>%
      mutate(
        published = as.character(published),
        thread_published = as.character(thread_published),
        crawled = as.character(crawled)
      )
  }

  con <- do.call(DBI::dbConnect, db)
  dbWriteTable(con, "articles", articles, append = append, overwrite = overwrite)
  dbDisconnect(con)

  cat(crayon::green(cli::symbol$tick), nrow(articles), "articles collected.")

}
