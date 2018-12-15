#' Parse dates
#' @param x A character vector.
#' @note Someone take care of this I suck with date/time stuff.
.parse_dates <- function(x){
  x <- gsub(".000+.*", "", x)
  x <- gsub("T", " ", x)
  x <- as.POSIXct(x, "%Y-%m-%d %H:%M:%S")
  return(x)
}

#' Preprocess results of webhose.io
#' @param dat Data as returned by webhoser.
.preproc_crawl <- function(dat){
  dat$published <- webhoser.extension::whe_date(dat$published)
  dat$thread.published <- webhoser.extension::whe_date(dat$thread.published)
  dat$crawled <- webhoser.extension::whe_date(dat$crawled)
  return(dat)
}

#' Convertes segements list into a data.frame
#' @param settings raw \code{yml} list
.segments2df <- function(settings){

  segments <- suppressWarnings(
    purrr::map_df(settings$segments, as.data.frame)
  )

  names(segments) <- gsub("segment\\.", "", names(segments))
  return(segments)
}

#' Segment to valid column name
#' @param x A segment name
.segment2name <- function(x){
  gsub("[[:space:]]|[[:cntrl:]]|[[:blank:]]|[[:punct:]]", "", x)
}

#' Segment
#' @param data data as returned by crawl.
#' @param segments segments data.frame.
#' @param id query id.
.segment <- function(data, segments, id){

  relevant_segments <- segments %>%
    filter(segments$quer == id)

  if(nrow(relevant_segments)){

    for(i in 1:nrow(relevant_segments)){

      nm <- .segment2name(relevant_segments$name[i])

      nm_title <- paste0(nm, "_title_segment")
      nm_text <- paste0(nm, "_text_segment")
      nm_1p <- paste0(nm, "_1p_segment")

      data <- data %>%
        webhoser.extension::whe_search_1p(relevant_segments$regex[[i]], nm_1p) %>%
        webhoser.extension::whe_search(relevant_segments$regex[[i]], nm_title, "thread.title") %>%
        webhoser.extension::whe_search(relevant_segments$regex[[i]], nm_text, "text")
    }

  }

  return(data)
}

"%||%" <- function(x, y) {
  if (length(x) > 0 || !is.null(x)) x else y
}

#' Get echarts4r theme
.get_theme <- function(){
  getOption("echarts4r_theme")
}

#' Get database setup
.get_db <- function(){
  getOption("database_settings")
}

.db_con <- function(db){
  db$drv <- .type2drv(db$type)
  db$type <- NULL # remove type before call
  return(db)
}

#' Convert type to driver
#' @param x a type as specified in \code{_auritus.yml}
.type2drv <- function(x){

  DRV <- NULL

  if(x == "Postgres")
    DRV <- RPostgres::Postgres()
  else if (x == "MySQL")
    DRV <- RMySQL::MySQL()
  else if(x == "MariaDB")
    DRV <- RMariaDB::MariaDB()
  else if(x == "SQLite")
    DRV <- RSQLite::SQLite()

  return(DRV)
}
