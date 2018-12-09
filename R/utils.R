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
  dat$thread.published <- .parse_dates(dat$thread.published)
  dat$crawled <- .parse_dates(dat$crawled)
  return(dat)
}

#' Convertes segements list into a data.frame
#' @param settings raw \code{yml} list
.segments2df <- function(settings){
  suppressWarnings(
    settings$segments %>%
      purrr::map(as.data.frame) %>%
      purrr::map(function(x){
        names(x) <- gsub("segment.\\.", "", names(x))
        x
      }) %>%
      purrr::map_df(dplyr::bind_rows)
  )
}

.segment <- function(data, segments, search){
  data
}
