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

  segments <- suppressWarnings(
    purrr::map_df(settings$segments, as.data.frame)
  )

  names(segments) <- gsub("segment\\.", "", names(segments))
  return(segments)
}

.segment2name <- function(x){
  gsub("[[:space:]]|[[:cntrl:]]|[[:blank:]]|[[:punct:]]", "", x)
}

.grp <-function(x, y){
  resp <- grep(x, y)
  if(!length(resp))
    rep(0,length(y))
  else
    ifelse(is.na(resp), 0, resp)
}

.segment <- function(data, segments, id){

  relevant_segments <- segments %>%
    filter(segments$quer == id)

  if(nrow(relevant_segments)){

    for(i in 1:nrow(relevant_segments)){

      nm <- .segment2name(relevant_segments$name[i])

      nm_title <- paste0(nm, "_title")
      nm_text <- paste0(nm, "_text")

      data[[nm_title]] <- .grp(relevant_segments$regex[[i]], data$thread.title)
      data[[nm_text]] <- .grp(relevant_segments$regex[[i]], data$text)
    }

  }

  return(data)
}
