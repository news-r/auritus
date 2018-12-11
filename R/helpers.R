globalVariables("query_id")

#' Load data
#' 
#' Load crawled data.
#' 
#' @examples 
#' \dontrun{data <- load_data()}
#' 
#' @export
load_data <- function(){
  
  ex <- file.exists("data/articles.RData")
  
  out <- NULL
  if(isTRUE(ex))
    out <- get(load("data/articles.RData"))
  else {
    cat(crayon::red(cli::symbol$cross), "No data crawled, see", crayon::underline("setup_auritus")) 
  }
  
  return(out)
}

#' Segment
#' 
#' Re-segments all the data.
#' 
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
  
  articles <- load_data()
  
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
  
  save(segmented, file = "data/articles.RData")
  
}