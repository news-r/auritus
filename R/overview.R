#' Overview
#' @param id namespace id.
overviewUI <- function(id){

  ns <- NS(id)

  tagList(
    bulmaSection(
      bulmaContainer(
        bulmaColumns(
          bulmaColumn(
            bulmaCard(
              bulmaCardHeader(
                bulmaCardHeaderTitle(
                  "Trend", id = "trend-headline",
                  tippy_this("trend-headline", "Number of articles per day.")
                )
              ),
              bulmaCardContent(
                echarts4rOutput(ns("trend"))
              )
            )
          ),
          bulmaColumn(
            bulmaCard(
              bulmaCardHeader(
                bulmaCardHeaderTitle(
                  "Geographic Spread", id = "map-headline",
                  tippy_this("map-headline", "Number of articles per country.")
                )
              ),
              bulmaCardContent(
                echarts4rOutput(ns("map"))
              )
            )
          )
        )
      )
    )
  )
}

overview <- function(input, output, session){

  # common
  DB <- .get_db()
  THEME <- .get_theme()
  hide <- list(show = FALSE)
  
  country <- reactive({
    
    if(length(DB) == 1){
      data <- load_data()
      
      country <- data %>% 
        count(thread.country, sort = T) %>% 
        filter(thread.country != "") %>% 
        e_country_names(thread.country, thread.country)
    }
    
    return(country)
    
  })

  trend_data <- reactive({

    if(length(DB) == 1){

      res <- load_data() %>%
        mutate(published = as.Date(published))

    } else {
      args <- .db_con(DB)
      con <- do.call(dbConnect, args)
      dates <- dbGetQuery(con, "SELECT published FROM 'articles';")
      dbDisconnect(con)

      res <- dates %>%
        mutate(
          published = as.POSIXct(published, origin = "1970-01-01"),
          published = as.Date(published)
        )
    }

    res %>%
      count(published)

  })

  output$trend <- renderEcharts4r({

    trend_data() %>%
      e_charts(published) %>%
      e_area(
        n,
        name = "articles",
        legend = FALSE,
        smooth = TRUE,
        smoothMonotone = "x"
      ) %>%
      e_tooltip("axis") %>%
      e_x_axis(name = "Date", splitLine = hide) %>%
      e_y_axis(name = "Articles", splitLine = hide) %>%
      e_theme(THEME) %>%
      e_text_style(fontFamily = .font())

  })
  
  output$map <- renderEcharts4r({
    
    country() %>% 
      e_charts(thread.country) %>% 
      e_map_3d(n, name = "Articles by countries") %>% 
      e_visual_map(n, orient = "horizontal", bottom = "5%", right = "5%") %>% 
      e_theme(THEME) %>%
      e_text_style(fontFamily = .font())
  })

}
