#' Create HTML page (iframe) with all maps, barplots, and piecharts
#'
#' @param year The election year in the form of YYYY.
#'
#' @return No return values, writes HTML content in `content/core.html`.
#'   
#' @export
#'
#' @examples
#' \dontrun{
#' create_html_content(2022)
#' }

create_html_content <- function(year) {
  
  check_year(year)
  
  
  ## Import elections results ----
  
  filename_1 <- here::here("data", "derived-data", paste0("election_results_", 
                                                          year, "_round_1.rds"))
  
  if (file.exists(filename_1)) {
    results_round1 <- readRDS(filename_1)
    results_round1 <- results_round1[order(results_round1@data$"code"), ]
  }
  
  
  filename_2 <- here::here("data", "derived-data", paste0("election_results_", 
                                                          year, "_round_2.rds"))
  
  if (file.exists(filename_2)) {
    results_round2 <- readRDS(filename_2)
    results_round2 <- results_round2[order(results_round2@data$"code"), ]
  }
  
  if (file.exists(filename_2)) each <- 2 else each <- 1
  
  
  ## Prepare popup graph (bar plots)----
  
  if (file.exists(filename_1)) {
    
    bars <- list.files(here::here("content", "barplots"), pattern = "html$")
    
    bars_pop <- readRDS(here::here("data", "derived-data", 
                                   "popup_barplots.rds"))
    
    for (i in 1:length(bars_pop)) {
      bars_pop[[i]] <- gsub("src='.*graphs/tmp_[0-9]{1,}\\.html'", 
                            paste0("src='barplots/barplot_", names(bars_pop)[i], 
                                   ".html'"), bars_pop[[i]])
    }
    
    names(bars_pop) <- NULL
    
    bars_pop <- lapply(bars_pop, function(x) {
      gsub("leaflet-popup-content \\{", 
           "leaflet-popup-content \\{ \twidth: 755px !important;", x)
    })
  }
  
  
  ## Prepare popup graph (pie charts)----
  
  if (file.exists(filename_2)) {
    
    pies <- list.files(here::here("content", "piecharts"), pattern = "html$")
    
    pies_pop <- readRDS(here::here("data", "derived-data", 
                                   "popup_piecharts.rds"))
  
    for (i in 1:length(pies_pop)) {
      pies_pop[[i]] <- gsub("src='.*graphs/tmp_[0-9]{1,}\\.html'", 
                            paste0("src='piecharts/piechart_", names(pies_pop)[i], 
                                   ".html'"), pies_pop[[i]])
    }
    
    names(pies_pop) <- NULL
    
    pies_pop <- lapply(pies_pop, function(x) {
      gsub("leaflet-popup-content \\{", 
           "leaflet-popup-content \\{ \twidth: 755px !important;", x)
    })
  }
  
  
  ## Mapview map ----
  
  map <- leaflet::leaflet() %>%
    
    leaflet::setView(lng = 2.25, lat = 46.50, zoom = 5.85)
    
  
  if (file.exists(filename_2)) {
    
    map <- map %>%
      
      ### Votes 2nd round ----
      
      leafem::garnishMap(
        leaflet::addPolygons, data = results_round2, 
        group = 'Votes cast (2nd round)', weight = .5, smoothFactor = 0.5, 
        opacity = 1, fillOpacity = 1, color = "#212121", 
        fillColor = results_round2@data$"first_color",
        highlightOptions = leaflet::highlightOptions(color = "#212121", 
                                                     weight = 3, 
                                                     bringToFront = TRUE),
        label = mapply(function(x, y, z) {
          htmltools::HTML(sprintf("%s<br /> %s : %s", htmltools::htmlEscape(x), 
                                  htmltools::htmlEscape(y), 
                                  htmltools::htmlEscape(z))) 
        }, 
        toupper(results_round2@data$'departement'), 
        results_round2@data$'first_name', 
        paste0(as.character(format(results_round2@data$"first_value")), "%"),
        SIMPLIFY = FALSE, USE.NAMES = FALSE), popup = pies_pop)
    
    map <- map %>% 
      
      ### Abstention 2nd round ----
    
      leafem::garnishMap(
        leaflet::addPolygons, data = results_round2,
        group = 'Abstention (2nd round)', weight = .5, smoothFactor = 0.5, 
        opacity = 1, fillOpacity = 1, color = "#212121", 
        fillColor = results_round2@data$"abstention_color",
        highlightOptions = leaflet::highlightOptions(color = "#212121", 
                                                     weight = 3, 
                                                     bringToFront = TRUE),
        label = mapply(function(x, y) {
          htmltools::HTML(sprintf("%s<br />Abstention : %s", 
                                  htmltools::htmlEscape(x), 
                                  htmltools::htmlEscape(y))) 
          },
          toupper(results_round2@data$"departement"), 
          paste0(as.character(format(results_round2@data$"abstention")), "%"),
          SIMPLIFY = FALSE, USE.NAMES = FALSE))
  }
  
  if (file.exists(filename_1)) {
    
    map <- map %>%
      
      ### Votes 1st round ----
    
    leafem::garnishMap(
      leaflet::addPolygons, data = results_round1, 
      group = 'Votes cast (1st round)', weight = .5, smoothFactor = 0.5, 
      opacity = 1, fillOpacity = 1, color = "#212121", 
      fillColor = results_round1@data$"first_color",
      highlightOptions = leaflet::highlightOptions(color = "#212121", 
                                                   weight = 3, 
                                                   bringToFront = TRUE),
      label = mapply(function(x, y, z) {
        htmltools::HTML(sprintf("%s<br /> %s : %s", htmltools::htmlEscape(x), 
                                htmltools::htmlEscape(y), 
                                htmltools::htmlEscape(z))) 
      }, 
      toupper(results_round1@data$'departement'), 
      results_round1@data$'first_name', 
      paste0(as.character(format(results_round1@data$"first_value")), "%"),
      SIMPLIFY = FALSE, USE.NAMES = FALSE), popup = bars_pop)
    
    map <- map %>% 
      
      ### Abstention 2nd round ----
    
    leafem::garnishMap(
      leaflet::addPolygons, data = results_round1,
      group = 'Abstention (1st round)', weight = .5, smoothFactor = 0.5, 
      opacity = 1, fillOpacity = 1, color = "#212121", 
      fillColor = results_round1@data$"abstention_color",
      highlightOptions = leaflet::highlightOptions(color = "#212121", 
                                                   weight = 3, 
                                                   bringToFront = TRUE),
      label = mapply(function(x, y) {
        htmltools::HTML(sprintf("%s<br />Abstention : %s", 
                                htmltools::htmlEscape(x), 
                                htmltools::htmlEscape(y))) 
      },
      toupper(results_round1@data$"departement"), 
      paste0(as.character(format(results_round1@data$"abstention")), "%"),
      SIMPLIFY = FALSE, USE.NAMES = FALSE))
  }
  
  map <- map %>% 
  
    leaflet::addEasyButton(
      leaflet::easyButton(
        icon = 'fa-globe', title = 'Zoom initial',
        onClick = leaflet::JS('function(btn){ location.reload(); }')))
  
  if (file.exists((filename_1)) || file.exists((filename_2))) {
    
    map <- map %>% 
      
      leaflet::addLayersControl(
        baseGroups = paste0(c('Votes cast', 'Abstention'), 
                            rep(c(' (2nd round)', ' (1st round)'), 
                                each = each)),
        options = leaflet::layersControlOptions(collapsed = TRUE),
        position = 'topleft')
  }
  
  
  ## Export core html page ----
  
  htmlwidgets::saveWidget(widget = map, file = here::here("content", 
                                                          "core.html"), 
                          selfcontained = FALSE)
  
  
  ## Change background color of leaflet map ----
  
  html <- readLines(here::here("content", "core.html"))
  
  html[grep('<div id=\"htmlwidget-', html)] <- 
    gsub('width:100%;', 
         'width:100%;background-color:#212121;', 
         html[grep('<div id=\"htmlwidget-', html)])
  
  cat(paste0(html, collapse = "\n"), file = here::here("content", "core.html"))
  
  invisible(NULL)
}
