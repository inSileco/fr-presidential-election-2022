#' Create HTML Piecharts (Tooltip) pour second round of voting
#'
#' @param year The election year in the form of YYYY.
#'
#' @return No return values. Piecharts (HTML code) are exported in 
#'   `content/piecharts/`.
#'   
#' @export
#'
#' @examples
#' \dontrun{
#' make_tooltip_piechart(2022)
#' }

make_tooltip_piechart <- function(year) {
  
  check_year(year)
  
  dir.create(here::here("content", "piecharts", "libs"), recursive = TRUE, 
             showWarnings = FALSE)
  
  
  ## Get candidates list ----
  
  infos <- read_candidates_info(year)
  
  candidates <- infos$"name"
  candidates <- candidates[!(candidates %in% c("blancs", "nuls", "abstention"))]
  
  
  ## Import spatial results ----
  
  shp <- readRDS(here::here("data", "derived-data", 
                            paste0("election_results_", year, "_round_2.rds")))
  
  shp <- shp[order(shp@data$"code"), ]
  
  candidates <- candidates[candidates %in% colnames(shp@data)]
  
  
  ttips <- list()
  
  for (j in 1:length(shp)) {
    
    ## Prepare data for barplot ----
    
    vals <- t(shp@data[j, candidates])[ , 1]
    
    dat <- data.frame("name"  = names(vals), 
                      "value" = round(100 * vals / sum(vals), 2))
    
    dat <- merge(dat, infos, by = 'name', all.x = TRUE, all.y = FALSE)
    dat <- dat[order(dat[ , 'value'], decreasing = TRUE), ]
    
    dat$group <- seq(1, nrow(dat))
    
    ## Make Highcharter barplot ----
    
    ttips[[j]] <- highcharter::highchart() %>%
      
      highcharter::hc_add_series_labels_values(
        labels = dat$"label", values = dat$"value", colors = dat$"color", 
        type = "pie", startAngle = -90, endAngle = 90, innerSize = '0%', 
        center = c('50%', '75%'),
        dataLabels = list(enabled = TRUE, 
                                                    distance = -50,
                                                    fontWeight = 'bold', 
                                                    color = 'black')) %>%
      
      highcharter::hc_tooltip(pointFormat = '<b>{point.percentage:.1f}%</b>') %>%
      
      highcharter::hc_title(text = shp@data[j, 'departement'], 
                            style = list(fontWeight = "bold")) %>%
      
      highcharter::hc_subtitle(text = paste("French presidential election", 
                                            year, "- Results of 2nd round of", 
                                            "voting")) %>%
      
      highcharter::hc_tooltip(valueDecimals = 2, 
                              pointFormat = "Votes: {point.y}%") %>%
      
      highcharter::hc_credits(enabled = TRUE, text = "Source: data.gouv.fr", 
                              href = paste0("http://www.data.gouv.fr/fr/", 
                                            "posts/les-donnees-des-elections/"),
                              style = list(fontSize = "10px")) %>%
      
      highcharter::hc_add_theme(theme_alone())
    
    ttips[[j]]$"x"$"conf_opts"$"lang"$"decimalPoint" <- "."
    
    
    ## Export tooltip ----
    
    filename <- tolower(shp@data[j, "code"])
    filename <- gsub("fr-", "", filename)
    filename <- paste0("piechart_", filename, ".html")
    
    htmlwidgets::saveWidget(widget = ttips[[j]], 
                            file = here::here("content", "piecharts", filename),
                            selfcontained = FALSE)
    
    
    ## Move JS libs (only one for all piecharts) ----
    
    dirname <- gsub("\\.html", "_files", filename)
    
    if (j == 1) {
      
      invisible(file.copy(here::here("content", "piecharts", dirname, "/"), 
                          here::here("content", "piecharts", "libs"), 
                          recursive = TRUE))
    }
    
    
    ## Delete JS libs ----
    
    unlink(here::here("content", "piecharts", dirname), recursive = TRUE)
    
    
    ## Correct path to JS libs ----
    
    html <- readLines(here::here("content", "piecharts", filename))
    html <- gsub(dirname, "libs", html)
    html <- gsub('"padding":15|"padding":40', '"padding":5', html)
    
    cat(paste0(html, collapse = '\n'), 
        file = here::here("content", "piecharts", filename))
  }
  
  
  ## Export list of barplots ----
  
  pop_up_graph <- leafpop::popupGraph(ttips, type = "html", 
                                      width = 750, height = 425)
  
  listname <- tolower(shp@data[ , "code"])
  listname <- gsub("fr-", "", listname)
  names(pop_up_graph) <- listname
  
  saveRDS(pop_up_graph, here::here("data", "derived-data", 
                                   "popup_piecharts.rds"))
  
  invisible(NULL)
}
