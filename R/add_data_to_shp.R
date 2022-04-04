#' Add election results to department geometry
#'
#' @param year The election year in the form of YYYY.
#' 
#' @param round The election round (1 or 2).
#'
#' @return A `SpatialPolygonsDataFrame` with values and labels to plot/map.
#'   
#' @export
#'
#' @examples
#' \dontrun{
#' add_data_to_shp(2022, round = 1)
#' }

add_data_to_shp <- function(year, round) {
  
  check_year(year)
  check_round(round)
  
  
  ## Get candidates list ----
  
  infos <- read_candidates_info(year)
  
  candidates <- infos$"name"
  candidates <- candidates[!(candidates %in% c("blancs", "nuls", "abstention"))]
  
  
  ## Import department layer ----
  
  shp <- readRDS(here::here("data", "raw-data", 
                            "shp-FRA_DOM-composite_lowres.rds"))
  dat <- data.frame(shp@data)
  
  
  ## Import election results ----
  
  votes <- read_election_results(year, round)
  
  
  ## Select candidates ----
  
  candidates <- candidates[candidates %in% colnames(votes)]
  
  
  ## Merge results ----
  
  dat <- merge(dat, votes, by = "departement", all.x = TRUE, all.y = FALSE)
  
  
  ## Get winner ----
  
  dat$"first_label" <- as.character(apply(dat[ , candidates], 1, function(x) {
    names(x)[which(x == max(x))] 
    }))
  
  dat <- merge(dat, infos, by.x = "first_label", by.y = "name", all.x = TRUE, 
               all.y = FALSE)
  
  colnames(dat)[ncol(dat)]     <- "first_color"
  colnames(dat)[ncol(dat) - 1] <- "first_name"
  
  dat$"first_value" <- apply(dat, 1, function(x) {
    pos <- names(x)[which(names(x) == x["first_label"])]
    round(100 * as.numeric(x[pos]) / as.numeric(x["exprimes"]), 1)
  })
  
  
  ## Re-order columns ----
  
  dat <- dat[ , c(3, 5, 4, 2, 6:(ncol(dat) - 3), 1, (ncol(dat) - 2):ncol(dat))]
  
  
  ## Percentage of abstention ----
  
  dat$"abstention" <- dat$"abstention" / dat$"inscrits"
  dat$"abstention" <- round(100 * dat$"abstention", 1)
  
  abstention_classes <- seq(0, 100, by = 5)
  abstention_colors  <- RColorBrewer::brewer.pal(name = "YlOrRd", 9)
  
  cols <- highcharter::color_classes(abstention_classes, 
                                     colors = abstention_colors)
  
  dat$"abstention_color" <- NA
  
  for (i in 1 : length(cols)){
    dat$"abstention_color" <- ifelse(dat$"abstention" >= cols[[i]]$"from" &
                                     dat$"abstention" <  cols[[i]]$"to",
                                     cols[[i]]$"color", dat$"abstention_color")
  }
  
  
  ## Append to shp ----
  
  dat <- dat[order(as.numeric(dat$"noid")), ]
  rownames(dat) <- dat$"noid"
  
  shp@data <- dat
  
  
  ## Export shapefile ----
  
  saveRDS(shp, here::here("data", "derived-data", paste0("election_results_", 
                                                         year, "_round_", round,
                                                         ".rds")))
  
  invisible(shp)
}
