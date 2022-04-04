#' Read candidates information
#'
#' Imports a CSV file stored in `data/YYYY/` and named 
#' `candidates_info_YYYY.csv` where `YYYY` is the year of the election. 
#'
#' @param year The election year in the form of YYYY.
#'
#' @return A `data.frame` with three columns:
#'   - `name`: the name of the candidate (for joins);
#'   - `label`: the name of the candidate (for display);
#'   - `color`: the color associated to the candidate.
#'   
#' @export
#'
#' @examples
#' \dontrun{
#' info <- read_candidates_info(2022)
#' }

read_candidates_info <- function(year) {
  
  check_year(year)
  
  path <- here::here("data", "raw-data", paste0("candidates_info_", year, 
                                                ".csv"))
  
  if (!file.exists(path)) {
    stop("Unable to find candidates info file for year ", year)
  }
                     
  utils::read.csv(path)
}
