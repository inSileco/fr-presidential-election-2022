#' Define elections years
#' 
#' @noRd

years <- function() c(2017, 2022)



#' Define elections rounds
#' 
#' @noRd

rounds <- function() 1:2



#' Check election year
#' 
#' @noRd

check_year <- function(year) {
  
  if (length(year) != 1) {
    stop("Argument 'year' must of length 1", call. = FALSE)
  }
  
  if (!(year %in% years())) {
    msg <- paste0(years(), collapse = " or ")
    stop("Argument 'year' must be equal to ", msg, call. = FALSE)
  }
  
  invisible(NULL)
}



#' Check election round
#' 
#' @noRd

check_round <- function(round) {
  
  if (length(round) != 1) {
    stop("Argument 'round' must of length 1", call. = FALSE)
  }
  
  if (!(round %in% rounds())) {
    msg <- paste0(rounds(), collapse = " or ")
    stop("Argument 'round' must be equal to ", msg, call. = FALSE)
  }
  
  invisible(NULL)
}
