#' Read election results
#'
#' Imports an XLS file stored in `data/YYYY/` and named 
#' `Presidentielle_YYYY_Resultats_Tour_X_c.xls` where `YYYY` is the year of the 
#' election and `X` is the round.
#'
#' @param year The election year in the form of YYYY.
#' 
#' @param round The election round (1 or 2).
#'
#' @return A `data.frame` with the following columns:
#'   - `no_departement`: the number of the department;
#'   - `departement`: the name of the department;
#'   - `inscrits`: the number of registered electors;
#'   - `abstention`: the number of abstentions;
#'   - `votants`: the number of voters;
#'   - `blancs`: the number of blank votes;
#'   - `nuls`: the number of spoiled votes;
#'   - `exprimes`: the number of expressed votes;
#'   - `xxx`: the number of votes for the candidate xxx.
#'   
#' @export
#'
#' @examples
#' \dontrun{
#' votes <- read_election_results(2022, round = 1)
#' }

read_election_results <- function(year, round) {
  
  check_year(year)
  check_round(round)
  
  path <- here::here("data", "raw-data", paste0("Presidentielle_", year, 
                                                "_Resultats_Tour_", round, 
                                                "_c.xls"))
  
  if (!file.exists(path)) {
    stop("Unable to find elections results file for year ", year, " and round ",
         round)
  }
  
  
  tab <- readxl::read_excel(path  = path, 
                            sheet = paste("D\u00e9partements Tour", round),
                            skip  = ifelse(round == 1, 0, 3),
                            progress = FALSE)
  tab <- as.data.frame(tab)
  
  
  ## Select general columns ----
  
  pattern <- paste0("d\u00e9partement$|^Inscrits$|^Abstentions$|^Votants$|", 
                    "^Blancs$|^Nuls$|^Exprim\u00e9s$")
  
  dat <- tab[ , grep(pattern, colnames(tab))]
  colnames(dat) <- c("no_departement", "departement", "inscrits", "abstention", 
                     "votants", "blancs", "nuls", "exprimes")
  
  
  ## Select candidates columns ----
  
  pattern <- paste0("^Voix|^Nom")
  
  mat <- tab[ , grep(pattern, colnames(tab))]

  
  ## Wider to long format ----
  
  tab <- data.frame()
  
  for (i in seq(1, ncol(mat), 2)) {
    
    tmp <- data.frame(dat, "name" = mat[ , i], "vote" = mat[ , i + 1])
    tab <- rbind(tab, tmp)
  }
  
  
  ## Check candidate names ----
  
  tab$"name" <- tolower(gsub("-| ", "", tab$"name"))
  tab$"name" <- tolower(gsub("\u00e9", "e",  tab$"name"))
  
  candidates <- sort(unique(tab$"name"))
  
  candidates_list <- read_candidates_info(year)
  candidates_list <- candidates_list$"name"
  
  if (sum(candidates %in% candidates_list) != length(candidates)) {
    stop("Unable to find info for some candidates")
  }
  
  
  ## Long to wider format ----
  
  votes <- dat
  
  for (candidate in candidates) {
    
    dat <- tab[which(tab[ , "name"] == candidate), c("no_departement", "vote")]
    colnames(dat)[2] <- candidate
    votes <- merge(votes, dat, by = "no_departement", all = TRUE)
  }
  
  votes
}
