#' Download election results from Minister of Interior
#'
#' @param year The election year in the form of YYYY.
#'
#' @return No return value.
#'   
#' @export
#'
#' @examples
#' \dontrun{
#' get_election_results(2022)
#' }

get_election_results <- function(year) {
  
  url <- paste0("https://www.resultats-elections.interieur.gouv.fr/", 
                "presidentielle-", year)
  
  page <- rvest::session(paste0(url, "/index.html"))
  department <- rvest::html_nodes(page, xpath = '//*[@id="listeDpt"]/option')
  
  pages <- data.frame(url = rvest::html_attr(department, "value"),
                      dpt = rvest::html_text(department))
  
  pages <- pages[-c(1, nrow(pages)), ]
  
  datas <- list()
  datas[[1]] <- data.frame()
  datas[[2]] <- data.frame()
  
  for (i in 1:nrow(pages)) {
    
    code_dpt    <- strsplit(pages[i, "dpt"], " - ")[[1]][1]
    libelle_dpt <- strsplit(pages[i, "dpt"], " - ")[[1]][2]
    
    page <- rvest::session(paste0(url, "/", pages[i, "url"]))
    
    tables <- rvest::html_table(page, dec = ",")
    tables <- tables[-1]
    
    for (round in 1:2) {
      
      if (round == 1 || length(tables) == 4 && round == 2) {
        
        candidates <- as.data.frame(tables[[round + (round - 1)]])
        candidates[ , 2] <- as.numeric(gsub("\\s", "", candidates[ , 2]))
        
        resume <- as.data.frame(tables[[round + (round - 1) + 1]])
        resume[ , 2] <- as.numeric(gsub("\\s", "", resume[ , 2]))
        
        dat <- data.frame(
          `Code`        = code_dpt, 
          `Departement` = libelle_dpt,
          `Inscrits`    = resume[resume[ , 1] == "Inscrits", "Nombre"],
          `Abstentions` = resume[resume[ , 1] == "Abstentions", "Nombre"],
          `Votants`     = resume[resume[ , 1] == "Votants", "Nombre"],
          `Blancs`      = resume[resume[ , 1] == "Blancs", "Nombre"],
          `Nuls`        = resume[resume[ , 1] == "Nuls", "Nombre"],
          `Exprimes`    = resume[resume[ , 1] == "Exprimés", "Nombre"])
        
        for (j in 1:nrow(candidates)) {
          
          info <- strsplit(candidates[j, 1], "\\s")[[1]]
          name <- info[-c(1:2)]
          
          dat <- data.frame(dat, "Sexe" = ifelse(info[1] == "M.", "M", "F"))
          dat <- data.frame(dat, "Nom"  = paste0(name, collapse = " "))
          dat <- data.frame(dat, "Voix" = candidates[j, "Voix"])
        }
        
        datas[[round]] <- rbind(datas[[round]], dat)
      }
    }
  }
  
  for (round in 1:2) {
    if (nrow(datas[[round]])) {
      write.csv(datas[[round]], 
                here::here("data", "raw-data", 
                           paste0("Presidentielle_", year,"_Resultats_Tour_", 
                                  round, "_c.csv")),
                row.names = FALSE) 
    }
  }
  
  invisible(NULL)
}
