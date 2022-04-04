#' Results of French Presidential Election 2022
#' 
#' @author 
#'   - Nicolas Casajus \email{nicolas.casajus@@gmail.com}
#'   - Kevin Cazelles \email{kevin.cazelles@@gmail.com}
#'   - Steve Vissault \email{s.vissault@@yahoo.fr}
#' 
#' @date 2022/04/04



## Install dependencies (listed in renv.lock) ----

renv::restore()


## Load project addins (R functions and packages) ----

devtools::load_all(here::here())


## Global variables ----

year <- 2022


## Add election results to French departments geometry ----

add_data_to_shp(year, round = 1)
# add_data_to_shp(year, round = 2)


## Create tooltips - Barplots for 1st round ----

make_tooltip_barplot(year)


## Create tooltips - Piecharts for 2nd round ----

# make_tooltip_piechart(year)


## Create Web page ----

create_html_content(year)
