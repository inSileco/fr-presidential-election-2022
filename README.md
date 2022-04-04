## Results of French Presidential Election - 2022

<!-- badges: start -->
[![License: GPL (>= 2)](https://img.shields.io/badge/License-GPL%20%28%3E%3D%202%29-blue.svg)](https://choosealicense.com/licenses/gpl-2.0/)
<!-- badges: end -->


This repository contains material for the website: https://blog.insileco.io/fr-presidential-election-2022/
mapping votes cast and abstention for the French presidential election of 2022 at
the department level (metropolis and overseas departments).

The website was built with the software [**R**](https://www.r-project.org/) and 
the packages 
[`leaflet`](https://rstudio.github.io/leaflet/),
[`highcharter`](https://jkunst.com/highcharter/),
[`htmltools`](https://rstudio.github.io/htmltools/),
[`htmlwidgets`](https://www.htmlwidgets.org/),
[`leafem`](https://github.com/r-spatial/leafem), and
[`leafpop`](https://github.com/r-spatial/leafpop).



### Data sources



All original data are stored in 
[`data/raw-data`](https://github.com/inSileco/fr-presidential-election-2022/tree/main/data/raw-data)
and are organized as follow:

- `Presidentielle_2022_Resultats_Tour_1_c.xls`: dataset released by the 
**French Government** with results of the first round of voting at the Department level 
([source](http://www.data.gouv.fr/fr/posts/les-donnees-des-elections/)).
- `Presidentielle_2022_Resultats_Tour_2_c.xls`: dataset released by the 
**French Government** with results of the second round of voting at the Department level 
([source](http://www.data.gouv.fr/fr/posts/les-donnees-des-elections/)).
- `candidates_info_2022.csv`: table with the name of candidates and their associated colors
- `shp-FRA_DOM-composite.rds`: composite spatial layer 
([`sp`](https://github.com/edzer/sp) polygons) of France and overseas departments. 
Coordinates of overseas departments have been altered to be positioned at the bottom
of France. This layer is derived from the version 2.8 of the 
**Global Administrative Areas** ([GADM](http://www.gadm.org/version2)).
- `shp-FRA_DOM-composite_lowres.rds`: same at the previous layer but with simplified
geometries (low resolution of a better display).



### Usage



1. Clone the repository
2. Run the `make.R`
3. Open the `index.html` in a web browser

**N.B** All dependencies will be automatically installed using the R package 
[`renv`](https://rstudio.github.io/renv/).

