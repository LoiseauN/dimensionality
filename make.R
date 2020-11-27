#' Run the Entire Project
#'
#' This script reproduces all analyses and figures of the ___________ article.
#'
#' @author Nicolas LOISEAU, \email{nicolas.loiseau1@@gmail.com},
#'         Michele ALLEGRA, \email{micheleallegra85@@gmail.com},
#'         Matthias GRENIE, \email{matthias.grenie@@cefe.cnrs.fr},
#'         Nicolas CASAJUS, \email{nicolas.casajus@@fondationbiodiversite.fr}
#'
#' @date 2020/11/27



## Install Dependencies (listed in DESCRIPTION)                             ----

if (!("remotes" %in% installed.packages())) install.packages("remotes")
remotes::install_deps(upgrade = "never")


## Load Project Addings (R functions and packages)                          ----

devtools::load_all()


## Global Variables                                                         ----

percent_list <- seq(0.1, 0.8, by = 0.1)


## Run Analyses                                                             ----

source(here::here("analyses", "bartonova_2016.R"))
