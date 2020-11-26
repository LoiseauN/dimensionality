#' Setup the Project
#'
#' This R script installs missing packages and loads required R functions
#' (listed in the R directory) and packages.
#'
#' @author Nicolas Casajus, \email{nicolas.casajus@@fondationbiodiversite.fr},
#'         Nicolas Loiseau, \email{nicolas.loiseau1@@gmail.com},
#'         Nicolas Mouquet, \email{nicolas.mouquet@@cnrs.fr}
#'
#' @date 2020/06/10


## Install Missing Packages (listed in DESCRIPTION file) ----

if (!("remotes" %in% installed.packages())) install.packages("remotes")
if (!("devtools" %in% installed.packages())) install.packages("devtools")

remotes::install_deps(upgrade = "never")

if (!("ggtree" %in% installed.packages())) BiocManager::install("ggtree")


## Load Project Addings (R functions and packages) ----

devtools::load_all()
