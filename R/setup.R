#' Setup the Project
#'
#' This R script installs missing packages and loads required R functions
#' (listed in the R directory) and packages.
#'
#' @author Nicolas Loiseau, \email{nicolas.loiseau1@@gmail.com},
#'         Nicolas Casajus , \email{nicolas.casajus@@fondationbiodiversite.fr},
#'         Nicolas Mouquet, \email{nicolas.mouquet@@cnrs.fr}
#'
#' @date 2020/06/10


## Install Missing Packages (listed in DESCRIPTION file) ----

pkgs <- c("vegan", "cluster", "ape", "FD", "data.table", "coRanking","parallel","gridExtra","grid",
          "ggplot2","dplyr","ade4","mistr","dendextend","pbmcapply","usethis")

nip  <- pkgs[!(pkgs %in% installed.packages())]
nip <- lapply(nip, install.packages, dependencies = TRUE)
ip   <- unlist(lapply(pkgs, require, character.only = TRUE, quietly = TRUE))


