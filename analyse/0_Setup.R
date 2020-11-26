#' Setup the Project
#'
#' This R script installs missing packages and loads required R functions
#' (listed in the R directory) and packages.
#'
#' @author Nicolas Loiseau, \email{nicolas.loiseau1@@gmail.com},
#'         Michele Allegra, \email{micheleallegra85@gmail.com},
#'         Matthias Grenié, \email{Matthias.grenie@cefe.cnrs.fr},
#'         Nicolas Casajus , \email{nicolas.casajus@@fondationbiodiversite.fr},
#'
#' @date 2020/06/10


## Install Missing Packages (listed in DESCRIPTION file) ----

pkgs <- c("vegan", "cluster", "ape", "FD", "data.table", "coRanking","parallel","gridExtra","grid",
          "ggplot2","dplyr","ade4","mistr","dendextend","pbmcapply","usethis")

nip  <- pkgs[!(pkgs %in% installed.packages())]
nip <- lapply(nip, install.packages, dependencies = TRUE)
ip   <- unlist(lapply(pkgs, require, character.only = TRUE, quietly = TRUE))


source(here::here('R', 'dim_plot.R'))
source(here::here('R','calc_dist.R'))
source(here::here('R','compute_missing_trait_distance.R'))
source(here::here('R','depletion_plot.R'))
source(here::here('R','dim_plot.R'))
source(here::here('R','dimension_funct.R'))
source(here::here('R','elbow.R'))
source(here::here('R','get_trait_combinations.R'))
source(here::here('R','getcombn.R'))
source(here::here('R','parse_miss_trait.R'))
source(here::here('R','sample_trait_combinations.R'))
source(here::here('R','singleton.R'))
source(here::here('R','synth_results.R'))
