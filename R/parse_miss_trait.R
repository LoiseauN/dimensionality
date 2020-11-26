################################################################################
#' Parse Name of missing trait category
#'
#' Return two numbers: missing trait percentage and number of traits
#' @param miss_trait_name a character of type "misstrait_15%_n4"
#################################################################################
parse_miss_trait = function(miss_trait_name) {
  miss_part = strsplit(as.character(miss_trait_name), "_")[[1]]
  
  miss_percent = as.numeric(gsub("%", "", miss_part[2])) / 100
  
  n_trait = as.numeric(gsub("n", "", miss_part[3]))
  
  list(miss_percent = miss_percent, n_trait = n_trait)
}
