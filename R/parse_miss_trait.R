#' Parse Name of Missing Trait Category
#'
#' @param miss_trait_name a character of type "misstrait_15%_n4"
#'
#' @return Returns two numbers: the percentage of missing traits and the number
#'   of traits
#'
#' @export

parse_miss_trait <- function(miss_trait_name) {

  miss_part <- strsplit(as.character(miss_trait_name), "_")[[1]]

  list(
    miss_percent = as.numeric(gsub("%", "", miss_part[2])) / 100,
    n_trait      = as.numeric(gsub("n", "", miss_part[3]))
  )
}
