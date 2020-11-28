#' Find Empty Numerical Vector
#'
#' @param x a numerical vector
#'
#' @return a boolean
#'
#' @export

is_empty <- function(x) identical(x, numeric(0))
