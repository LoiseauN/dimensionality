#' Get Combinations with a Giving Number of Variables
#'
#' This function is an alternative to the default combn() and returns a subset
#' of combinaison.
#'
#' @param x colnames of the matrix of traits
#' @param m number of modalities in the combination
#' @param pos number of possibilities considered
#'
#' @return __WHAT?__
#'
#' @export

get_combn <- function(x, m, pos) {

  combo <- rep(NA, m)
  start <- 1

  for (i in seq_len(m - 1)) {

    end_pos   <- cumsum(choose((length(x) - start):(m - i), m - i))
    selection <- which.max(end_pos >= pos)
    start     <- start + selection
    combo[i]  <- x[start - 1]
    pos       <- pos - c(0, end_pos)[selection]
  }

  combo[m] <- x[start + pos - 1]
  combo
}

#' @describeIn get_combn Batch execution of `get_combn()`
chose_n_combn <- function(x, m, pos) sapply(pos, function(y) get_combn(x, m, y))
