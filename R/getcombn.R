################################################################################
#' Function to find combination. (because combn give all possible combination and it is too big!!)
#' @param x colnames of matrice of traits
#' @param m number of modality in the combination   
#' @param pos number of possibility considered
#' ################################################################################
getcombn <- function(x, m, pos) {
  
  combo <- rep(NA, m)
  start <- 1
  
  for (i in seq_len(m-1)) {
    end.pos <- cumsum(choose((length(x)-start):(m-i), m-i))
    selection <- which.max(end.pos >= pos)
    start <- start + selection
    combo[i] <- x[start - 1]
    pos <- pos - c(0, end.pos)[selection]
  }
  
  combo[m] <- x[start + pos - 1]
  combo
}

chosencombn <- function(x, m, all.pos) {
  sapply(all.pos, function(pos) getcombn(x, m, pos))
}
#-------------
