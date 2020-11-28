#' Convert Variable Type
#'
#' These functions convert variables (columns of a data frame) to a numeric
#'   (`as_numerical()`), to a non-ordered categorical variable
#'   (`as_categorical()`), or to an ordinal variable (`as_ordinal()`).
#'
#' @param x a data frame
#' @param cols columns names or numbers corresponding to variables to convert
#' @param level a boolean
#'
#' @return A data frame.
#'
#' @export

as_numerical <- function(x, cols = NULL) {

  if (!is.null(cols)) {
    for (i in cols){
      x[ , i] <- as.numeric(as.character(x[ , i]))
    }
  }

  return(x)
}


#' @describeIn as_numerical Convert to categorical variable.
as_categorical <- function(x, cols = NULL) {

  if (!is.null(cols)) {
    for (i in cols){
      x[ , i] <- as.factor(as.character(x[ , i]))
    }
  }

  return(x)
}


#' @describeIn as_numerical Convert to ordinal variable.
as_ordinal <- function(x, cols = NULL, level = TRUE) {

  if (!is.null(cols)) {
    for (i in cols){
      x[ , i] <- factor(as.character(x[ , i], level = TRUE))
    }
  }

  return(x)
}
