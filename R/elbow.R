#' Elbow Method to Detect Infection Point in a Concave Curve
#'
#' This function implements the Elbow rule to detect infection point in a
#'   concave curve.
#'
#' @param data a two-columns data frame (x and y respectively).
#' @param xmin [optional] the min value on x-Axis (if missing in data).
#' @param xmax [optional] the max value on x-Axis (if missing in data).
#' @param ymin [optional] the min value on y-Axis (if missing in data).
#' @param ymax [optional] the max value on y-Axis (if missing in data).
#'
#' @return This function returns a 2-elements list with:
#'   - a numeric giving the value on x-Axis corresponding to the optimal
#'   - a data frame with the original values and two additional columns used in
#'     the graphic.
#'
#' @export
#' @importFrom stats lm

elbow <- function(data, xmin = NULL, xmax = NULL, ymin = NULL, ymax = NULL) {

  if (is.null(xmin) && !is.null(ymin)) {
    stop("`xmin` cannot be NULL if `ymin` is not null.")
  }

  if (!is.null(xmin) && is.null(ymin)) {
    stop("`ymin` cannot be NULL if `xmin` is not null.")
  }

  if (is.null(xmax) && !is.null(ymax)) {
    stop("`xmax` cannot be NULL if `ymax` is not null.")
  }

  if (!is.null(xmax) && is.null(ymin)) {
    stop("`ymax` cannot be NULL if `xmax` is not null.")
  }

  data <- data[ , 1:2]

  if (!is.null(xmin)) {

    xdat <- data.frame(xmin, ymin)
    colnames(xdat) <- colnames(data)

    data <- rbind(xdat, data)
  }

  if (!is.null(xmax)) {

    xdat <- data.frame(xmax, ymax)
    colnames(xdat) <- colnames(data)

    data <- rbind(xdat, data)
  }

  data <- data[order(data[ , 1]), ]

  constant <- data[c(1, nrow(data)), ]
  colnames(constant) <- c("x", "y")

  mod <- stats::lm(y ~ x, data = constant)

  for (i in 1:nrow(data)) {
    data[i, "constant"] <- round(mod$coef[[1]] + mod$coef[[2]] * data[i, 1], 3)
  }

  data[ , "benefits"] <- round(data[ , 2] - data[ , "constant"], 3)


  data[ , "SelectedorNot"] <- NA

  for (i in 1:nrow(data)) {
    if (data$Axe[i] <= data[which.max(data[ , "benefits"]), ]$"Axe") {
      data[i, "SelectedorNot"] <- "Selected"
    } else {
      data[i, "SelectedorNot"] <- "Not Selected"
    }
  }

  xxx <- data[-1, ]
  rownames(xxx) <- xxx[ ,1]

  return(xxx)
}
