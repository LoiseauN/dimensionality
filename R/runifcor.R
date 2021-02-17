#' Generate Correlated Traits Matrix Following Uniform Distribution

runifcor <- function(x, rho) {
  
  hw <- function(r) ((3 - sqrt(1 + 8 * abs(r))) / 4) * sign(r)

  (x * sign(rho) + runif(length(x), -1 * hw(abs(rho)), hw(rho * sign(rho)))) %% 1
}
