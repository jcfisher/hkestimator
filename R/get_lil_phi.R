#' Calculates little phi
#'
#' @param data: a data.frame
#' @param net: an adjacency matrix
#'
#' @details WARNING: no checks on inputs, use with caution!
#'
#' @export getlilphi
#'
#' @return a matrix, the estimate of \eqn{\phi}
#'
getlilphi <- function(data, net) {
  r <- nrow(data)
  lilphi <- matrix(0, nrow = r, ncol = 1)
  for (k in 1:r) {
    lilphi[k, 1] <- net[data[k, 1], data[k, 3]]
  }
  return(lilphi)
}
