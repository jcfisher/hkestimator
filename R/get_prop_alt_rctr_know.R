#' Calculates the proportion that alter's recruiter (?) knows
#'
#' @param data: a data.frame
#' @param net: an adjacency matrix
#'
#' @export getpropaltrctrknow
#'
#' @return a matrix, little phi?

getpropaltrctrknow <- function(data, net) {
  r <- nrow(data)
  lilphi <- matrix(0, nrow = r, ncol = 1)
  for (k in 1:r) {
    potential.recruitees <- apply(t(net[data[k, 2], ]), 1, which)
    lilphi[k, 1] <- sum(net[potential.recruitees, data[k, 1]]) /
      (NROW(potential.recruitees) - 1)
  }
  return(lilphi)
}
