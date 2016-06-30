#' Calculates HK clustering coefficients
#'
#' @param recruiter: a vector listing... recruiter ID number?
#' @param recruit: a vector listing... recruit ID number?
#' @param recruitee: a vector listing... recruitee ID number?
#' @param deg: a numeric vector of degree values for each recruit
#' @param net: the adjacency matrix for the network
#'
#' @export HardimanKatzirCCs
#'
#' @return A named, numeric list containing the various clustering coefficients

HardimanKatzirCCs <- function(recruiter, recruit, recruitee, deg, net) {
  lilphi <- getlilphi(data.frame(recruiter, recruit, recruitee), net)
  n <- length(deg)  # equiv to rows(rdssample) in Stata code

  # Local clustering coefficient
  phi_l <- (1 / n) * sum(lilphi * (1 / (deg - 1)))
  psi_l <- (1 / n) * sum(1 / deg)
  clhat <- phi_l / psi_l

  # Global clustering coefficient
  phi_g <- (1 / (n - 2)) * sum(lilphi * deg)
  psi_g <- (1 / n) * sum(deg - 1)
  cghat <- phi_g / psi_g

  ## Percent measure

  # Local clustering coefficient
  lilphi <- getpropaltrctrknow(data.frame(recruiter, recruit), net)
  phi_l <- (1 / n) * sum(lilphi * (1 / (deg - 1)))
  psi_l <- (1 / n) * sum(1 / deg)
  clhat_pct <- phi_l / psi_l

  # Global clustering coefficient
  phi_g <- (1 / (n - 2)) * sum(lilphi * deg)
  psi_g <- (1 / n) * sum(deg - 1)
  cghat_pct <- phi_g / psi_g

  return(list(nsamp = n, clhat = clhat, cghat = cghat, clhat_pct = clhat_pct,
              cghat_pct))
}
