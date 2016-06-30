#' Calculates clustering coefficients
#'
#' @param net: an adjacency matrix
#'
#' @export getccs
#'
#' @return a named vector with the local and global clustering coefficients
getccs <- function(net) {
  # Local clustering coefficient
  ci <- matrix(NA, nrow = nrow(net), ncol = 1)
  for (i in 1:nrow(net)) {
    i.vals <- which(net[i, ])
    neighborhood <- net[i.vals, i.vals]
    ci[i, 1] <- sum(neighborhood) / (nrow(neighborhood) *
                                       (nrow(neighborhood) - 1))
  }
  ci[is.na(ci)] <- 0
  clocal <- mean(ci)

  # global clustering coefficient
  # aijaikajk is N3
  aijaikajk=matrix(0, nrow = nrow(net), ncol = 1)
  for (i in 1:nrow(net)) {
    jk <- which(net[, i])
    for (j in 1:length(jk)) {
      for (k in 1:length(jk)) {
        aijaikajk[i] <- aijaikajk[i] + net[i, jk[j]] %*% net[i, jk[k]] %*%
          net[jk[j], jk[k]]
      }
    }
  }

  # aijaik is N2
  aijaik <- matrix(0, nrow = rows(net), ncol = 1)
  for (i in 1:nrow(net)) {
    jk <- which(net[., i])
    for (j in 1:length(jk)) {
      for (k in 1:length(jk)) {
        aijaik[i] <- aijaik[i] + net[i, jk[j]] %*% net[i,jk[k]]
      }
    }
  }

  # these give the global clustering
  cglobal <- sum(aijaikajk) / sum(aijaik)

  # return
  return(list(clocal = clocal, cglobal = cglobal))
}
