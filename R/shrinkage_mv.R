shrinkage_mv <- function(g, x_curr, z, hyperrectangle) {
  x_dim <- length(x_curr)
  left_boarders <- hyperrectangle[, 1]
  right_boarders <- hyperrectangle[, 2]

  while (TRUE) {
    x_new <- rep(NA, x_dim)
    # Sample new point from interval
    for (j in 1:x_dim) {
      x_new[j] <- runif(1L, min = left_boarders[j], max = right_boarders[j])
    }
    # Return if point is within slice
    if (z < g(x_new)) {
      return(x_new)
    }
    for (j in 1:x_dim) {
      if (x_new[j] < x_curr[j]) {
        left_boarders[j] <- x_new[j]
      } else {
        right_boarders[j] <- x_new[j]
      }
    }
  }
}
