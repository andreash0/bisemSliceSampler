slice_sampler_mv <- function(g, x_curr, w, n_samples) {

  samples <- matrix(NA, nrow = n_samples, ncol = length(x_curr))
  for (i in 1:n_samples) {
    # Draw auxiliary variable
    z <- draw_auxiliary(g, x_curr)
    # Set hyperrectangle
    hyperrectangle <- set_hyperrectangle(x_curr, w)
    # Draw new point per shrinkage procedure
    x_new <- shrinkage_mv(g, x_curr, z, hyperrectangle)
    samples[i, ] <- x_new
    x_curr <- x_new
    if (i %% 100 == 0) message(paste("Sampled", i, "from", n_samples, "\n"))
  }

  samples

}


set_hyperrectangle <- function(x_curr, w){
  x_dim <- length(x_curr)
  left_boarders <- rep(NA, x_dim)
  right_boarders <- rep(NA, x_dim)
  for (j in 1:x_dim) {
    U <- runif(1)
    # Randomly set interval of width w_i around current x_i
    left_boarders[j] <- x_curr[j] - w[j] * U
    right_boarders[j] <- left_boarders[j] + w[j]
  }
  matrix(c(left_boarders, right_boarders), ncol = 2)
}




