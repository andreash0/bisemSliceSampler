slice_sampler_univ <- function(g, x_curr, w, max_extensions, n_samples, procedure,
                               param_log) {
  samples <- c()
  z_vals <- c()
  for (i in 1:n_samples) {
    # Draw auxiliary variable
    z <- draw_auxiliary(g, x_curr)
    # Interval search
    interval <- interval_search(g, x_curr, w, max_extensions, procedure, z)
    # Shrinkage
    x_new <- shrinkage(g, x_curr, procedure, z, interval)
    samples <- c(samples, x_new)
    x_curr <- x_new
    if (i %% 1000 == 0) message(paste("Sampled", i, "from", n_samples, "\n"))
    if (param_log) z_vals <- c(z_vals, z)
  }

  if (param_log) attr(samples, "y") <- exp(z_vals)

  samples

}


draw_auxiliary <- function(g, x_curr) {
  g(x_curr) - rexp(1, 1)
}
