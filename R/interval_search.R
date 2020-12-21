interval_search <- function(g, x_curr, w, max_extensions, procedure, z) {
  if (procedure == "stepping_out") {
    interval <- stepping_out(g, x_curr, z, w, max_extensions)
  } else {
    interval <- doubling(g, x_curr, z, w, max_extensions)
  }
  interval
}

stepping_out <- function(g, x_curr, z, w, max_extensions) {

  # Randomly set interval of width w around current point
  U <- runif(1L, min = 0L, max = 1L)
  left_boarder <- x_curr - w * U
  right_boarder <- left_boarder + w
  # Randomly split up the maximum number of total interval extensions m to both sides.
  V <- runif(1L, min = 0L, max = 1L)
  # Maximal Number of interval expansions left
  expans_left <- floor(max_extensions * V)
  # Maximal Number of interval expansions right
  expans_right <- (max_extensions - 1L) - expans_left

  # Interval expansions to the left
  while (expans_left > 0 & z < g(left_boarder)) {
    left_boarder <- left_boarder - w
    expans_left <- expans_left - 1
  }

  # Interval expansions to the right
  while (expans_right > 0 & z < g(right_boarder)) {
    right_boarder <- right_boarder + w
    expans_right <- expans_right - 1
  }

  # Found interval
  c(left_boarder, right_boarder)
}


doubling <- function(g, x_curr, z, w, max_extensions) {

  # Randomly set interval of width w around current point
  U <- runif(1L, min = 0L, max = 1L)
  left_boarder <- x_curr - w * U
  right_boarder <- left_boarder + w
  # Remaining interval extensions
  rem_extensions <- max_extensions

  while (rem_extensions > 0 & (z < g(left_boarder)) | z < g(right_boarder)) {
    V <- runif(1L, min = 0L, max = 1L)
    interval_width <- (right_boarder - left_boarder)
    # Interval expansion in random direction
    if (V < 0.5) {
      # expansion to the left
      left_boarder <- left_boarder - interval_width
    } else {
      # expansion to the right
      right_boarder <- right_boarder + interval_width
    }
    rem_extensions <- rem_extensions - 1
  }
  c(left_boarder, right_boarder)
}
