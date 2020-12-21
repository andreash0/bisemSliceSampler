## ---- slice_sampler.R


#' Markov Chain Monte Carlo with Slice Sampling
#'
#' @description
#' Samples from a function using Slice Sampling introduced by Neal (2003).
#' Depending on \code{x_curr} either univariate or multivariate slice sampling
#' is performed. In the univariate case, the user can choose between the
#' stepping out and the doubling procedure for interval search.
#'
#' @param f Density to sample from.
#' @param x_init Numerical vector containing initial state of chain.
#' @param w Scale estimate. For more information see details section.
#' @param n_samples Number of samples to be drawn.
#' @param max_extensions Maximal interval extensions in interval search for univariate
#' slice sampling. Does not affect multivariate slice sampling.
#' @param procedure_univ Interval search procedure for univariate slice sampling.
#' Can be either `stepping_out` (default) or `doubling`.
#' Does not affect multivariate slice sampling.
#' @param f_log_form \code{TRUE} if \code{f} is already in log form (e.g.
#' if \code{f} is log_likelihood). Default is \code{TRUE}.
#' @param param_log \code{TRUE} if temporary parameters such as horizontal value \code{y}
#' should be saved. Those can be used for visualizations.
#'
#' @return vector (in univariate case) or matrix (in multivariate case) containing
#' the drawn samples from \code{f}.
#'
#' @details
#' In the univariate case param \code{w} determines the interval width for interval search.
#' In the multivariate case its components contain the side lengths of the hyperrectangle
#' for all variables in \code{x_curr}.
#'
#' @references
#' \insertRef{neal2003slice}{bisSliceSampling}
#'
#' @importFrom Rdpack reprompt
#' @export
slice_sampler <- function(f, x_init, w,  n_samples = 1000, max_extensions = NULL,
                          procedure_univ = NULL, f_log_form = TRUE, param_log = NULL) {

  is_univariate <- length(x_init) == 1
  check_inputs(
    f, x_init, w, max_extensions, n_samples, procedure_univ,
    f_log_form, param_log, is_univariate
  )

  # Sample based on log(f(x)) if f is not already in log form in order to avoid
  # possible computational underflow
  if (f_log_form) {
    g <- function(x) f(x)
  } else {
    g <- function(x) log(f(x))
  }

  # Univariate Case
  if (is_univariate) {
    if (is.null(procedure_univ)) procedure_univ <- "stepping_out"
    samples <- slice_sampler_univ(
      g, x_init, w, max_extensions, n_samples, procedure_univ, param_log
    )
    # Multivariate Case
  } else {
    samples <- slice_sampler_mv(g, x_init, w, n_samples)
  }

  samples
}

## ----

check_inputs <- function(f, x_init, w, max_extensions, n_samples,
                         procedure_univ, f_log_form, param_log, is_univariate) {

  # For Univariate and Multivariate
  checkmate::assert_function(f)
  checkmate::assert_numeric(x_init, any.missing = FALSE)
  checkmate::assert_numeric(w, any.missing = FALSE)
  checkmate::assert_number(max_extensions, lower = 1, null.ok = TRUE)
  checkmate::assert_number(n_samples, lower = 1)
  checkmate::assert_logical(f_log_form, any.missing = FALSE, len = 1, null.ok = FALSE)
  checkmate::assert_logical(f_log_form, any.missing = FALSE, len = 1, null.ok = TRUE)

  if (length(x_init) != length(w)) {
    stop("x_init and w must have same length")
  }

  # Univariate Case
  if (is_univariate) {

    if (is.null(max_extensions)) {
      stop("max_extensions must be set in univariate case")
    }

    if (is.null(procedure_univ)) procedure_univ <- "stepping_out"
    procedure_univ_known <- procedure_univ %in% c("stepping_out", "doubling")
    if (!procedure_univ_known) {
      stop("procedure_univ must be either `stepping_out` or `doubling`")
    }

    # Multivariate Case
  } else {

    if (!is.null(procedure_univ)) {
      warning("procedure_univ can only be choosen in univariate case")
    }
    if (!is.null(max_extensions)) {
      warning("max_extensions can only be choosen in univariate case")
    }
    if (!is.null(param_log)) {
      warning("param_log can only be choosen in univariate case")
    }

  }


}


