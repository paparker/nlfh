#' Extract Posterior Draws from a Fay-Herriot Model
#'
#' Extract posterior draws stored in fitted Bayesian Fay-Herriot model objects.
#' Draws are returned with one row per retained MCMC draw and one column per
#' requested parameter or area-level quantity.
#'
#' @param object A fitted model object.
#' @param variable Character string naming the posterior quantity to extract.
#'   Supported values are `"theta"` for area-level estimates, `"beta"` for
#'   regression or hidden-layer coefficients when available, `"u"` for
#'   area-level random effects when available, and `"A"` for the random-effect
#'   variance parameter.
#' @param ... Additional arguments passed to methods.
#'
#' @return A `posterior::draws_df` object when the `posterior` package is
#'   installed. Otherwise, a data frame with one row per posterior draw.
#' @export
#'
#' @examples
#' data(acs_dat)
#' acs_small <- as.data.frame(acs_dat[1:500, ])
#' fit <- fit_fh(
#'   MedInc ~ SNAPRate + PovRate + White + Black + Hispanic + Asian,
#'   sampling_variance = MedIncSE^2,
#'   data = acs_small,
#'   method = "linear",
#'   control = list(n_iter = 500, burn_in = 250, progress = FALSE)
#' )
#'
#' theta_draws <- posterior_draws(fit, variable = "theta")
#' beta_draws <- posterior_draws(fit, variable = "beta")
posterior_draws <- function(object, variable = c("theta", "beta", "u", "A"),
                            ...) {
  UseMethod("posterior_draws")
}

#' @rdname posterior_draws
#' @export
posterior_draws.nlfh_fit <- function(object,
                                     variable = c("theta", "beta", "u", "A"),
                                     ...) {
  variable <- match.arg(variable)
  draws <- switch(
    variable,
    theta = .nlfh_theta_draws(object),
    beta = .nlfh_beta_draws(object),
    u = .nlfh_u_draws(object),
    A = .nlfh_A_draws(object)
  )
  .nlfh_as_draws_df(draws)
}

.nlfh_theta_draws <- function(object) {
  if (is.null(object$predictions)) {
    stop("Posterior draws for `theta` are not available.", call. = FALSE)
  }
  draws <- t(as.matrix(object$predictions))
  colnames(draws) <- .nlfh_parameter_names("theta", rownames(object$predictions),
                                           ncol(draws))
  draws
}

.nlfh_beta_draws <- function(object) {
  if (is.null(object$coefficients)) {
    stop("Posterior draws for `beta` are not available for this model.", call. = FALSE)
  }
  draws <- as.matrix(object$coefficients)
  beta_names <- colnames(draws)
  if (is.null(beta_names) &&
      !is.null(object$model_matrix_colnames) &&
      length(object$model_matrix_colnames) == ncol(draws)) {
    beta_names <- object$model_matrix_colnames
  }
  if (is.null(beta_names) && !is.null(object$n_hidden) &&
      object$n_hidden == ncol(draws)) {
    beta_names <- paste0("hidden", seq_len(ncol(draws)))
  }
  colnames(draws) <- .nlfh_parameter_names("beta", beta_names, ncol(draws))
  draws
}

.nlfh_u_draws <- function(object) {
  if (is.null(object$random_effects)) {
    stop("Posterior draws for `u` are not available for this model.", call. = FALSE)
  }
  draws <- t(as.matrix(object$random_effects))
  colnames(draws) <- .nlfh_parameter_names("u", rownames(object$random_effects),
                                           ncol(draws))
  draws
}

.nlfh_A_draws <- function(object) {
  if (is.null(object$random_effect_variance)) {
    stop("Posterior draws for `A` are not available.", call. = FALSE)
  }
  draws <- matrix(object$random_effect_variance, ncol = 1L)
  colnames(draws) <- "A"
  draws
}

.nlfh_parameter_names <- function(prefix, labels, n) {
  if (is.null(labels)) {
    labels <- seq_len(n)
  }
  paste0(prefix, "[", labels, "]")
}

.nlfh_as_draws_df <- function(draws) {
  out <- as.data.frame(draws, check.names = FALSE)
  if (requireNamespace("posterior", quietly = TRUE)) {
    return(posterior::as_draws_df(out))
  }
  out
}
