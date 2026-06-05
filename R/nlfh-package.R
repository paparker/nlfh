#' nlfh: Nonlinear Fay-Herriot Models for Small Area Estimation
#'
#' `nlfh` fits Bayesian Fay-Herriot models for area-level small area estimation.
#' The package works with direct estimates, known sampling variances, and
#' area-level covariates, returning posterior draws and summaries for latent
#' area quantities and model parameters.
#'
#' The main entry point is [fit_fh()]. It supports a formula interface for
#' specifying the response and covariates, and a matrix interface for workflows
#' that already construct design matrices. Fitted model objects provide compact
#' printing, posterior summaries via [summary()], fitted area estimates via
#' [fitted()], and retained MCMC draws via [posterior_draws()].
#'
#' Available model families are:
#'
#' * `method = "linear"`: the standard linear Fay-Herriot model.
#' * `method = "rnn"`: a random-weight neural network Fay-Herriot extension for
#'   nonlinear mean functions.
#' * `method = "bart"`: a BART Fay-Herriot model for flexible nonlinear and
#'   interaction effects. The first model-matrix column is treated as a
#'   baseline/intercept column and is excluded from BART variable importance.
#'
#' @keywords internal
"_PACKAGE"
