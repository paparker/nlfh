#' Fit a Linear Fay-Herriot Model
#'
#' Fits the basic Bayesian Fay-Herriot model with a linear mean function and
#' area-level random effects.
#'
#' @param formula Optional model formula such as `y ~ x1 + x2`. The formula
#'   interface requires `data`.
#' @param data Optional data frame containing variables used by `formula` and,
#'   optionally, `sampling_variance`.
#' @param y Numeric vector of area-level direct estimates for the matrix
#'   interface. If the first argument is a formula, it is treated as `formula`.
#' @param x,X Numeric matrix or data frame of area-level covariates for the
#'   matrix interface. Rows must correspond to entries of `y`. Include an
#'   intercept column if one is desired.
#' @param sampling_variance Numeric vector of known sampling variances for `y`.
#'   With the formula interface, this may also be an unquoted column name from
#'   `data` or a length-one character string naming a column in `data`.
#' @param prior_beta_variance Positive scalar prior variance for the regression
#'   coefficients.
#' @param prior_shape Non-negative scalar shape parameter for the inverse-gamma
#'   prior on the random-effect variance.
#' @param prior_rate Non-negative scalar rate parameter for the inverse-gamma
#'   prior on the random-effect variance.
#' @param n_iter Positive integer number of MCMC iterations.
#' @param burn_in Positive integer number of initial MCMC iterations to discard.
#' @param scale Logical; if `TRUE`, center and scale non-intercept covariates
#'   before fitting. Intercept columns named `(Intercept)`, `Intercept`, or
#'   `intercept` are not scaled.
#' @param progress Logical; if `TRUE`, display a progress bar.
#' @return An object of class `nlfh_linear_fit` and `nlfh_fit`, a list with
#'   posterior draws for `predictions`, `random_effect_variance`, `coefficients`,
#'   `mean`, the scalar `dic`, and MCMC metadata.
#'
#' @details
#' Formula inputs are parsed with [stats::model.frame()] and
#' [stats::model.matrix()]. Factors are expanded using R's standard contrast and
#' dummy-variable rules. An intercept is included when the formula includes one,
#' which is the default; matrix inputs are used as supplied.
#' @export
#'
#' @examples
#' data(acs_dat)
#' acs_small <- as.data.frame(acs_dat[1:500, ])
#' fit <- fit_fh_linear(
#'   MedInc ~ SNAPRate + PovRate + White + Black + Hispanic + Asian,
#'   sampling_variance = MedIncSE^2,
#'   data = acs_small,
#'   n_iter = 500,
#'   burn_in = 250,
#'   progress = FALSE
#' )
#' summary(fit)
fit_fh_linear <- function(y = NULL, x = NULL, sampling_variance = NULL,
                          formula = NULL, data = NULL, X = NULL,
                          prior_beta_variance = 10000^2,
                          prior_shape = 0.1, prior_rate = 0.1,
                          n_iter = 1000, burn_in = 500,
                          scale = FALSE, progress = TRUE) {
  input <- parse_fh_inputs(
    formula = formula,
    data = data,
    y_expr = if (missing(y)) NULL else substitute(y),
    x_expr = if (missing(x)) NULL else substitute(x),
    X_expr = if (missing(X)) NULL else substitute(X),
    sampling_variance_expr = if (missing(sampling_variance)) NULL else {
      substitute(sampling_variance)
    },
    env = parent.frame()
  )
  input <- .scale_fh_inputs(input, scale, baseline = "intercept")
  y <- input$y
  x <- input$X
  sampling_variance <- input$vardir
  prior_beta_variance <- .validate_positive_scalar(
    prior_beta_variance,
    "prior_beta_variance"
  )
  prior_shape <- .validate_nonnegative_scalar(prior_shape, "prior_shape")
  prior_rate <- .validate_nonnegative_scalar(prior_rate, "prior_rate")
  mcmc <- .validate_mcmc(n_iter, burn_in)
  n_iter <- mcmc$n_iter
  burn_in <- mcmc$burn_in
  progress <- .validate_logical_scalar(progress, "progress")

  n <- length(y)
  p <- ncol(x)
  inv_sampling_variance <- 1 / sampling_variance
  tau2 <- 1
  eta1 <- stats::rnorm(n)
  tau2_out <- rep(NA_real_, n_iter)
  beta1_out <- matrix(NA_real_, nrow = n_iter, ncol = p)
  eta1_out <- matrix(NA_real_, nrow = n_iter, ncol = n)
  theta_out <- xb_out <- matrix(NA_real_, nrow = n, ncol = n_iter)
  ll <- rep(NA_real_, n_iter)
  xtx <- crossprod(x, x * inv_sampling_variance)
  ub <- chol(xtx + diag(1 / prior_beta_variance, p))
  if (progress) {
    pb <- utils::txtProgressBar(min = 0, max = n_iter, style = 3)
    on.exit(close(pb), add = TRUE)
  }
  for (i in seq_len(n_iter)) {
    mean_beta <- crossprod(x, inv_sampling_variance * (y - eta1))
    b <- stats::rnorm(p)
    beta1 <- beta1_out[i, ] <- backsolve(
      ub,
      backsolve(ub, mean_beta, transpose = TRUE) + b
    )

    eta_variance <- 1 / (inv_sampling_variance + 1 / tau2)
    mean_eta <- eta_variance * inv_sampling_variance * as.numeric(y - x %*% beta1)
    eta1 <- eta1_out[i, ] <- stats::rnorm(
      n,
      mean = as.numeric(mean_eta),
      sd = sqrt(eta_variance)
    )

    theta <- theta_out[, i] <- x %*% beta1 + eta1
    xb_out[, i] <- theta - eta1

    tau2 <- tau2_out[i] <- 1 / stats::rgamma(
      1,
      prior_shape + n / 2,
      prior_rate + t(theta - x %*% beta1) %*% (theta - x %*% beta1) / 2
    )
    ll[i] <- -2 * sum(stats::dnorm(
      y,
      mean = theta,
      sd = sqrt(sampling_variance),
      log = TRUE
    ))

    if (progress) {
      utils::setTxtProgressBar(pb, i)
    }
  }

  keep_cols <- .posterior_columns(n_iter, burn_in)
  keep_rows <- .posterior_rows(n_iter, burn_in)
  dic <- 2 * mean(ll[keep_rows]) +
    2 * sum(stats::dnorm(
      y,
      mean = rowMeans(theta_out[, keep_cols, drop = FALSE]),
      sd = sqrt(sampling_variance),
      log = TRUE
    ))

  .new_nlfh_fit(
    c(
      list(
      predictions = theta_out[, keep_cols, drop = FALSE],
      random_effect_variance = tau2_out[keep_rows],
      coefficients = beta1_out[keep_rows, , drop = FALSE],
      mean = xb_out[, keep_cols, drop = FALSE],
      dic = dic,
      method = "linear",
      call = match.call(),
      n_iter = n_iter,
      burn_in = burn_in,
      progress = progress
      ),
      .fh_fit_metadata(input)
    ),
    "nlfh_linear_fit"
  )
}
