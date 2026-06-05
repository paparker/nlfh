#' Fit a Random-Weight Neural Network Fay-Herriot Model
#'
#' Fits a Bayesian Fay-Herriot model whose mean function is represented by a
#' fixed random hidden layer with logistic activation and sampled output-layer
#' coefficients.
#'
#' @param formula Optional model formula such as `y ~ x1 + x2`. For nonlinear
#'   models, the formula specifies the predictors available to the model; it
#'   does not imply an additive linear mean structure.
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
#' @param n_hidden Positive integer number of hidden nodes in the random-weight
#'   neural network.
#' @param prior_beta_variance Optional positive scalar prior variance for the
#'   output-layer coefficients. When `NULL`, the coefficient variance is sampled
#'   with the original inverse-gamma update.
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
#' @return An object of class `nlfh_rnn_fit` and `nlfh_fit`, a list with
#'   posterior draws for `predictions`, `random_effect_variance`,
#'   `coefficient_variance`, hidden-layer `coefficients`, `mean`, the scalar
#'   `dic`, and MCMC metadata.
#'
#' @references
#' Parker, P. A. (2024). Nonlinear Fay-Herriot Models for Small Area Estimation
#'   Using Random Weight Neural Networks. *Journal of Official Statistics*,
#'   40(2), 317-332. \doi{10.1177/0282423X241244671}
#'
#' @details
#' Formula inputs are parsed with [stats::model.frame()] and
#' [stats::model.matrix()]. Factors are expanded using R's standard contrast and
#' dummy-variable rules. An intercept is included when the formula includes one,
#' which is the default; matrix inputs are used as supplied. For this nonlinear
#' method, the formula specifies the available predictors and does not impose an
#' additive linear mean structure. The model estimates an unknown function
#' `f(X)`.
#'
#' The response and sampling variances are standardized internally before
#' fitting the RNN. Posterior predictions, mean function draws, coefficients,
#' random-effect variances, and DIC are transformed back to the original
#' response scale before being returned.
#' @export
#'
#' @examples
#' data(acs_dat)
#' acs_small <- as.data.frame(acs_dat[1:500, ])
#' fit <- fit_fh_rnn(
#'   MedInc ~ .,
#'   sampling_variance = MedIncSE^2,
#'   data = acs_small,
#'   n_iter = 500,
#'   burn_in = 250,
#'   progress = FALSE
#' )
#' summary(fit)
fit_fh_rnn <- function(y = NULL, x = NULL, sampling_variance = NULL,
                       formula = NULL, data = NULL, X = NULL,
                       n_hidden = 200, prior_beta_variance = NULL,
                       prior_shape = 0.1, prior_rate = 0.1,
                       n_iter = 1000, burn_in = 500, scale = TRUE,
                       progress = TRUE) {
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
  y_original <- input$y
  x <- input$X
  sampling_variance_original <- input$vardir
  response_center <- mean(y_original)
  response_scale <- stats::sd(y_original)
  if (!is.finite(response_scale) || response_scale == 0) {
    response_scale <- 1
  }
  y <- (y_original - response_center) / response_scale
  sampling_variance <- sampling_variance_original / response_scale^2
  n_hidden <- .validate_positive_integer(n_hidden, "n_hidden")
  if (!is.null(prior_beta_variance)) {
    prior_beta_variance <- .validate_positive_scalar(
      prior_beta_variance,
      "prior_beta_variance"
    )
  }
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
  sig2b <- if (is.null(prior_beta_variance)) 1 else prior_beta_variance
  eta1 <- stats::rnorm(n)
  tau2_out <- sig2b_out <- rep(NA_real_, n_iter)
  beta1_out <- matrix(NA_real_, nrow = n_iter, ncol = n_hidden)
  eta1_out <- matrix(NA_real_, nrow = n_iter, ncol = n)
  hidden_layer <- stats::plogis(
    x %*% matrix(stats::rnorm(p * n_hidden, sd = 1), nrow = p)
  )
  xtx <- crossprod(hidden_layer, hidden_layer * inv_sampling_variance)
  theta_out <- xb_out <- matrix(NA_real_, nrow = n, ncol = n_iter)
  ll <- rep(NA_real_, n_iter)
  if (progress) {
    pb <- utils::txtProgressBar(min = 0, max = n_iter, style = 3)
    on.exit(close(pb), add = TRUE)
  }
  for (i in seq_len(n_iter)) {
    mean_beta <- crossprod(hidden_layer, inv_sampling_variance * (y - eta1))
    ub <- chol(xtx + diag(1 / sig2b, n_hidden))
    b <- stats::rnorm(n_hidden)
    beta1 <- beta1_out[i, ] <- backsolve(
      ub,
      backsolve(ub, mean_beta, transpose = TRUE) + b
    )

    eta_variance <- 1 / (inv_sampling_variance + 1 / tau2)
    mean_eta <- eta_variance * inv_sampling_variance *
      as.numeric(y - hidden_layer %*% beta1)
    eta1 <- eta1_out[i, ] <- stats::rnorm(
      n,
      mean = as.numeric(mean_eta),
      sd = sqrt(eta_variance)
    )

    theta <- theta_out[, i] <- hidden_layer %*% beta1 + eta1
    xb_out[, i] <- theta - eta1

    tau2 <- tau2_out[i] <- 1 / stats::rgamma(
      1,
      prior_shape + n / 2,
      prior_rate + t(theta - hidden_layer %*% beta1) %*%
        (theta - hidden_layer %*% beta1) / 2
    )

    sig2b <- sig2b_out[i] <- if (is.null(prior_beta_variance)) {
      1 / stats::rgamma(
        1,
        20 + n_hidden / 2,
        8 + sum(beta1^2) / 2
      )
    } else {
      prior_beta_variance
    }

    theta_original <- response_center + response_scale * theta
    ll[i] <- -2 * sum(stats::dnorm(
      y_original,
      mean = theta_original,
      sd = sqrt(sampling_variance_original),
      log = TRUE
    ))

    if (progress) {
      utils::setTxtProgressBar(pb, i)
    }
  }

  keep_cols <- .posterior_columns(n_iter, burn_in)
  keep_rows <- .posterior_rows(n_iter, burn_in)
  predictions <- response_center + response_scale *
    theta_out[, keep_cols, drop = FALSE]
  mean <- response_center + response_scale *
    xb_out[, keep_cols, drop = FALSE]
  coefficients <- response_scale * beta1_out[keep_rows, , drop = FALSE]
  random_effect_variance <- response_scale^2 * tau2_out[keep_rows]
  coefficient_variance <- response_scale^2 * sig2b_out[keep_rows]
  dic <- 2 * mean(ll[keep_rows]) +
    2 * sum(stats::dnorm(
      y_original,
      mean = rowMeans(predictions),
      sd = sqrt(sampling_variance_original),
      log = TRUE
    ))

  .new_nlfh_fit(
    c(
      list(
      predictions = predictions,
      random_effect_variance = random_effect_variance,
      coefficient_variance = coefficient_variance,
      coefficients = coefficients,
      mean = mean,
      dic = dic,
      method = "rnn",
      call = match.call(),
      n_iter = n_iter,
      burn_in = burn_in,
      n_hidden = n_hidden,
      response_center = response_center,
      response_scale = response_scale,
      progress = progress
      ),
      .fh_fit_metadata(input)
    ),
    "nlfh_rnn_fit"
  )
}
