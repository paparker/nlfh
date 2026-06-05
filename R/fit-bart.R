#' Fit a BART Fay-Herriot Model
#'
#' Fits a Bayesian Fay-Herriot model whose mean function is represented with
#' Bayesian additive regression trees via the `dbarts` package. The sampling
#' model, priors, and MCMC updates match the original `BARTFH()` implementation.
#'
#' @param formula Optional model formula such as `y ~ x1 + x2`. For nonlinear
#'   models, the formula specifies the predictors available to the model; it
#'   does not imply an additive linear mean structure.
#' @param data Optional data frame containing variables used by `formula` and,
#'   optionally, `sampling_variance`.
#' @param y Numeric vector of area-level direct estimates for the matrix
#'   interface. If the first argument is a formula, it is treated as `formula`.
#' @param x,X Numeric matrix or data frame of area-level covariates for the
#'   matrix interface. Rows must correspond to entries of `y`.
#' @param sampling_variance Numeric vector of known sampling variances for `y`.
#'   With the formula interface, this may also be an unquoted column name from
#'   `data` or a length-one character string naming a column in `data`.
#' @param prior_shape Non-negative scalar shape parameter for the inverse-gamma
#'   prior on the random-effect variance.
#' @param prior_rate Non-negative scalar rate parameter for the inverse-gamma
#'   prior on the random-effect variance.
#' @param n_iter Positive integer number of MCMC iterations.
#' @param burn_in Positive integer number of initial MCMC iterations to discard.
#' @param n_bart_samples Positive integer number of BART samples to draw per
#'   outer MCMC iteration.
#' @param n_trees Positive integer number of trees used by `dbarts`.
#' @param progress Logical; if `TRUE`, display a progress bar.
#' @param D Legacy argument name for `sampling_variance`.
#' @param a,b Legacy argument names for `prior_shape` and `prior_rate`.
#' @param n.iter,n.burn Legacy argument names for `n_iter` and `burn_in`.
#' @param n.bart.samples Legacy argument name for `n_bart_samples`.
#' @param n.tree Legacy argument name for `n_trees`.
#'
#' @return An object of class `nlfh_bart_fit` and `nlfh_fit`, a list with
#'   posterior draws for `predictions`, the BART mean function `mean`, random effects
#'   `random_effects`, random-effect variance `random_effect_variance`,
#'   `variable_importance`, the scalar `dic`, and MCMC metadata.
#'
#' @references
#' Parker, P. A. and Eideh, A. (2026). BART-FH: Flexible Nonlinear Modeling for
#'   Small Area Estimation. *Journal of Survey Statistics and Methodology*,
#'   00, 1-18. \doi{10.1093/jssam/smaf050}
#'
#' @details
#' Formula inputs are parsed with [stats::model.frame()] and
#' [stats::model.matrix()]. Factors are expanded using R's standard contrast and
#' dummy-variable rules. An intercept is included when the formula includes one,
#' which is the default; matrix inputs are used as supplied. For this nonlinear
#' method, the formula specifies the available predictors and does not impose an
#' additive linear mean structure. The BART mean component estimates an unknown
#' function `f(X)`.
#' @export
fit_fh_bart <- function(y = NULL, x = NULL, sampling_variance = NULL,
                        formula = NULL, data = NULL, X = NULL,
                        prior_shape = 0.01, prior_rate = 0.01,
                        n_iter = 1000, burn_in = 500, n_bart_samples = 10,
                        n_trees = 50, progress = TRUE) {
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
  y <- input$y
  x <- input$X
  sampling_variance <- input$vardir
  prior_shape <- .validate_nonnegative_scalar(prior_shape, "prior_shape")
  prior_rate <- .validate_nonnegative_scalar(prior_rate, "prior_rate")
  mcmc <- .validate_mcmc(n_iter, burn_in)
  n_iter <- mcmc$n_iter
  burn_in <- mcmc$burn_in
  n_bart_samples <- .validate_positive_integer(n_bart_samples, "n_bart_samples")
  n_trees <- .validate_positive_integer(n_trees, "n_trees")
  progress <- .validate_logical_scalar(progress, "progress")

  n <- length(y)
  p <- ncol(x)
  u <- stats::rnorm(n, sd = 0.01)
  sigma_u2 <- 1

  varcount_iter <- NULL
  varprob_iter <- NULL
  u_samples <- matrix(NA_real_, nrow = n_iter, ncol = n)
  sigma_u2_samples <- numeric(n_iter)
  f_samples <- matrix(NA_real_, nrow = n_iter, ncol = n)
  theta_samples <- matrix(NA_real_, nrow = n_iter, ncol = n)
  ll <- rep(NA_real_, n_iter)

  bart_control <- dbarts::dbartsControl(
    updateState = FALSE,
    n.samples = n_bart_samples,
    n.burn = 0,
    n.trees = n_trees,
    n.chains = 1L
  )

  bart_sampler <- dbarts::dbarts(
    formula = y ~ .,
    data = data.frame(y = y, x),
    weights = 1 / sampling_variance,
    control = bart_control,
    verbose = FALSE
  )
  if (progress) {
    pb <- utils::txtProgressBar(min = 0, max = n_iter, style = 3)
    on.exit(close(pb), add = TRUE)
  }
  for (iter in seq_len(n_iter)) {
    y_star <- y - u
    bart_sampler$setResponse(y_star)

    bart_fit <- bart_sampler$run()
    vc <- bart_fit$varcount
    vc_sum <- rowSums(vc)
    if (is.null(varcount_iter)) {
      var_names <- rownames(vc)
      if (is.null(var_names)) {
        var_names <- colnames(x)[seq_along(vc_sum)]
      }
      varcount_iter <- matrix(
        NA_real_,
        nrow = n_iter,
        ncol = length(vc_sum),
        dimnames = list(NULL, var_names)
      )
      varprob_iter <- varcount_iter
    }
    varcount_iter[iter, ] <- vc_sum
    varprob_iter[iter, ] <- if (sum(vc_sum) > 0) {
      vc_sum / sum(vc_sum)
    } else {
      rep(0, length(vc_sum))
    }

    f_xi_samples <- bart_fit$train
    f_samples[iter, ] <- rowMeans(f_xi_samples)
    f_xi <- rowMeans(f_xi_samples)

    for (i in seq_len(n)) {
      mu_i <- ((y[i] - f_xi[i]) * sigma_u2) / (sampling_variance[i] + sigma_u2)
      var_u_i <- (sampling_variance[i] * sigma_u2) /
        (sampling_variance[i] + sigma_u2)
      u[i] <- stats::rnorm(1, mean = mu_i, sd = sqrt(var_u_i))
    }

    u_samples[iter, ] <- u
    theta <- f_xi + u
    theta_samples[iter, ] <- theta

    shape <- prior_shape + n / 2
    rate <- prior_rate + sum(u^2) / 2
    sigma_u2 <- 1 / stats::rgamma(1, shape, rate)
    sigma_u2_samples[iter] <- sigma_u2
    ll[iter] <- -2 * sum(stats::dnorm(
      y,
      mean = theta,
      sd = sqrt(sampling_variance),
      log = TRUE
    ))

    if (progress) {
      utils::setTxtProgressBar(pb, iter)
    }
  }

  keep_rows <- .posterior_rows(n_iter, burn_in)
  dic <- 2 * mean(ll[keep_rows]) +
    2 * sum(stats::dnorm(
      y,
      mean = colMeans(theta_samples[keep_rows, , drop = FALSE]),
      sd = sqrt(sampling_variance),
      log = TRUE
    ))
  .new_nlfh_fit(
    c(
      list(
      predictions = t(theta_samples[keep_rows, , drop = FALSE]),
      mean = t(f_samples[keep_rows, , drop = FALSE]),
      random_effects = t(u_samples[keep_rows, , drop = FALSE]),
      random_effect_variance = sigma_u2_samples[keep_rows],
      variable_importance = colMeans(varprob_iter[keep_rows, , drop = FALSE]),
      dic = dic,
      method = "bart",
      call = match.call(),
      n_iter = n_iter,
      burn_in = burn_in,
      n_bart_samples = n_bart_samples,
      n_trees = n_trees,
      progress = progress
      ),
      .fh_fit_metadata(input)
    ),
    "nlfh_bart_fit"
  )
}

#' @rdname fit_fh_bart
#' @export
BARTFH <- function(y, x, D, a = 0.01, b = 0.01, n.iter = 1000,
                   n.burn = 500, n.bart.samples = 10, n.tree = 50) {
  fit_fh_bart(
    y = y,
    x = x,
    sampling_variance = D,
    prior_shape = a,
    prior_rate = b,
    n_iter = n.iter,
    burn_in = n.burn,
    n_bart_samples = n.bart.samples,
    n_trees = n.tree
  )
}
