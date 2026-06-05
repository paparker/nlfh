test_that("all three methods return nlfh_fit objects with finite DIC", {
  dat <- test_data()

  linear_fit <- fit_fh(
    y ~ x1 + x2,
    sampling_variance = vardir,
    data = dat,
    method = "linear",
    control = small_control()
  )
  rnn_fit <- fit_fh(
    y ~ x1 + x2,
    sampling_variance = vardir,
    data = dat,
    method = "rnn",
    control = small_control(n_hidden = 2)
  )
  bart_fit <- fit_fh(
    y ~ x1 + x2,
    sampling_variance = vardir,
    data = dat,
    method = "bart",
    control = small_control(n_bart_samples = 1, n_trees = 1)
  )

  fits <- list(linear = linear_fit, rnn = rnn_fit, bart = bart_fit)

  expect_true(all(vapply(fits, inherits, logical(1), "nlfh_fit")))
  expect_true(all(vapply(fits, function(x) is.finite(x$dic), logical(1))))
  expect_identical(linear_fit$method, "linear")
  expect_identical(rnn_fit$method, "rnn")
  expect_identical(bart_fit$method, "bart")
})

test_that("output dimensions are consistent across methods", {
  dat <- test_data()
  n_areas <- as.integer(nrow(dat))
  n_draws <- as.integer(small_control()$n_iter - small_control()$burn_in)

  linear_fit <- fit_fh(
    y ~ x1 + x2,
    sampling_variance = vardir,
    data = dat,
    method = "linear",
    control = small_control()
  )
  rnn_fit <- fit_fh(
    y ~ x1 + x2,
    sampling_variance = vardir,
    data = dat,
    method = "rnn",
    control = small_control(n_hidden = 2)
  )
  bart_fit <- fit_fh(
    y ~ x1 + x2,
    sampling_variance = vardir,
    data = dat,
    method = "bart",
    control = small_control(n_bart_samples = 1, n_trees = 1)
  )

  expect_identical(dim(linear_fit$predictions), c(n_areas, n_draws))
  expect_identical(dim(rnn_fit$predictions), c(n_areas, n_draws))
  expect_identical(dim(bart_fit$predictions), c(n_areas, n_draws))
  expect_length(linear_fit$random_effect_variance, n_draws)
  expect_length(rnn_fit$random_effect_variance, n_draws)
  expect_length(bart_fit$random_effect_variance, n_draws)
})

test_that("method dispatch selects the requested model implementation", {
  dat <- test_data()

  expect_s3_class(
    fit_fh(
      y ~ x1 + x2,
      sampling_variance = vardir,
      data = dat,
      method = "linear",
      control = small_control()
    ),
    "nlfh_linear_fit"
  )
  expect_s3_class(
    fit_fh(
      y ~ x1 + x2,
      sampling_variance = vardir,
      data = dat,
      method = "rnn",
      control = small_control(n_hidden = 2)
    ),
    "nlfh_rnn_fit"
  )
  expect_s3_class(
    fit_fh(
      y ~ x1 + x2,
      sampling_variance = vardir,
      data = dat,
      method = "bart",
      control = small_control(n_bart_samples = 1, n_trees = 1)
    ),
    "nlfh_bart_fit"
  )
})

test_that("method-specific fitting functions are public-facing", {
  dat <- test_data()

  expect_setequal(
    c("fit_fh_linear", "fit_fh_rnn", "fit_fh_bart"),
    intersect(
      c("fit_fh_linear", "fit_fh_rnn", "fit_fh_bart"),
      getNamespaceExports("nlfh")
    )
  )

  expect_s3_class(
    fit_fh_linear(
      y ~ x1 + x2,
      sampling_variance = vardir,
      data = dat,
      n_iter = 4,
      burn_in = 1,
      progress = FALSE
    ),
    "nlfh_linear_fit"
  )
  expect_s3_class(
    fit_fh_rnn(
      y ~ x1 + x2,
      sampling_variance = vardir,
      data = dat,
      n_hidden = 2,
      n_iter = 4,
      burn_in = 1,
      progress = FALSE
    ),
    "nlfh_rnn_fit"
  )
  expect_s3_class(
    fit_fh_bart(
      y ~ x1 + x2,
      sampling_variance = vardir,
      data = dat,
      n_bart_samples = 1,
      n_trees = 1,
      n_iter = 4,
      burn_in = 1,
      progress = FALSE
    ),
    "nlfh_bart_fit"
  )
})

test_that("control validation rejects unknown controls", {
  dat <- test_data()

  expect_error(
    fit_fh(
      y ~ x1 + x2,
      sampling_variance = vardir,
      data = dat,
      method = "linear",
      control = list(not_a_control = 1)
    ),
    "Unknown control parameter",
    fixed = TRUE
  )
})

test_that("random-effect variance prior controls are accepted for all methods", {
  dat <- test_data()

  linear_fit <- fit_fh(
    y ~ x1 + x2,
    sampling_variance = vardir,
    data = dat,
    method = "linear",
    control = small_control(prior_shape = 0.5, prior_rate = 0.25)
  )
  rnn_fit <- fit_fh(
    y ~ x1 + x2,
    sampling_variance = vardir,
    data = dat,
    method = "rnn",
    control = small_control(
      n_hidden = 2,
      prior_shape = 0.5,
      prior_rate = 0.25
    )
  )
  bart_fit <- fit_fh(
    y ~ x1 + x2,
    sampling_variance = vardir,
    data = dat,
    method = "bart",
    control = small_control(
      prior_shape = 0.5,
      prior_rate = 0.25,
      n_bart_samples = 1,
      n_trees = 1
    )
  )

  expect_s3_class(linear_fit, "nlfh_linear_fit")
  expect_s3_class(rnn_fit, "nlfh_rnn_fit")
  expect_s3_class(bart_fit, "nlfh_bart_fit")
})

test_that("RNN output-layer prior variance can be fixed", {
  dat <- test_data()
  fit <- fit_fh(
    y ~ x1 + x2,
    sampling_variance = vardir,
    data = dat,
    method = "rnn",
    control = small_control(n_hidden = 2, prior_beta_variance = 2.5)
  )

  expect_equal(fit$response_center, mean(dat$y))
  expect_equal(fit$response_scale, stats::sd(dat$y))
  expect_equal(fit$coefficient_variance, rep(2.5 * stats::var(dat$y), 3L))
})

test_that("RNN scales non-intercept covariates by default", {
  dat <- test_data()
  X <- model.matrix(y ~ x1 + x2, dat)
  scale_cols <- 2:3
  centers <- colMeans(X[, scale_cols, drop = FALSE])
  scales <- apply(X[, scale_cols, drop = FALSE], 2L, stats::sd)
  X_scaled <- X
  X_scaled[, scale_cols] <- base::scale(
    X[, scale_cols, drop = FALSE],
    center = centers,
    scale = scales
  )

  set.seed(20)
  default_fit <- fit_fh(
    y ~ x1 + x2,
    sampling_variance = vardir,
    data = dat,
    method = "rnn",
    control = small_control(n_hidden = 2)
  )
  set.seed(20)
  manual_fit <- fit_fh_rnn(
    y = dat$y,
    X = X_scaled,
    sampling_variance = dat$vardir,
    n_hidden = 2,
    n_iter = 4,
    burn_in = 1,
    scale = FALSE,
    progress = FALSE
  )

  expect_true(default_fit$scale)
  expect_equal(default_fit$covariate_center, centers)
  expect_equal(default_fit$covariate_scale, scales)
  expect_equal(default_fit$predictions, manual_fit$predictions)
})

test_that("invalid inputs error clearly", {
  dat <- test_data()

  expect_error(
    fit_fh(y ~ x1 + x2, data = dat, method = "linear"),
    "`sampling_variance` is required",
    fixed = TRUE
  )
  expect_error(
    fit_fh(
      y ~ x1 + x2,
      sampling_variance = rep(1, nrow(dat) - 1),
      data = dat,
      method = "linear"
    ),
    "`sampling_variance` must have the same length as `y`",
    fixed = TRUE
  )
  expect_error(
    fit_fh(
      y ~ x1 + x2,
      sampling_variance = rep(-1, nrow(dat)),
      data = dat,
      method = "linear"
    ),
    "`sampling_variance` values must be positive",
    fixed = TRUE
  )
  expect_error(
    fit_fh(y = dat$y, sampling_variance = dat$vardir, method = "linear"),
    "`x` or `X` is required",
    fixed = TRUE
  )
})
