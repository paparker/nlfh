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
