test_that("posterior_draws extracts theta draws", {
  dat <- test_data()
  fit <- fit_fh(
    y ~ x1 + x2,
    sampling_variance = vardir,
    data = dat,
    method = "linear",
    control = small_control()
  )

  draws <- posterior_draws(fit, variable = "theta")

  expect_s3_class(draws, "draws_df")
  expect_identical(posterior::ndraws(draws), ncol(fit$predictions))
  expect_true("theta[1]" %in% posterior::variables(draws))
  expect_true("theta[12]" %in% posterior::variables(draws))
})

test_that("posterior_draws extracts beta draws when available", {
  dat <- test_data()
  fit <- fit_fh(
    y ~ x1 + x2,
    sampling_variance = vardir,
    data = dat,
    method = "linear",
    control = small_control()
  )

  draws <- posterior_draws(fit, variable = "beta")

  expect_s3_class(draws, "draws_df")
  expect_identical(posterior::ndraws(draws), nrow(fit$coefficients))
  expect_true("beta[(Intercept)]" %in% posterior::variables(draws))
  expect_true("beta[x1]" %in% posterior::variables(draws))
})

test_that("posterior_draws extracts A draws", {
  dat <- test_data()
  fit <- fit_fh(
    y ~ x1 + x2,
    sampling_variance = vardir,
    data = dat,
    method = "linear",
    control = small_control()
  )

  draws <- posterior_draws(fit, variable = "A")

  expect_s3_class(draws, "draws_df")
  expect_identical(posterior::variables(draws), "A")
  expect_equal(as.numeric(draws$A), fit$random_effect_variance)
})

test_that("posterior_draws extracts u draws when available", {
  dat <- test_data()
  fit <- fit_fh(
    y ~ x1 + x2,
    sampling_variance = vardir,
    data = dat,
    method = "bart",
    control = small_control(n_bart_samples = 1, n_trees = 1)
  )

  draws <- posterior_draws(fit, variable = "u")

  expect_s3_class(draws, "draws_df")
  expect_identical(posterior::ndraws(draws), ncol(fit$random_effects))
  expect_true("u[1]" %in% posterior::variables(draws))
})

test_that("posterior_draws gives helpful errors for unavailable variables", {
  dat <- test_data()
  linear_fit <- fit_fh(
    y ~ x1 + x2,
    sampling_variance = vardir,
    data = dat,
    method = "linear",
    control = small_control()
  )
  bart_fit <- fit_fh(
    y ~ x1 + x2,
    sampling_variance = vardir,
    data = dat,
    method = "bart",
    control = small_control(n_bart_samples = 1, n_trees = 1)
  )

  expect_error(
    posterior_draws(linear_fit, variable = "u"),
    "not available",
    fixed = TRUE
  )
  expect_error(
    posterior_draws(bart_fit, variable = "beta"),
    "not available",
    fixed = TRUE
  )
})
