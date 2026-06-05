test_that("print.nlfh_fit gives a concise model overview", {
  dat <- test_data()
  fit <- fit_fh(
    y ~ x1 + x2,
    sampling_variance = vardir,
    data = dat,
    method = "linear",
    control = small_control()
  )

  output <- capture.output(print(fit))

  expect_true(any(grepl("Model type: linear Fay-Herriot", output, fixed = TRUE)))
  expect_true(any(grepl("Formula: y ~ x1 + x2", output, fixed = TRUE)))
  expect_true(any(grepl("Areas: 12", output, fixed = TRUE)))
  expect_true(any(grepl("Posterior draws: 3", output, fixed = TRUE)))
  expect_true(any(grepl("MCMC: n_iter = 4, burn_in = 1", output, fixed = TRUE)))
  expect_true(any(grepl("Variance components:", output, fixed = TRUE)))
  expect_true(any(grepl("random-effect variance: median =", output, fixed = TRUE)))
  expect_true(any(grepl("Next: use summary(), plot(), fitted(), or posterior_draws()", output, fixed = TRUE)))
})

test_that("print.nlfh_fit handles matrix-interface fits", {
  dat <- test_data()
  X <- model.matrix(y ~ x1 + x2, dat)
  fit <- fit_fh(
    y = dat$y,
    X = X,
    sampling_variance = dat$vardir,
    method = "linear",
    control = small_control()
  )

  output <- capture.output(print(fit))

  expect_true(any(grepl("Formula: matrix interface", output, fixed = TRUE)))
  expect_true(any(grepl("Areas: 12", output, fixed = TRUE)))
  expect_true(any(grepl("Posterior draws: 3", output, fixed = TRUE)))
})
