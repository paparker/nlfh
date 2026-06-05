test_that("summary.nlfh_fit returns a structured summary object", {
  dat <- test_data()
  fit <- fit_fh(
    y ~ x1 + x2,
    sampling_variance = vardir,
    data = dat,
    method = "linear",
    control = small_control()
  )

  out <- summary(fit)

  expect_s3_class(out, "summary.nlfh_fit")
  expect_s3_class(out, "summary")
  expect_identical(out$method, "linear")
  expect_identical(out$model_type, "linear Fay-Herriot")
  expect_equal(out$formula, y ~ x1 + x2)
  expect_named(
    out,
    c(
      "call", "method", "formula", "model_type", "mcmc", "diagnostics",
      "coefficients", "variance", "areas", "dic", "variable_importance",
      "model_matrix_colnames", "response_name", "predictor_names", "interface"
    )
  )
})

test_that("summary.nlfh_fit summarizes coefficients, variance, and areas", {
  dat <- test_data()
  fit <- fit_fh(
    y ~ x1 + x2,
    sampling_variance = vardir,
    data = dat,
    method = "linear",
    control = small_control()
  )

  out <- summary(fit)
  expected_columns <- c("parameter", "mean", "sd", "median", "q2.5", "q97.5")

  expect_named(out$coefficients, expected_columns)
  expect_identical(out$coefficients$parameter, c("(Intercept)", "x1", "x2"))
  expect_named(out$variance, expected_columns)
  expect_true("random_effect_variance" %in% out$variance$parameter)
  expect_named(out$areas, c("area", "mean", "sd", "median", "q2.5", "q97.5"))
  expect_identical(nrow(out$areas), 12L)
  expect_true(all(is.finite(out$areas$mean)))
})

test_that("summary.nlfh_fit includes method-specific diagnostics", {
  dat <- test_data()
  fit <- fit_fh(
    y ~ x1 + x2,
    sampling_variance = vardir,
    data = dat,
    method = "rnn",
    control = small_control(n_hidden = 2)
  )

  out <- summary(fit)

  expect_true("coefficient_variance" %in% out$variance$parameter)
  expect_identical(out$diagnostics$n_hidden, 2L)
  expect_identical(out$coefficients$parameter, c("hidden1", "hidden2"))
})

test_that("summary.nlfh_fit handles BART fits without coefficient summaries", {
  dat <- test_data()
  fit <- fit_fh(
    y ~ x1 + x2,
    sampling_variance = vardir,
    data = dat,
    method = "bart",
    control = small_control(n_bart_samples = 1, n_trees = 1)
  )

  out <- summary(fit)

  expect_null(out$coefficients)
  expect_true("random_effect_variance" %in% out$variance$parameter)
  expect_identical(out$diagnostics$n_trees, 1L)
  expect_identical(out$diagnostics$n_bart_samples, 1L)
  expect_false(is.null(out$variable_importance))
  expect_identical(names(out$variable_importance), c("x1", "x2"))
})

test_that("print.summary.nlfh_fit prints a compact summary", {
  dat <- test_data()
  fit <- fit_fh(
    y ~ x1 + x2,
    sampling_variance = vardir,
    data = dat,
    method = "linear",
    control = small_control()
  )

  output <- capture.output(print(summary(fit), max_rows = 2))

  expect_true(any(grepl("<summary.nlfh_fit>", output, fixed = TRUE)))
  expect_true(any(grepl("Details:", output, fixed = TRUE)))
  expect_true(any(grepl("Variance parameters:", output, fixed = TRUE)))
  expect_true(any(grepl("Coefficients:", output, fixed = TRUE)))
  expect_true(any(grepl("Area-level estimates theta_i:", output, fixed = TRUE)))
  expect_true(any(grepl("... 10 more rows", output, fixed = TRUE)))
})
