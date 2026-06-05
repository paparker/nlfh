test_that("formula interface works for the linear Fay-Herriot model", {
  dat <- test_data()
  fit <- fit_fh(
    y ~ x1 + x2,
    sampling_variance = vardir,
    data = dat,
    method = "linear",
    control = small_control()
  )

  expect_s3_class(fit, "nlfh_linear_fit")
  expect_identical(dim(fit$predictions), c(12L, 3L))
  expect_identical(fit$response_name, "y")
  expect_identical(fit$model_matrix_colnames, colnames(model.matrix(y ~ x1 + x2, dat)))
})

test_that("formula interface works for nonlinear methods", {
  dat <- test_data()

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

  expect_s3_class(rnn_fit, "nlfh_rnn_fit")
  expect_s3_class(bart_fit, "nlfh_bart_fit")
  expect_identical(dim(rnn_fit$predictions), c(12L, 3L))
  expect_identical(dim(bart_fit$predictions), c(12L, 3L))
})

test_that("fit_fh dispatches all model families from formula inputs", {
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

  expect_s3_class(linear_fit, "nlfh_linear_fit")
  expect_s3_class(rnn_fit, "nlfh_rnn_fit")
  expect_s3_class(bart_fit, "nlfh_bart_fit")
})

test_that("BART formula interface requires an intercept", {
  dat <- test_data()

  expect_error(
    fit_fh(
      y ~ 0 + x1 + x2,
      sampling_variance = vardir,
      data = dat,
      method = "bart",
      control = small_control(n_bart_samples = 1, n_trees = 1)
    ),
    "requires a formula intercept",
    fixed = TRUE
  )
  expect_error(
    fit_fh_bart(
      y ~ 0 + x1 + x2,
      sampling_variance = vardir,
      data = dat,
      n_bart_samples = 1,
      n_trees = 1,
      n_iter = 4,
      burn_in = 1,
      progress = FALSE
    ),
    "requires a formula intercept",
    fixed = TRUE
  )
})

test_that("matrix interface still works", {
  dat <- test_data()
  X <- model.matrix(y ~ x1 + x2, dat)
  fit <- fit_fh(
    y = dat$y,
    X = X,
    sampling_variance = dat$vardir,
    method = "linear",
    control = small_control()
  )

  expect_s3_class(fit, "nlfh_linear_fit")
  expect_identical(fit$interface, "matrix")
  expect_identical(fit$model_matrix_colnames, colnames(X))
})

test_that("formula and matrix interfaces match with the same X", {
  dat <- test_data()
  X <- model.matrix(y ~ x1 + x2, dat)

  set.seed(10)
  formula_fit <- fit_fh(
    y ~ x1 + x2,
    sampling_variance = vardir,
    data = dat,
    method = "linear",
    control = small_control()
  )
  set.seed(10)
  matrix_fit <- fit_fh(
    y = dat$y,
    X = X,
    sampling_variance = dat$vardir,
    method = "linear",
    control = small_control()
  )

  expect_equal(formula_fit$predictions, matrix_fit$predictions)
  expect_equal(formula_fit$dic, matrix_fit$dic)
})

test_that("y dot formula excludes explicit sampling variance column", {
  dat <- test_data()
  fit <- fit_fh(
    y ~ .,
    sampling_variance = vardir,
    data = dat,
    method = "rnn",
    control = small_control(n_hidden = 2)
  )

  expect_false("vardir" %in% fit$model_matrix_colnames)
  expect_true("groupb" %in% fit$model_matrix_colnames)
  expect_true("groupc" %in% fit$model_matrix_colnames)
  expect_identical(fit$sampling_variance_name, "vardir")
})

test_that("y dot formula excludes variables used in sampling variance expressions", {
  dat <- test_data()
  dat$MedIncSE <- sqrt(dat$vardir)
  dat$vardir <- NULL

  fit <- fit_fh(
    y ~ .,
    sampling_variance = MedIncSE^2,
    data = dat,
    method = "linear",
    control = small_control()
  )

  expect_false("MedIncSE" %in% fit$model_matrix_colnames)
  expect_identical(fit$sampling_variance_name, "MedIncSE")
  expect_identical(
    fit$model_matrix_colnames,
    colnames(model.matrix(y ~ . - MedIncSE, dat))
  )
})

test_that("factor predictors use model.matrix encoding", {
  dat <- test_data()
  fit <- fit_fh(
    y ~ group,
    sampling_variance = "vardir",
    data = dat,
    method = "linear",
    control = small_control()
  )

  expect_identical(fit$model_matrix_colnames, colnames(model.matrix(y ~ group, dat)))
})

test_that("helpful input errors are thrown", {
  dat <- test_data()

  expect_error(
    fit_fh(y ~ x1, sampling_variance = vardir, method = "linear"),
    "`data` is required",
    fixed = TRUE
  )
  expect_error(
    fit_fh(y ~ x1, data = dat, method = "linear"),
    "`sampling_variance` is required",
    fixed = TRUE
  )
  expect_error(
    fit_fh(y = dat$y, X = model.matrix(y ~ x1, dat), method = "linear"),
    "`sampling_variance` is required",
    fixed = TRUE
  )
  expect_error(
    fit_fh(y = dat$y, X = model.matrix(y ~ x1, dat), sampling_variance = rep(1, 5)),
    "same length as `y`",
    fixed = TRUE
  )
  expect_error(
    fit_fh(~ x1, sampling_variance = vardir, data = dat),
    "must include a response",
    fixed = TRUE
  )
})
