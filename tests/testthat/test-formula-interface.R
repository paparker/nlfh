test_data <- function() {
  x1 <- rep(c(0, 1), 6)
  x2 <- seq(-1.5, 1.5, length.out = 12)
  data.frame(
    y = 1 + 0.5 * x1 + sin(x2) + c(0.2, -0.1, 0.05, 0.3, -0.2, 0.15,
                                   -0.05, 0.1, -0.15, 0.25, -0.3, 0.05),
    x1 = x1,
    x2 = x2,
    group = factor(rep(c("a", "b", "c"), 4)),
    vardir = rep(0.5, 12)
  )
}

small_control <- function(...) {
  utils::modifyList(
    list(n_iter = 4, burn_in = 1, progress = FALSE),
    list(...)
  )
}

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

test_that("method-specific exported functions accept formula inputs", {
  dat <- test_data()

  linear_fit <- fit_fh_linear(
    y ~ x1 + x2,
    sampling_variance = vardir,
    data = dat,
    n_iter = 4,
    burn_in = 1,
    progress = FALSE
  )
  rnn_fit <- fit_fh_rnn(
    y ~ x1 + x2,
    sampling_variance = vardir,
    data = dat,
    n_hidden = 2,
    n_iter = 4,
    burn_in = 1,
    progress = FALSE
  )
  bart_fit <- fit_fh_bart(
    y ~ x1 + x2,
    sampling_variance = vardir,
    data = dat,
    n_bart_samples = 1,
    n_trees = 1,
    n_iter = 4,
    burn_in = 1,
    progress = FALSE
  )

  expect_s3_class(linear_fit, "nlfh_linear_fit")
  expect_s3_class(rnn_fit, "nlfh_rnn_fit")
  expect_s3_class(bart_fit, "nlfh_bart_fit")
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
