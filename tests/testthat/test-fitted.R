test_that("fitted.nlfh_fit returns posterior means by default", {
  dat <- test_data()
  fit <- fit_fh(
    y ~ x1 + x2,
    sampling_variance = vardir,
    data = dat,
    method = "linear",
    control = small_control()
  )

  out <- fitted(fit)

  expect_type(out, "double")
  expect_length(out, nrow(dat))
  expect_named(out, as.character(seq_len(nrow(dat))))
  expect_equal(unname(out), rowMeans(fit$predictions))
})

test_that("fitted.nlfh_fit can return posterior medians", {
  dat <- test_data()
  fit <- fit_fh(
    y ~ x1 + x2,
    sampling_variance = vardir,
    data = dat,
    method = "linear",
    control = small_control()
  )

  out <- fitted(fit, statistic = "median")

  expect_equal(unname(out), apply(fit$predictions, 1L, stats::median))
})

test_that("fitted.nlfh_fit can return full posterior summaries", {
  dat <- test_data()
  fit <- fit_fh(
    y ~ x1 + x2,
    sampling_variance = vardir,
    data = dat,
    method = "linear",
    control = small_control()
  )

  out <- fitted(fit, full = TRUE)

  expect_s3_class(out, "data.frame")
  expect_named(out, c("area", "mean", "sd", "median", "q2.5", "q97.5"))
  expect_identical(nrow(out), nrow(dat))
  expect_equal(out$mean, rowMeans(fit$predictions))
})

test_that("fitted.nlfh_fit can return posterior draws", {
  dat <- test_data()
  fit <- fit_fh(
    y ~ x1 + x2,
    sampling_variance = vardir,
    data = dat,
    method = "linear",
    control = small_control()
  )

  out <- fitted(fit, summary = FALSE)

  expect_equal(unname(out), fit$predictions)
  expect_identical(rownames(out), as.character(seq_len(nrow(dat))))
})

test_that("fitted.nlfh_fit preserves existing area names", {
  dat <- test_data()
  rownames(dat) <- paste0("area", seq_len(nrow(dat)))
  fit <- fit_fh(
    y ~ x1 + x2,
    sampling_variance = vardir,
    data = dat,
    method = "linear",
    control = small_control()
  )
  rownames(fit$predictions) <- rownames(dat)

  out <- fitted(fit)

  expect_named(out, rownames(dat))
})
