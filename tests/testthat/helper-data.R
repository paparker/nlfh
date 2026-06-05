test_data <- function() {
  x1 <- rep(c(0, 1), 6)
  x2 <- seq(-1.5, 1.5, length.out = 12)
  data.frame(
    y = 1 + 0.5 * x1 + sin(x2) + c(
      0.2, -0.1, 0.05, 0.3, -0.2, 0.15,
      -0.05, 0.1, -0.15, 0.25, -0.3, 0.05
    ),
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
