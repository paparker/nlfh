.validate_response <- function(y) {
  if (!is.numeric(y) || !is.vector(y)) {
    stop("`y` must be a numeric vector.", call. = FALSE)
  }
  if (length(y) == 0L) {
    stop("`y` must contain at least one value.", call. = FALSE)
  }
  if (anyNA(y) || any(!is.finite(y))) {
    stop("`y` must contain only finite, non-missing values.", call. = FALSE)
  }
  y
}

.validate_covariates <- function(x, n) {
  if (is.data.frame(x)) {
    numeric_columns <- vapply(x, is.numeric, logical(1))
    if (!all(numeric_columns)) {
      stop("All columns in `x` must be numeric.", call. = FALSE)
    }
    x <- as.matrix(x)
  }
  if (!is.matrix(x) || !is.numeric(x)) {
    stop("`x` must be a numeric matrix or numeric data frame.", call. = FALSE)
  }
  if (nrow(x) != n) {
    stop("`x` must have one row for each value in `y`.", call. = FALSE)
  }
  if (ncol(x) == 0L) {
    stop("`x` must contain at least one covariate column.", call. = FALSE)
  }
  if (anyNA(x) || any(!is.finite(x))) {
    stop("`x` must contain only finite, non-missing values.", call. = FALSE)
  }
  x
}

.validate_sampling_variance <- function(sampling_variance, n) {
  if (!is.numeric(sampling_variance) || !is.vector(sampling_variance)) {
    stop("`sampling_variance` must be a numeric vector.", call. = FALSE)
  }
  if (length(sampling_variance) != n) {
    stop("`sampling_variance` must have the same length as `y`.", call. = FALSE)
  }
  if (anyNA(sampling_variance) || any(!is.finite(sampling_variance))) {
    stop("`sampling_variance` must contain only finite, non-missing values.", call. = FALSE)
  }
  if (any(sampling_variance <= 0)) {
    stop("`sampling_variance` values must be positive.", call. = FALSE)
  }
  sampling_variance
}

.validate_positive_scalar <- function(x, name) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) || x <= 0) {
    stop("`", name, "` must be a positive finite scalar.", call. = FALSE)
  }
  x
}

.validate_nonnegative_scalar <- function(x, name) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) || x < 0) {
    stop("`", name, "` must be a non-negative finite scalar.", call. = FALSE)
  }
  x
}

.validate_positive_integer <- function(x, name) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) ||
      x <= 0 || x != as.integer(x)) {
    stop("`", name, "` must be a positive integer.", call. = FALSE)
  }
  as.integer(x)
}

.validate_logical_scalar <- function(x, name) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop("`", name, "` must be `TRUE` or `FALSE`.", call. = FALSE)
  }
  x
}

.validate_mcmc <- function(n_iter, burn_in) {
  n_iter <- .validate_positive_integer(n_iter, "n_iter")
  burn_in <- .validate_positive_integer(burn_in, "burn_in")
  if (burn_in >= n_iter) {
    stop("`burn_in` must be smaller than `n_iter`.", call. = FALSE)
  }
  list(n_iter = n_iter, burn_in = burn_in)
}

.validate_control <- function(control, allowed, method) {
  if (is.null(control)) {
    control <- list()
  }
  if (!is.list(control)) {
    stop("`control` must be a named list.", call. = FALSE)
  }
  if (length(control) == 0L) {
    return(control)
  }
  if (is.null(names(control)) || any(names(control) == "")) {
    stop("All entries in `control` must be named.", call. = FALSE)
  }
  unknown <- setdiff(names(control), allowed)
  if (length(unknown) > 0L) {
    stop(
      "Unknown control parameter",
      if (length(unknown) > 1L) "s" else "",
      " for method `", method, "`: ",
      paste(unknown, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  control
}

.posterior_columns <- function(n_iter, burn_in) {
  seq.int(burn_in + 1L, n_iter)
}

.posterior_rows <- function(n_iter, burn_in) {
  seq.int(burn_in + 1L, n_iter)
}

.new_nlfh_fit <- function(x, subclass) {
  class(x) <- c(subclass, "nlfh_fit")
  x
}
