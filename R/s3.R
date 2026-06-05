#' @export
print.nlfh_fit <- function(x, ...) {
  cat("<", class(x)[1], ">\n", sep = "")
  if (!is.null(x$method)) {
    cat("Method: ", x$method, "\n", sep = "")
  }
  cat("Iterations: ", x$n_iter, "\n", sep = "")
  cat("Burn-in: ", x$burn_in, "\n", sep = "")
  if (!is.null(x$dic)) {
    cat("DIC: ", format(x$dic, digits = 4), "\n", sep = "")
  }
  invisible(x)
}

#' @export
summary.nlfh_fit <- function(object, ...) {
  out <- list(
    call = object$call,
    method = object$method,
    n_iter = object$n_iter,
    burn_in = object$burn_in,
    random_effect_variance = summary(object$random_effect_variance)
  )
  if (!is.null(object$dic)) {
    out$dic <- object$dic
  }
  if (!is.null(object$variable_importance)) {
    out$variable_importance <- object$variable_importance
  }
  class(out) <- "summary.nlfh_fit"
  out
}

#' @export
print.summary.nlfh_fit <- function(x, ...) {
  cat("<summary.nlfh_fit>\n")
  if (!is.null(x$method)) {
    cat("Method: ", x$method, "\n", sep = "")
  }
  cat("Iterations: ", x$n_iter, "\n", sep = "")
  cat("Burn-in: ", x$burn_in, "\n", sep = "")
  if (!is.null(x$dic)) {
    cat("DIC: ", format(x$dic, digits = 4), "\n", sep = "")
  }
  cat("\nRandom-effect variance:\n")
  print(x$random_effect_variance)
  if (!is.null(x$variable_importance)) {
    cat("\nVariable importance:\n")
    print(x$variable_importance)
  }
  invisible(x)
}
