#' Print a Fitted Nonlinear Fay-Herriot Model
#'
#' Provides a compact overview of a fitted `nlfh_fit` object. The print method
#' reports the model type, formula or input interface, number of areas, number
#' of posterior draws, MCMC settings, a short variance-component summary, and
#' DIC when available. It intentionally avoids printing posterior draw matrices,
#' coefficient tables, or other large objects.
#'
#' @param x An object inheriting from class `nlfh_fit`.
#' @param ... Additional arguments passed to methods. Currently unused.
#'
#' @return The input object `x`, invisibly.
#' @export
print.nlfh_fit <- function(x, ...) {
  model_type <- .nlfh_model_type(x)
  n_areas <- .nlfh_n_areas(x)
  n_draws <- .nlfh_n_draws(x)

  cat("<", class(x)[1], ">\n", sep = "")
  cat("Model type: ", model_type, "\n", sep = "")
  cat("Formula: ", .nlfh_formula_label(x), "\n", sep = "")
  cat("Areas: ", .nlfh_format_count(n_areas), "\n", sep = "")
  cat("Posterior draws: ", .nlfh_format_count(n_draws), "\n", sep = "")

  mcmc <- .nlfh_mcmc_label(x)
  if (!is.null(mcmc)) {
    cat("MCMC: ", mcmc, "\n", sep = "")
  }

  variance_components <- .nlfh_variance_components(x)
  if (length(variance_components) > 0L) {
    cat("Variance components:\n")
    for (component in variance_components) {
      cat("  ", component, "\n", sep = "")
    }
  }

  if (!is.null(x$dic)) {
    cat("DIC: ", format(x$dic, digits = 4), "\n", sep = "")
  }

  cat(
    "Next: use summary(), plot(), fitted(), or ",
    "posterior_draws() for more detail.\n",
    sep = ""
  )
  invisible(x)
}

#' Extract Fitted Values from a Nonlinear Fay-Herriot Model
#'
#' For fitted `nlfh_fit` objects, fitted values are posterior summaries of the
#' area-level quantities `theta_i`. The posterior draws are stored in the fitted
#' object as an `n_areas` by `n_draws` matrix.
#'
#' @param object An object inheriting from class `nlfh_fit`.
#' @param statistic Character string selecting the posterior point summary to
#'   return when `summary = TRUE`. Options are `"mean"` and `"median"`.
#' @param summary Logical. If `TRUE`, return posterior summaries. If `FALSE`,
#'   return the posterior draw matrix for `theta_i`.
#' @param full Logical. If `TRUE` and `summary = TRUE`, return a data frame with
#'   posterior mean, standard deviation, median, 2.5% quantile, and 97.5%
#'   quantile for each area.
#' @param ... Additional arguments passed to methods. Currently unused.
#'
#' @return If `summary = TRUE` and `full = FALSE`, a named numeric vector of
#'   posterior means or medians. If `summary = TRUE` and `full = TRUE`, a data
#'   frame of posterior summaries. If `summary = FALSE`, the posterior draw
#'   matrix for `theta_i`.
#' @export
fitted.nlfh_fit <- function(object, statistic = c("mean", "median"),
                            summary = TRUE, full = FALSE, ...) {
  if (is.null(object$predictions)) {
    stop("Posterior draws of `theta_i` are not available.", call. = FALSE)
  }
  draws <- object$predictions
  if (!is.matrix(draws)) {
    draws <- as.matrix(draws)
  }
  area_names <- .nlfh_area_names(draws)

  if (!summary) {
    rownames(draws) <- area_names
    return(draws)
  }

  if (full) {
    out <- .nlfh_area_summary(object)
    out$area <- area_names
    return(out)
  }

  statistic <- match.arg(statistic)
  out <- switch(
    statistic,
    mean = rowMeans(draws),
    median = apply(draws, 1L, stats::median)
  )
  names(out) <- area_names
  out
}

.nlfh_area_names <- function(draws) {
  rownames(draws) %||% as.character(seq_len(nrow(draws)))
}

.nlfh_model_type <- function(x) {
  switch(
    x$method %||% "",
    linear = "linear Fay-Herriot",
    rnn = "random-weight neural network Fay-Herriot",
    bart = "BART Fay-Herriot",
    class(x)[1]
  )
}

.nlfh_formula_label <- function(x) {
  if (!is.null(x$formula)) {
    return(paste(deparse(x$formula), collapse = " "))
  }
  if (!is.null(x$interface) && identical(x$interface, "matrix")) {
    return("matrix interface")
  }
  "not stored"
}

.nlfh_n_areas <- function(x) {
  if (!is.null(x$predictions) && length(dim(x$predictions)) == 2L) {
    return(nrow(x$predictions))
  }
  if (!is.null(x$mean) && length(dim(x$mean)) == 2L) {
    return(nrow(x$mean))
  }
  NA_integer_
}

.nlfh_n_draws <- function(x) {
  if (!is.null(x$predictions) && length(dim(x$predictions)) == 2L) {
    return(ncol(x$predictions))
  }
  if (!is.null(x$mean) && length(dim(x$mean)) == 2L) {
    return(ncol(x$mean))
  }
  if (!is.null(x$random_effect_variance)) {
    return(length(x$random_effect_variance))
  }
  NA_integer_
}

.nlfh_mcmc_label <- function(x) {
  fields <- character()
  if (!is.null(x$n_iter)) {
    fields <- c(fields, paste0("n_iter = ", x$n_iter))
  }
  if (!is.null(x$burn_in)) {
    fields <- c(fields, paste0("burn_in = ", x$burn_in))
  }
  if (!is.null(x$n_hidden)) {
    fields <- c(fields, paste0("n_hidden = ", x$n_hidden))
  }
  if (!is.null(x$n_trees)) {
    fields <- c(fields, paste0("n_trees = ", x$n_trees))
  }
  if (!is.null(x$n_bart_samples)) {
    fields <- c(fields, paste0("n_bart_samples = ", x$n_bart_samples))
  }
  if (!is.null(x$progress)) {
    fields <- c(fields, paste0("progress = ", x$progress))
  }
  if (length(fields) == 0L) {
    return(NULL)
  }
  paste(fields, collapse = ", ")
}

.nlfh_variance_components <- function(x) {
  components <- character()
  if (!is.null(x$random_effect_variance)) {
    components <- c(
      components,
      .nlfh_variance_component_label(
        x$random_effect_variance,
        "random-effect variance"
      )
    )
  }
  if (!is.null(x$coefficient_variance)) {
    components <- c(
      components,
      .nlfh_variance_component_label(
        x$coefficient_variance,
        "coefficient variance"
      )
    )
  }
  components
}

.nlfh_variance_component_label <- function(x, label) {
  x <- x[is.finite(x)]
  if (length(x) == 0L) {
    return(paste0(label, ": unavailable"))
  }
  qs <- stats::quantile(x, probs = c(0.025, 0.5, 0.975), names = FALSE)
  paste0(
    label,
    ": median = ", format(qs[2L], digits = 4),
    ", 95% CrI [", format(qs[1L], digits = 4),
    ", ", format(qs[3L], digits = 4), "]"
  )
}

.nlfh_format_count <- function(x) {
  if (length(x) != 1L || is.na(x)) {
    return("unknown")
  }
  format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
}

`%||%` <- function(x, y) {
  if (is.null(x)) {
    y
  } else {
    x
  }
}

#' Summarize a Fitted Nonlinear Fay-Herriot Model
#'
#' Computes posterior summaries from the MCMC draws stored in a fitted
#' `nlfh_fit` object. The returned object is structured for downstream use and
#' is not printed directly by `summary()`.
#'
#' Posterior summaries include the mean, standard deviation, median, 2.5%
#' quantile, and 97.5% quantile. Summaries are computed for area-level
#' estimates `theta_i`, variance parameters, and regression or hidden-layer
#' coefficients when those draws are available.
#'
#' @param object An object inheriting from class `nlfh_fit`.
#' @param ... Additional arguments passed to methods. Currently unused.
#'
#' @return An object with classes `summary.nlfh_fit` and `summary`, containing:
#' \describe{
#'   \item{call}{Original model call.}
#'   \item{method}{Model fitting method.}
#'   \item{formula}{Formula, if the formula interface was used.}
#'   \item{model_type}{Human-readable model type.}
#'   \item{mcmc}{Stored MCMC settings and draw counts.}
#'   \item{diagnostics}{Available MCMC diagnostics and metadata.}
#'   \item{coefficients}{Posterior summaries of coefficient draws, if present.}
#'   \item{variance}{Posterior summaries of variance parameters.}
#'   \item{areas}{Posterior summaries of area-level estimates `theta_i`.}
#'   \item{dic}{DIC, if available.}
#'   \item{variable_importance}{BART variable-importance proportions, if
#'   available. The first model-matrix column is treated as a baseline/intercept
#'   column and is excluded.}
#' }
#' @export
summary.nlfh_fit <- function(object, ...) {
  n_areas <- .nlfh_n_areas(object)
  n_draws <- .nlfh_n_draws(object)
  out <- list(
    call = object$call,
    method = object$method,
    formula = object$formula,
    model_type = .nlfh_model_type(object),
    mcmc = .nlfh_mcmc_summary(object, n_areas = n_areas, n_draws = n_draws),
    diagnostics = .nlfh_mcmc_diagnostics(object, n_draws = n_draws),
    coefficients = .nlfh_coefficient_summary(object),
    variance = .nlfh_variance_summary(object),
    areas = .nlfh_area_summary(object),
    dic = object$dic,
    variable_importance = object$variable_importance,
    model_matrix_colnames = object$model_matrix_colnames,
    response_name = object$response_name,
    predictor_names = object$predictor_names,
    interface = object$interface
  )
  class(out) <- c("summary.nlfh_fit", "summary")
  out
}

#' Print a Summary of a Fitted Nonlinear Fay-Herriot Model
#'
#' Prints a compact view of a `summary.nlfh_fit` object. Large area-level and
#' coefficient summaries are truncated to their first rows.
#'
#' @param x An object returned by `summary.nlfh_fit()`.
#' @param digits Number of significant digits to print.
#' @param max_rows Maximum number of rows to print from coefficient and
#'   area-level summaries.
#' @param ... Additional arguments passed to methods. Currently unused.
#'
#' @return The input object `x`, invisibly.
#' @export
print.summary.nlfh_fit <- function(x, digits = 4, max_rows = 6, ...) {
  cat("<summary.nlfh_fit>\n")
  cat("Model type: ", x$model_type %||% x$method %||% "unknown", "\n", sep = "")
  if (!is.null(x$formula)) {
    cat("Formula: ", paste(deparse(x$formula), collapse = " "), "\n", sep = "")
  } else if (!is.null(x$interface)) {
    cat("Interface: ", x$interface, "\n", sep = "")
  }
  if (!is.null(x$dic)) {
    cat("DIC: ", format(x$dic, digits = 4), "\n", sep = "")
  }

  if (!is.null(x$mcmc)) {
    cat("\nMCMC:\n")
    print(x$mcmc, row.names = FALSE)
  }

  if (!is.null(x$diagnostics) && length(x$diagnostics) > 0L) {
    cat("\nDetails:\n")
    print(x$diagnostics, row.names = FALSE)
  }

  if (!is.null(x$variance) && nrow(x$variance) > 0L) {
    cat("\nVariance parameters:\n")
    print(.nlfh_round_summary(x$variance, digits = digits), row.names = FALSE)
  }

  if (!is.null(x$coefficients) && nrow(x$coefficients) > 0L) {
    cat("\nCoefficients:\n")
    .nlfh_print_limited_table(x$coefficients, max_rows = max_rows, digits = digits)
  }

  if (!is.null(x$areas) && nrow(x$areas) > 0L) {
    cat("\nArea-level estimates theta_i:\n")
    .nlfh_print_limited_table(x$areas, max_rows = max_rows, digits = digits)
  }

  if (!is.null(x$variable_importance)) {
    cat("\nVariable importance:\n")
    print(x$variable_importance)
  }
  invisible(x)
}

.nlfh_mcmc_summary <- function(x, n_areas, n_draws) {
  data.frame(
    n_iter = x$n_iter %||% NA_integer_,
    burn_in = x$burn_in %||% NA_integer_,
    posterior_draws = n_draws,
    areas = n_areas,
    stringsAsFactors = FALSE
  )
}

.nlfh_mcmc_diagnostics <- function(x, n_draws) {
  diagnostics <- list(
    retained_draws = n_draws
  )
  if (!is.null(x$n_iter) && !is.null(x$burn_in)) {
    diagnostics$burn_in_fraction <- x$burn_in / x$n_iter
  }
  if (!is.null(x$progress)) {
    diagnostics$progress <- x$progress
  }
  if (!is.null(x$n_hidden)) {
    diagnostics$n_hidden <- x$n_hidden
  }
  if (!is.null(x$n_trees)) {
    diagnostics$n_trees <- x$n_trees
  }
  if (!is.null(x$n_bart_samples)) {
    diagnostics$n_bart_samples <- x$n_bart_samples
  }
  as.data.frame(diagnostics, stringsAsFactors = FALSE)
}

.nlfh_coefficient_summary <- function(x) {
  if (is.null(x$coefficients)) {
    return(NULL)
  }
  out <- .nlfh_summarize_draw_matrix(x$coefficients, margin = "columns")
  if (!is.null(x$model_matrix_colnames) &&
      length(x$model_matrix_colnames) == nrow(out)) {
    out$parameter <- x$model_matrix_colnames
  } else if (!is.null(x$n_hidden) && x$n_hidden == nrow(out)) {
    out$parameter <- paste0("hidden", seq_len(nrow(out)))
  }
  out
}

.nlfh_variance_summary <- function(x) {
  summaries <- list()
  if (!is.null(x$random_effect_variance)) {
    summaries[[length(summaries) + 1L]] <- .nlfh_summarize_draw_vector(
      x$random_effect_variance,
      "random_effect_variance"
    )
  }
  if (!is.null(x$coefficient_variance)) {
    summaries[[length(summaries) + 1L]] <- .nlfh_summarize_draw_vector(
      x$coefficient_variance,
      "coefficient_variance"
    )
  }
  if (length(summaries) == 0L) {
    return(NULL)
  }
  do.call(rbind, summaries)
}

.nlfh_area_summary <- function(x) {
  if (is.null(x$predictions)) {
    return(NULL)
  }
  out <- .nlfh_summarize_draw_matrix(x$predictions, margin = "rows")
  out$area <- rownames(x$predictions) %||% seq_len(nrow(out))
  out <- out[, c("area", setdiff(names(out), "area")), drop = FALSE]
  out$parameter <- NULL
  out
}

.nlfh_summarize_draw_matrix <- function(x, margin = c("rows", "columns")) {
  margin <- match.arg(margin)
  if (!is.matrix(x)) {
    x <- as.matrix(x)
  }
  if (margin == "rows") {
    labels <- rownames(x)
    if (is.null(labels)) {
      labels <- as.character(seq_len(nrow(x)))
    }
    stats <- t(apply(x, 1L, .nlfh_draw_summary))
    out <- data.frame(parameter = labels, stats, check.names = FALSE)
  } else {
    labels <- colnames(x)
    if (is.null(labels)) {
      labels <- as.character(seq_len(ncol(x)))
    }
    stats <- t(apply(x, 2L, .nlfh_draw_summary))
    out <- data.frame(parameter = labels, stats, check.names = FALSE)
  }
  rownames(out) <- NULL
  out
}

.nlfh_summarize_draw_vector <- function(x, parameter) {
  data.frame(
    parameter = parameter,
    t(.nlfh_draw_summary(x)),
    check.names = FALSE,
    row.names = NULL
  )
}

.nlfh_draw_summary <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0L) {
    return(c(mean = NA_real_, sd = NA_real_, median = NA_real_,
             q2.5 = NA_real_, q97.5 = NA_real_))
  }
  c(
    mean = mean(x),
    sd = stats::sd(x),
    median = stats::median(x),
    q2.5 = stats::quantile(x, 0.025, names = FALSE),
    q97.5 = stats::quantile(x, 0.975, names = FALSE)
  )
}

.nlfh_round_summary <- function(x, digits) {
  numeric_columns <- vapply(x, is.numeric, logical(1))
  x[numeric_columns] <- lapply(x[numeric_columns], signif, digits = digits)
  x
}

.nlfh_print_limited_table <- function(x, max_rows, digits) {
  n <- nrow(x)
  shown <- utils::head(x, max_rows)
  print(.nlfh_round_summary(shown, digits = digits), row.names = FALSE)
  if (n > max_rows) {
    cat("... ", n - max_rows, " more rows\n", sep = "")
  }
  invisible(x)
}
