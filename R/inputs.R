parse_fh_inputs <- function(formula = NULL, data = NULL, y = NULL, x = NULL,
                            X = NULL, sampling_variance = NULL,
                            y_expr = NULL, x_expr = NULL, X_expr = NULL,
                            sampling_variance_expr = NULL,
                            env = parent.frame()) {
  if (is.null(formula) && !is.null(y_expr) &&
      (inherits(y_expr, "formula") || .is_formula_call(y_expr))) {
    formula <- stats::as.formula(y_expr, env = env)
    y <- NULL
    y_expr <- NULL
  }

  if (!is.null(formula)) {
    .parse_formula_inputs(
      formula = formula,
      data = data,
      sampling_variance = sampling_variance,
      sampling_variance_expr = sampling_variance_expr,
      env = env
    )
  } else {
    .parse_matrix_inputs(
      y = y,
      x = x,
      X = X,
      sampling_variance = sampling_variance,
      y_expr = y_expr,
      x_expr = x_expr,
      X_expr = X_expr,
      sampling_variance_expr = sampling_variance_expr,
      env = env
    )
  }
}

.is_formula_call <- function(x) {
  is.call(x) && identical(x[[1L]], as.name("~"))
}

.parse_formula_inputs <- function(formula, data, sampling_variance,
                                  sampling_variance_expr, env) {
  if (!inherits(formula, "formula")) {
    stop("`formula` must be a formula.", call. = FALSE)
  }
  if (length(formula) != 3L) {
    stop("`formula` must include a response, for example `y ~ x1 + x2`.", call. = FALSE)
  }
  if (missing(data) || is.null(data)) {
    stop("`data` is required when using the formula interface.", call. = FALSE)
  }
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame when using the formula interface.", call. = FALSE)
  }

  sampling_variance_name <- .sampling_variance_name(sampling_variance_expr, data)
  model_data <- if (!is.null(sampling_variance_name)) {
    data[setdiff(names(data), sampling_variance_name)]
  } else {
    data
  }

  terms <- stats::terms(formula, data = model_data)
  response_index <- attr(terms, "response")
  if (is.null(response_index) || response_index == 0L) {
    stop("`formula` must include a response.", call. = FALSE)
  }

  model_frame <- stats::model.frame(
    formula = formula,
    data = model_data,
    na.action = stats::na.fail
  )
  y <- stats::model.response(model_frame)
  X <- stats::model.matrix(terms, model_frame)
  sampling_variance <- .resolve_sampling_variance(
    sampling_variance = sampling_variance,
    sampling_variance_expr = sampling_variance_expr,
    data = data,
    env = env
  )

  y <- .validate_response(y)
  X <- .validate_covariates(X, length(y))
  sampling_variance <- .validate_sampling_variance(sampling_variance, length(y))

  list(
    y = y,
    X = X,
    vardir = sampling_variance,
    sampling_variance = sampling_variance,
    formula = formula,
    terms = terms,
    model_frame = model_frame,
    response_name = names(model_frame)[response_index],
    predictor_names = attr(terms, "term.labels"),
    model_matrix_colnames = colnames(X),
    has_intercept = attr(terms, "intercept") == 1L,
    sampling_variance_name = sampling_variance_name,
    interface = "formula"
  )
}

.parse_matrix_inputs <- function(y, x, X, sampling_variance, y_expr, x_expr,
                                 X_expr, sampling_variance_expr, env) {
  if (!missing(X) && !is.null(X) && !missing(x) && !is.null(x)) {
    stop("Provide only one of `x` or `X`.", call. = FALSE)
  }

  y <- .resolve_argument(y, y_expr, "`y` is required for the matrix interface.", env)
  if ((missing(X) || is.null(X)) && (missing(x) || is.null(x))) {
    if (!is.null(X_expr) && !identical(X_expr, quote(NULL))) {
      X <- eval(X_expr, envir = env)
    } else if (!is.null(x_expr) && !identical(x_expr, quote(NULL))) {
      x <- eval(x_expr, envir = env)
    }
  }
  X <- if (!missing(X) && !is.null(X)) X else x
  if (missing(X) || is.null(X)) {
    stop("`x` or `X` is required for the matrix interface.", call. = FALSE)
  }
  sampling_variance <- .resolve_sampling_variance(
    sampling_variance = sampling_variance,
    sampling_variance_expr = sampling_variance_expr,
    data = NULL,
    env = env
  )

  y <- .validate_response(y)
  X <- .validate_covariates(X, length(y))
  sampling_variance <- .validate_sampling_variance(sampling_variance, length(y))

  list(
    y = y,
    X = X,
    vardir = sampling_variance,
    sampling_variance = sampling_variance,
    formula = NULL,
    terms = NULL,
    model_frame = NULL,
    response_name = NULL,
    predictor_names = colnames(X),
    model_matrix_colnames = colnames(X),
    has_intercept = any(colnames(X) %in% c("(Intercept)", "Intercept", "intercept")),
    sampling_variance_name = NULL,
    interface = "matrix"
  )
}

.sampling_variance_name <- function(sampling_variance_expr, data) {
  if (is.null(data) || is.null(sampling_variance_expr)) {
    return(NULL)
  }
  if (is.symbol(sampling_variance_expr)) {
    name <- as.character(sampling_variance_expr)
    if (name %in% names(data)) {
      return(name)
    }
  }
  if (is.character(sampling_variance_expr) &&
      length(sampling_variance_expr) == 1L &&
      sampling_variance_expr %in% names(data)) {
    return(sampling_variance_expr)
  }
  NULL
}

.resolve_argument <- function(value, expr, missing_message, env) {
  if (!missing(value) && !is.null(value)) {
    return(value)
  }
  if (!is.null(expr) && !identical(expr, quote(NULL))) {
    return(eval(expr, envir = env))
  }
  stop(missing_message, call. = FALSE)
}

.resolve_sampling_variance <- function(sampling_variance, sampling_variance_expr,
                                       data, env) {
  if (!is.null(sampling_variance)) {
    return(sampling_variance)
  }
  if (is.null(sampling_variance_expr) ||
      identical(sampling_variance_expr, quote(NULL))) {
    stop("`sampling_variance` is required.", call. = FALSE)
  }
  if (is.symbol(sampling_variance_expr)) {
    name <- as.character(sampling_variance_expr)
    if (!is.null(data) && name %in% names(data)) {
      return(data[[name]])
    }
  }
  if (is.character(sampling_variance_expr) &&
      length(sampling_variance_expr) == 1L &&
      !is.null(data) &&
      sampling_variance_expr %in% names(data)) {
    return(data[[sampling_variance_expr]])
  }
  eval(sampling_variance_expr, envir = env)
}

.fh_fit_metadata <- function(input) {
  list(
    formula = input$formula,
    terms = input$terms,
    model_frame = input$model_frame,
    model_matrix_colnames = input$model_matrix_colnames,
    response_name = input$response_name,
    predictor_names = input$predictor_names,
    has_intercept = input$has_intercept,
    sampling_variance_name = input$sampling_variance_name,
    interface = input$interface
  )
}
