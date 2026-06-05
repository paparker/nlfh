#' Fit a Fay-Herriot Model
#'
#' Primary user-facing model fitting function for linear and nonlinear
#' Fay-Herriot models. Use `method` to choose the mean-function model and
#' `control` to pass method-specific tuning parameters.
#'
#' @param formula Optional model formula such as `y ~ x1 + x2`. If the first
#'   argument is a formula, it is treated as `formula`. For nonlinear methods,
#'   the formula specifies the predictors available to the model; it does not
#'   imply an additive linear mean structure. Nonlinearities and interactions
#'   may be learned implicitly by the selected model.
#' @param data Optional data frame containing variables used by `formula` and,
#'   optionally, `sampling_variance`.
#' @param y Numeric vector of area-level direct estimates for the matrix
#'   interface.
#' @param x,X Numeric matrix or data frame of area-level covariates for the
#'   matrix interface. Rows must correspond to entries of `y`. Include an
#'   intercept column if one is desired.
#' @param sampling_variance Numeric vector of known sampling variances for `y`.
#'   With the formula interface, this may also be an unquoted column name from
#'   `data` or a length-one character string naming a column in `data`.
#' @param method Character string selecting the model. Options are `"linear"`
#'   for the linear Fay-Herriot model, `"rnn"` for the random-weight neural
#'   network Fay-Herriot model, and `"bart"` for the BART Fay-Herriot model.
#' @param control Named list of control parameters. Common controls are
#'   `n_iter`, `burn_in`, and `progress`. Linear-specific control:
#'   `prior_beta_variance`. RNN-specific control: `n_hidden`. BART-specific
#'   controls: `prior_shape`, `prior_rate`, `n_bart_samples`, and `n_trees`.
#'
#' @return An object inheriting from `nlfh_fit`. The first class identifies the
#'   fitted method: `nlfh_linear_fit`, `nlfh_rnn_fit`, or `nlfh_bart_fit`.
#'
#' @details
#' Formula inputs are parsed with [stats::model.frame()] and
#' [stats::model.matrix()]. Factors are expanded using R's standard contrast and
#' dummy-variable rules. An intercept is included when the formula includes one,
#' which is the default for formulas such as `y ~ x1 + x2`; use `0 +` or `- 1`
#' in the formula to omit it. Matrix inputs are used as supplied, so include an
#' intercept column manually if one is desired.
#'
#' For `method = "rnn"` and `method = "bart"`, the formula identifies the
#' predictors available to the nonlinear mean function. It does not impose an
#' additive linear model. These methods estimate an unknown function `f(X)`, and
#' nonlinearities or interactions may be learned implicitly by the model.
#' `sampling_variance` is always supplied separately from the formula. When
#' `sampling_variance` names a column in `data`, that column is excluded from
#' `y ~ .` expansion.
#' @export
#'
#' @examples
#' \dontrun{
#' x <- cbind(intercept = 1, z = rnorm(20))
#' y <- rnorm(20)
#' sampling_variance <- rep(0.2, 20)
#'
#' dat <- data.frame(y = y, x1 = x[, 2], sampling_variance = sampling_variance)
#' fit_fh(y ~ x1, sampling_variance = sampling_variance, data = dat,
#'        method = "linear")
#' fit_fh(y = y, X = x, sampling_variance = sampling_variance,
#'        method = "linear")
#' fit_fh(y ~ ., sampling_variance = sampling_variance, data = dat,
#'        method = "rnn", control = list(n_hidden = 50, n_iter = 2000))
#' fit_fh(y, x, sampling_variance, method = "rnn",
#'        control = list(n_hidden = 50, n_iter = 2000))
#' fit_fh(y, x, sampling_variance, method = "bart",
#'        control = list(n_trees = 25, n_bart_samples = 5))
#' }
fit_fh <- function(y = NULL, x = NULL, sampling_variance = NULL,
                   method = c("linear", "rnn", "bart"), control = list(),
                   data = NULL, formula = NULL, X = NULL) {
  method <- match.arg(method)

  input <- parse_fh_inputs(
    formula = formula,
    data = data,
    y_expr = if (missing(y)) NULL else substitute(y),
    x_expr = if (missing(x)) NULL else substitute(x),
    X_expr = if (missing(X)) NULL else substitute(X),
    sampling_variance_expr = if (missing(sampling_variance)) NULL else {
      substitute(sampling_variance)
    },
    env = parent.frame()
  )

  defaults <- switch(
    method,
    linear = list(
      prior_beta_variance = 1000,
      n_iter = 1000,
      burn_in = 500,
      progress = TRUE
    ),
    rnn = list(
      n_hidden = 200,
      n_iter = 1000,
      burn_in = 500,
      progress = TRUE
    ),
    bart = list(
      prior_shape = 0.01,
      prior_rate = 0.01,
      n_iter = 1000,
      burn_in = 500,
      n_bart_samples = 10,
      n_trees = 50,
      progress = TRUE
    )
  )
  control <- .validate_control(control, names(defaults), method)
  args <- c(
    list(y = input$y, X = input$X, sampling_variance = input$vardir),
    utils::modifyList(defaults, control)
  )

  fit <- switch(
    method,
    linear = do.call(fit_fh_linear, args),
    rnn = do.call(fit_fh_rnn, args),
    bart = do.call(fit_fh_bart, args)
  )
  fit$method <- method
  fit$call <- match.call()
  fit$formula <- input$formula
  fit$terms <- input$terms
  fit$model_frame <- input$model_frame
  fit$model_matrix_colnames <- input$model_matrix_colnames
  fit$response_name <- input$response_name
  fit$predictor_names <- input$predictor_names
  fit$has_intercept <- input$has_intercept
  fit$sampling_variance_name <- input$sampling_variance_name
  fit$interface <- input$interface
  fit
}
