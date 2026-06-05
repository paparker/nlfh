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
#'   intercept column if one is desired. For `method = "bart"`, the first
#'   column is treated as a baseline/intercept column and is excluded from the
#'   BART splitting variables and `variable_importance`.
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
#'
#' For `method = "bart"`, the first column of the model matrix is treated as a
#' baseline/intercept column. With the formula interface this is usually the
#' default `(Intercept)` column. With the matrix interface, put the baseline or
#' intercept column first. BART variable importance is computed only for the
#' remaining columns, so `fit$variable_importance` does not include the first
#' column.
#' @export
#'
#' @examples
#' data(acs_dat)
#' acs_small <- as.data.frame(acs_dat[1:500, ])
#' example_control <- list(n_iter = 500, burn_in = 250, progress = FALSE)
#'
#' fit_linear <- fit_fh(
#'   MedInc ~ SNAPRate + PovRate + White + Black + Hispanic + Asian,
#'   sampling_variance = MedIncSE^2,
#'   data = acs_small,
#'   method = "linear",
#'   control = example_control
#' )
#'
#' X <- model.matrix(
#'   MedInc ~ SNAPRate + PovRate + White + Black + Hispanic + Asian,
#'   data = acs_small
#' )
#' fit_matrix <- fit_fh(
#'   y = acs_small$MedInc,
#'   X = X,
#'   sampling_variance = acs_small$MedIncSE^2,
#'   method = "linear",
#'   control = example_control
#' )
#'
#' fit_rnn <- fit_fh(
#'   MedInc ~ .,
#'   sampling_variance = MedIncSE^2,
#'   data = acs_small,
#'   method = "rnn",
#'   control = example_control
#' )
#'
#' fit_bart <- fit_fh(
#'   MedInc ~ SNAPRate + PovRate + White,
#'   sampling_variance = MedIncSE^2,
#'   data = acs_small,
#'   method = "bart",
#'   control = example_control
#' )
#'
#' # The default formula intercept is the first model-matrix column and is not
#' # included in BART variable importance.
#' fit_bart$variable_importance
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
      prior_beta_variance = 10000^2,
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
