#' Example ACS Small Area Data
#'
#' A small example data set for fitting nonlinear Fay-Herriot models. `MedInc`
#' is the direct estimate response and `MedIncSE` is its standard error. Use
#' `MedIncSE^2` as the sampling variance when fitting models.
#'
#' @format A data frame with 1,617 rows and 8 variables:
#' \describe{
#'   \item{MedInc}{Area-level direct estimate of median income.}
#'   \item{MedIncSE}{Standard error of `MedInc`.}
#'   \item{SNAPRate}{Area-level SNAP participation rate covariate.}
#'   \item{PovRate}{Area-level poverty rate covariate.}
#'   \item{White}{Area-level proportion identifying as White.}
#'   \item{Black}{Area-level proportion identifying as Black.}
#'   \item{Hispanic}{Area-level proportion identifying as Hispanic.}
#'   \item{Asian}{Area-level proportion identifying as Asian.}
#' }
#'
#' @examples
#' data(acs_dat)
#' head(acs_dat)
"acs_dat"
