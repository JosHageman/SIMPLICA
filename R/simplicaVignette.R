#' Simulated matrix and fitted components
#'
#' A simulated matrix and its SIMPLICA fit for the introductory vignette.
#' The fit is stored so examples can run without repeating the GA.
#'
#' @format A named list with three elements:
#' \describe{
#'   \item{data}{The simulated matrix with 60 rows, 120 columns and 5 percent missing values. It
#'     contains one planted block for each of the six built-in patterns.}
#'   \item{fit}{The result of fitting up to six components, including
#'     post-fit CV, residuals, component fits and scores by seed.}
#'   \item{info}{Arguments used for the fit, dataset ID, simulation seeds, and SIMPLICA and
#'     R versions.}
#' }
#'
#' @details Use `do.call(simplica, c(list(df = simplicaVignette$data),
#' simplicaVignette$info$gaSettings))` to rerun the fit. The source checkout
#' contains the simulation and fitting scripts in `data-raw/`.
#'
#' @docType data
#' @keywords datasets
#' @examples
#' data("simplicaVignette")
#'
#' x <- simplicaVignette$data
#' fit <- simplicaVignette$fit
#'
#' dim(x)
#' mean(is.na(x))
#' summaryComponents(fit)
"simplicaVignette"
