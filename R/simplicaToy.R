#' Toy matrix with one multiplicative and one additive bicluster
#'
#' A small 30×60 matrix to demonstrate SIMPLICA in a controlled setting.
#' Contains one multiplicative and one additive simplivariate component (non-overlapping).
#'
#' @name simplicaToy
#' @docType data
#' @keywords datasets
#' @usage data(simplicaToy)
#'
#' @format A list with three elements:
#' \describe{
#'   \item{data}{numeric matrix of dimension \eqn{30 \times 60}}
#'   \item{trueComponents}{list of length 2 with \code{type}, \code{rows}, \code{cols}}
#'   \item{description}{character string}
#' }
#'
#' @examples
#' data("simplicaToy")
#' str(simplicaToy)
#' image(t(simplicaToy$data))
"simplicaToy"
