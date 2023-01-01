#' Find out the three D-stats for symmetry
#'
#' This gets a vector with the three d values.
#'
#' The three different d values are useful for different purposes.
#' D1 is not very robust but quite simple to understand and comes from `(mean(x) - median(x))/sd(x)`.
#' D2 is more robust by instead using the f-spread `(mean(x) - median(x))/(fivenum(x)[4] - fivenum(x)[2])`.
#' D3 lastly only measures the the middle 50% of the data. `((fivenum(x)[4]+fivenum(x)[2])/2 - median(x))/(fivenum(x)[4] - fivenum(x)[2])`
#'
#' @param x The variable that you are going to be looking at.
#'
#' @return a named vector with the three d values
#'
#' @examples
#' # Find out if the height is symmetric
#' D_stats(Loblolly$height)
#'
#' @export
D_stats <- function(x) {
  c(
    D1 = (mean(x) - stats::median(x)) / stats::sd(x),
    D2 = (mean(x) - stats::median(x)) / (stats::fivenum(x)[4] - stats::fivenum(x)[2]),
    D3 = ((stats::fivenum(x)[4] + stats::fivenum(x)[2]) / 2 - stats::median(x)) / (stats::fivenum(x)[4] - stats::fivenum(x)[2])
  )
}
