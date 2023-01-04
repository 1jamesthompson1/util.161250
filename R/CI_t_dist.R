#' Quick function to get population mean estimate from sample
#'
#' This function will get the confidence interval for a sample data set.
#'
#' @param data A atomic vector
#' @param level Confidence level you would like. Standard is 0.95 (95%) however it is a aribatary number.
#'
#' @return CI_t_dist class object with all the required information
#' @export
#' @name CI
#' @examples
#' exampleData <- rnorm(1e4)
#' ci <- CI_t_dist(exampleData)
CI_t_dist <- function(data, level = 0.95) {
  t <- stats::qt((1 - level) / 2, length(data) - 1, lower.tail = FALSE)
  sample_mean <- mean(data)
  e.s.e <- stats::sd(data) / sqrt(length(data))

  min <- sample_mean - t * e.s.e
  max <- sample_mean + t * e.s.e

  structure(
    list(
      sample_mean = sample_mean,
      t = t,
      e.s.e = e.s.e,
      interval_min = min,
      interval_max = max,
      level = level
    ),
    class = "CI_t_dist"
  )
}

#' @rdname CI
#'
#' @param x CI_t_dist object to be printed
#' @param ... Ignored by this printing function
#'
#' @return Invisible CI_t_dist object.
#' @export
#'
#' @examples
#' print(ci)
print.CI_t_dist <- function(x, ...) {
  cat(paste0(x$level, "% confidence interval is: (", x$interval_min, ", ", x$interval_max, ")\n"))
  cat(paste0(x$sample_mean, " +- ", x$t, " * ", x$e.s.e, "\n"))
  invisible(x)
}
