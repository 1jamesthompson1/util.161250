#' Chisq functions
#'
#' Various homebrew functions to work with chisq tests
#'
#' @param observed This is the observed counts
#' @param ... Any other varaibles. For example `expected` which is used in the default method.
#'
#' @return in almost all cases it returns the p-value
#'
#' @name chisq_test
#'
#' @export
chisq_test <- function(observed, ...) {
  UseMethod("chisq_test")
}

#' @rdname chisq_test
#'
#' @details the p-value from a chisq test for the observed and expected outcomes.
#'

#' @param expected This is the expected counts
#'
#' @export
#'
#'
chisq_test.default <- function(observed, expected, ...) {
  chisq <- sum((observed - expected)^2 / expected)
  df <- length(observed) - 1
  stats::pchisq(chisq, df, lower.tail = FALSE)
  .format_chisq_output(chisq, df)
}

#'
#'
#' @rdname chisq_test
#'
#' @details Get the p-value from the contingency table for whether the rows and colums are associated or not.
#'
#' @export
#'
chisq_test.matrix <- function(observed, ...) {
  numR <- nrow(observed)
  numC <- ncol(observed)
  rowSum <- rowSums(observed)
  colSum <- colSums(observed)
  expected <- outer(rowSum, colSum) / sum(observed)
  chisq_stat <- mapply(function(o, e) (o - e)^2 / e, observed, expected) |>
    sum()
  df <- (numR - 1) * (numC - 1)
  .format_chisq_output(chisq_stat, df)
}

#' Structure the chisq_output
#'
#' Makes the formatting of the chisq outputs central so that all the smae names can be used without worry.
#'
#' @param chisq_value The chisq value that you have gotten from the calculations
#' @param df the degrees of freedom
#' @param p_value the p_value after applying to the the pchisq function. This can be left null if it is than it will be calculated.
#'
#' @return A list structure with the class `chisq_output`
#'
.format_chisq_output <- function(chisq_value = NULL, df = NULL, p_value = NULL) {
  if (is.null(p_value)) {
    p_value <- stats::pchisq(chisq_value, df, lower.tail = FALSE)
  }
  structure(list(
    chisq_value = chisq_value,
    df = df,
    p_value = p_value
  ), class = "chisq_output")
}


#' Printout for the chisq_output
#'
#' @param x A chisq_output that you want to print out
#' @param ... Any other variable currently they are ignored.
#'
#' @return the `invisible object`
#' @export
#'
print.chisq_output <- function(x, ...) {
  cat("  Chisq value\t\t\tdegrees of freedom\t\t\tp value\n")
  cat("  ", x$chisq_value, "\t\t\t", x$df, "\t\t\t", x$p_value, "\n")
  invisible(x)
}
