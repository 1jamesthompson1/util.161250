#' Create residual table
#'
#' Theses functions are created to make the calculating and displaying of
#' residuals easy and quick. It will get the raw, standard and studentized
#' residuals from the lm object.
#'
#' @param lm A linear model object created by the `lm()` function
#'
#' @return A residuals
#' @export
#' @name residuals
#' @examples
#' mtcars.mod <- lm(mpg ~ wt, data = mtcars)
#' residuals <- residuals_list(mtcars.mod)
residuals_list <- function(lm = NULL) {
  if (!is(lm, "lm")) {
    stop("You need to give this function a lm object created
         with lm() to make the table with")
  }

  residuals <- stats::resid(lm)

  rstandard <- stats::rstandard(lm)

  rstudent <- stats::rstudent(lm)

  fitted <- stats::fitted.values(lm)

  structure(
    data.frame(
      fitted = fitted,
      raw = residuals,
      standardized = rstandard,
      studentized = rstudent
    ),
    class = "residuals.data"
  )
}

#' @rdname residuals
#'
#' @param x A `residuals.data` list object
#' @param ... Ignored by this method
#'
#' @return Plot function will return the plot object
#' @export
#'
#' @examples
#' plot(residuals)
plot.residuals.data <- function(x, ...) {
  # The object comes as a list. It will be converted into a dataframe and then
  # made longer for plotting
  x |>
    unclass() |>
    as.data.frame() |>
    tidyr::pivot_longer(2:4, names_to = "type", values_to = "residual") |>
    ggplot(aes(fitted, residual)) +
    geom_point() +
    facet_wrap(~type, ncol = 3, scales = "free") +
    theme_minimal()
}

#' @rdname residuals
#'
#' @param x A `residuals.data` list object
#' @param ... ignored by this method
#'
#' @return A plain data.frame
#' @export
#'
#' @examples
#' print(residuals)
print.residuals.data <- function(x, ...) {
  unclass(x) |>
    as.data.frame()
}
