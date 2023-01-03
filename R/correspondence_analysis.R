#' Get the profiles of a 2d table
#'
#' Used to get either the row or column profiles of a data table.
#'
#' It currently it designed to work with matrixes. There ought to be row names and col names for it to plot correctly.
#'
#' @param data The 2d table you want to look at
#' @param rowprofile Whether you want it to be a row or col profile. False is for column profile
#'
#' @return A 2d matrix with the class `table_profile`
#' @export
#'
#' @name correspondence_analysis
data_profile <- function(data, rowprofile = TRUE) {
  total <- if (rowprofile) rowSums(data) else colSums(data)
  profile <- data / total
  structure(profile, class = "table_profile", rowprofile = rowprofile)
}

#' @rdname correspondence_analysis
#'
#' @details The plot function will simply plot this in a way that you would expect
#'
#' @param x The 2d profile table
#' @param y This is ignored by this plot function
#' @param ... This is also ignored by this plot function
#'
#' @return Plot returns nothing
#' @export
plot.table_profile <- function(x, y, ...) {
  attr(x, "class") <- NULL
  if (!attr(x, "rowprofile")) {
    numX <- ncol(x)
    numGroup <- nrow(x)
    table <- t(x)
    x.labs <- colnames(x)
    ylab <- "Row"
    xlab <- "Column"
  } else {
    numX <- nrow(x)
    numGroup <- ncol(x)
    table <- x
    x.labs <- rownames(x)
    ylab <- "Column"
    xlab <- "Row"
  }

  df <- table |>
    as.data.frame() |>
    dplyr::mutate(id = 1:nrow(table), rownames = rownames(table)) |>
    tidyr::pivot_longer(1:numGroup, names_to = "group", values_to = "value")

  ggplot(df, aes(x = id, y = value, shape = group, colour = group)) +
    geom_point() +
    ggplot2::scale_x_continuous(breaks = 1:numX, labels = x.labs) +
    xlab(paste(xlab, "grouping")) +
    ylab(paste(ylab, "Profile")) +
    labs(color = paste(ylab, "groupiqng"), shape = paste(ylab, "grouping")) +
    ggplot2::theme_minimal()
}
