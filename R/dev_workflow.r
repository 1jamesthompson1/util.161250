#' Function to prepare the pkg for a commit
#'
#' This will style the code and render the readme and helps.
#'
#' @param location Where the base directory is for the package you want to prepare
.renderAndStyle <- function(location = ".") {
  styler::style_pkg(location)
  devtools::document(location)
  if (location == ".") {
    devtools::build_readme(location)
  }
}
