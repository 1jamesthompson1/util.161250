#' Function to prepare the pkg for a commit
#'
#' This will style the code and render the readme and helps.
#'
#' @param location Where the base directory is for the package you want to prepare
.renderAndStyle <- function(location = ".") {
  styler::style_pkg(location)
  devtools::document(location)

  # the readme can only be built when in the base package location
  # It will also only run if the readme file is newer.
  if (location == "." && file.info("README.Rmd")$mtime > file.info("README.Rmd")$mtime) {
    devtools::build_readme(location)
  }
}
