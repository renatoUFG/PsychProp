packages <- c(
  "shiny", "shinyWidgets", "shinythemes",
  "tidyverse", "DT", "psych", "lavaan", "EFA.MRFA", "mirt"
)

install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

invisible(lapply(packages, install_if_missing))
