if (!require("renv")) {
  install.packages("renv")
  renv::init()
}

required_packages <- c(
  "languageserver",
  "jsonlite",
  "rlang",
  "yaml",
  "IRkernel",
  "data.table",
  "here",
  "tictoc",
  "stringr",
  "stringi",
  "lubridate",
  "docstring",
  "profvis",
  "hash",
  "future",
  "future.apply",
  "knitr",
  "htmlwidgets",
  "parallelly"
)

# Function to install and load packages using renv
install_and_load <- function(package) {
  if (!require(package, character.only = TRUE)) {
    renv::install(package)
    library(package, character.only = TRUE)
  }
}

# Install and load required packages using renv
lapply(required_packages, install_and_load)

suppressPackageStartupMessages({
  lapply(required_packages, library, character.only = TRUE)
})
