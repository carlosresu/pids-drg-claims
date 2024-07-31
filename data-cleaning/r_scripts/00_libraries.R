# Function to check if the default library is system-wide and initialize renv if it is
initialize_renv_if_needed <- function() {
  # Get the default library path
  default_lib <- .libPaths()[1]
  
  # Check if the default library is system-wide
  system_wide_lib <- file.path(R.home("library"))
  
  if (default_lib == system_wide_lib) {
    message("Default library is system-wide. Initializing renv...")
    
    # Ensure renv is installed
    if (!requireNamespace("renv", quietly = TRUE)) {
      install.packages("renv", lib = "~/R/library")
      library(renv, lib.loc = "~/R/library")
    }
    
    # Initialize renv
    renv::init()
  } else {
    message("Default library is not system-wide. Skipping renv initialization.")
  }
}

# Ensure renv is installed and initialized
initialize_renv_if_needed()

# Install renv if not already installed
if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv", lib = "~/R/library")
  library(renv, lib.loc = "~/R/library")
}

# Activate renv
renv::activate()

required_packages <- c(
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

# Snapshot the project state to save the package versions in the lockfile
renv::snapshot()