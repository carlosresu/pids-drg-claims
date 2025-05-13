# Update the grouper
system("git submodule update --init --recursive")

# List required packages
# TODO: Cut this down
required_packages <- c(
  "data.table", # Fast data manipulation
  "here", # Simplifies file path management
  "tictoc", # Timing code execution
  "stringr", # String manipulation
  "stringi", # Unicode string processing
  "lubridate", # Date-time handling
  "profvis", # Profiling R code
  "hash", # Hashing utility
  "future", # Parallel processing
  "future.apply", # Parallelized apply functions
  "knitr", # Dynamic report generation
  "htmlwidgets", # Interactive HTML widgets
  "parallelly", # Advanced parallel computing
  "stringdist", # String distance calculations
  "parallel", # Base parallel computing
  "reticulate", # Interface to Python
  "bigrquery", # BigQuery client
  "jsonlite", # JSON parsing
  "googleCloudStorageR", # Google Cloud Storage access
  "haven", # Read/write Stata, SPSS, SAS files
  "fst", # Fast serialization
  "httr", # HTTP requests
  "ggplot2", # Data visualization
  "rmarkdown", # Dynamic markdown documents
  "digest", # Create cryptographic hashes
  "base64enc", # Base64 encoding/decoding
  "arrow", # Apache Arrow for fast data storage
  "tidyverse", # Collection of data science packages,
  "fasttime", # for fastPOSIXct
  "glue", # for string pasting
  "progressr", # live progress and ETA
  "AhoCorasickTrie" # multi-pattern string matching
)

github_packages <- c(
  "r-lib/styler" # Code formatting
)

# Installation commands (commented out, for reference)
invisible(lapply(
  required_packages, function(pkg) {
    if (!require(pkg, character.only = TRUE)) {
      install.packages(pkg)
    }
  }
))
invisible(lapply(
  github_packages, function(repo) {
    if (!require(basename(repo), character.only = TRUE)) {
      remotes::install_github(repo)
    }
  }
))

# Load packages (assumes they are already installed)
invisible(lapply(required_packages, library, character.only = TRUE))
invisible(lapply(basename(github_packages), library, character.only = TRUE))
