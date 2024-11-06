# Setup cache directory and virtual environment
cache_dir <- here(cache_path, "py_pkgs")
if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

# Create the `r-reticulate` environment if it does not exist
if (!virtualenv_exists("r-reticulate")) virtualenv_create("r-reticulate")
use_virtualenv("r-reticulate", required = TRUE)

# Helper function to check if `pip` is up-to-date and update if needed
is_pip_latest <- function() {
  current <- tryCatch(py_run_string("import pkg_resources; version = pkg_resources.get_distribution('pip').version")$version, error = function(e) NA)
  latest <- tryCatch(fromJSON("https://pypi.org/pypi/pip/json")$info$version, error = function(e) NA)

  # Explicitly check if both `current` and `latest` are available for comparison
  if (!is.na(current) && !is.na(latest) && current == latest) {
    # message("pip is up-to-date (version ", current, ").")
    return(TRUE)
  }

  # Log the update message if `current` is available; otherwise, log a generic update message
  if (!is.na(current)) {
    message("Updating pip from version ", current, " to ", latest, ".")
  } else {
    message("Unable to determine current pip version; proceeding to install the latest version.")
  }

  # Proceed with updating pip as fallback
  py_install("pip", envname = "r-reticulate", pip = TRUE)
  return(FALSE)
}
# Update pip
py_install("pip", envname = "r-reticulate", pip = TRUE, upgrade = TRUE)
invisible(is_pip_latest())

# Package mapping
pkg_map <- list(
  "numpy" = "numpy", "pandas" = "pandas", "streamlit" = "streamlit",
  "python-dateutil" = "dateutil", "tabulate" = "tabulate", "swifter" = "swifter",
  "rpy2" = "rpy2", "pyreadr" = "pyreadr", "papermill" = "papermill",
  "nbformat" = "nbformat", "IProgress" = "IPython.display", "jupyter" = "jupyter",
  "ipywidgets" = "ipywidgets"
)

# Initialize `pkg_versions` data.table
pkg_versions <- data.table(
  package = names(pkg_map),
  cached_version = as.character(NA),
  installed_version = as.character(NA),
  latest_version = as.character(NA)
)

# Function to install/upgrade packages with caching
install_or_upgrade <- function(pkg, import_name) {
  cache_file <- file.path(cache_dir, paste0(pkg, ".rds"))
  cached <- if (file.exists(cache_file)) readRDS(cache_file) else NA

  # Retrieve installed version
  installed <- tryCatch(
    py_run_string(paste0("import pkg_resources; version = pkg_resources.get_distribution('", pkg, "').version"), convert = TRUE)$version,
    error = function(e) NA
  )

  # Retrieve latest version from PyPI
  latest <- tryCatch(
    fromJSON(paste0("https://pypi.org/pypi/", pkg, "/json"))$info$version,
    error = function(e) NA
  )

  # Update `pkg_versions` table
  pkg_versions[package == pkg, `:=`(
    cached_version = as.character(cached),
    installed_version = as.character(installed),
    latest_version = as.character(latest)
  )]

  # Install or upgrade based on version comparison
  if (!is.na(installed) && installed == latest) {
    # message(pkg, " is up-to-date (version ", installed, ").")
  } else {
    py_install(pkg, envname = "r-reticulate", pip = TRUE, upgrade = TRUE)
    action <- if (is.na(installed)) "installed" else "upgraded"
    message(pkg, " ", action, " to version ", latest, if (!is.na(installed)) paste0(" (previously installed: ", installed, ")") else "", ".")
    saveRDS(latest, cache_file)
  }
}

# Apply install/upgrade process for each package
invisible(lapply(names(pkg_map), function(pkg) install_or_upgrade(pkg, pkg_map[[pkg]])))

# Save the `pkg_versions` table to file
saveRDS(pkg_versions, file.path(cache_dir, "pkg_versions.rds"))

# message("Success! All specified packages are installed or upgraded, and package versions have been saved.")
