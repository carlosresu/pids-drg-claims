library(data.table)
library(here)

# Read year from cache
year_file <- here("data-cleaning/debug/cache/year.txt")
if (file.exists(year_file)) {
  year <- as.numeric(fread(year_file)$V1)
} else {
  stop("Error: year.txt not found.", call. = FALSE)
}

# Path to automate.txt
automate_file <- here("data-cleaning/debug/cache/automate.txt")
to_automate <- FALSE # Default value

# Check if automate.txt exists and read its content
if (file.exists(automate_file)) {
  automate_content <- fread(automate_file)$V1
  to_automate <- tolower(trimws(automate_content)) == "true"
}

# Print results for debugging
message("==== Loaded Parameters ====")
message(paste("Year:", year))
message(paste("Automate:", to_automate))
message("===========================")

# Source each script in r_scripts_v2 and allow outputs to be visible
script_dir <- here("data-cleaning/r_scripts_v2")
if (dir.exists(script_dir)) {
  script_files <- list.files(script_dir, pattern = "\\.R$", full.names = TRUE)

  if (length(script_files) > 0) {
    message("Sourcing scripts from:", script_dir)
    for (file in script_files) {
      message(paste("Sourcing:", file))
      source(file, local = FALSE) # Use local = FALSE to ensure global availability
    }
  } else {
    message("No scripts found in r_scripts_v2.")
  }
} else {
  warning("Warning: r_scripts_v2 directory not found.")
}

# Make sure variables remain in the global environment
globalVariables(c("year", "to_automate"))

message("00e-load-params-and-scripts.r successfully executed.")
