paths <- list(
  year_to_load = here::here("data-cleaning", "cache", "year_to_load.txt"),
  input_notebook = here::here("data-cleaning", "02-drg-grouping-v2.ipynb"),
  output_rscript = here::here("data-cleaning", "debug", "drg-grouping")
)

dir.create(dirname(here::here("data-cleaning/cache/year_to_load.txt")), recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(here::here("data-cleaning/debug/drg-grouping.r")), recursive = TRUE, showWarnings = FALSE)
if (!file.exists(here::here("data-cleaning/cache/year_to_load.txt"))) writeLines("2018", here::here("data-cleaning/cache/year_to_load.txt"))

# Helper function to run the notebook as an R script
run_notebook <- function(to_parallel) {
  Sys.setenv(TO_PARALLEL = to_parallel)

  system(paste(
    "jupyter nbconvert --no-prompt --to script",
    paths$input_notebook, "--output", paths$output_rscript
  ))

  system(paste("Rscript", paste0(paths$output_rscript, ".r"))) == 0
}

# Loop over the years (2019, 2022) with retry logic
for (year in c(2018:2018)) {
  write(as.character(year), paths$year_to_load)

  if (!run_notebook(TRUE)) {
    stop("R script failed.")
  }
}
