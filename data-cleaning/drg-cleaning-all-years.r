paths <- list(
  year_to_load = here::here("data-cleaning", "cache", "year_to_load.txt"),
  input_notebook = here::here("data-cleaning", "drg-cleaning-v2.ipynb"),
  output_rscript = here::here("data-cleaning", "debug", "drg-cleaning")
)

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
for (year in c(2018:2023)) {
  write(as.character(year), paths$year_to_load)

  if (!run_notebook(TRUE)) {
    message("Retrying with to_parallel = FALSE...")
    run_notebook(FALSE)
  }
}
