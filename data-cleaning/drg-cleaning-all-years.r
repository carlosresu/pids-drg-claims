# Helper function to run the notebook as an R script
run_notebook <- function(to_parallel) {
  system(paste("Rscript", "/home/resurreccion_cmc_gmail_com/drg-pipeline/data-cleaning/debug/drg-cleaning.r")) == 0
}

# Loop over the years (2019, 2022) with retry logic
for (year in c(2018:2023)) {
  write(as.character(year), here::here("data-cleaning", "cache", "year_to_load.txt"))

  if (!run_notebook(TRUE)) {
    stop("R script failed.")
  }
}
