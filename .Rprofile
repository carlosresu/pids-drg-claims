source("renv/activate.R")
library(here)
.libPaths(c(.libPaths(), here()))
# Assuming your R scripts are in a directory called 'R'
param_file <- list.files("data-cleaning/r_scripts", pattern = "parameters.R", full.names = TRUE)
lapply(param_file, source)
source_files <- list.files("data-cleaning/r_scripts", pattern = "\\.R$", full.names = TRUE)
lapply(source_files, source)
