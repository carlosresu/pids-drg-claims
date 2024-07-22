# IMPORTANT PARAMETERS:
# Which claims year to load
# TODO: maybe add a script that loops through all claims?
year_to_load <- "2018"
# How many parts to split the 12+m row claims file into
split_parts <- 5
# How many rows/entries to show in summary tables
rows_to_show <- 10
# How many rows/entries each part/chunk's summary should have (leave at Inf)
intermediate_rows_to_show <- Inf

# Input:
# Whether to forcibly read the whole file again instead
# of using the split parts created even if available
to_read <- FALSE
# Whether to split into split_parts parts (i.e. to fit in 32gb RAM)
to_split <- TRUE
# Whether to sample each split_parts part by sample_size_divisor
# Useful when iterating through code runs in quick succession
to_sample <- FALSE

# Output:
# Whether to write out intermediate files and caches
# (i.e. part files, sample files).
# TODO: upload to BQ as well
to_write <- TRUE
# Whether to export for the batch grouper or not
to_group <- TRUE

# Debug:
# Conduct runtime duration analysis via profvis or not
to_profvis <- FALSE
# Whether to view checks and print statements
to_view_checks <- TRUE
# Whether to view intermediate per part/chunk checks and print statements
# (Not consolidated) when parallelized
to_view_checks_parallelized <- FALSE
# Whether to parallelize each split_parts part into availableCores() - 1 chunks
# Cuts down processing time from 120min to 15min.
to_parallelize <- TRUE

# Sample size divisor:
# Formula for sample size is total_rows / split_parts / sample_size_divisor
sample_size_divisor <- 5

# Which columns to drop, note that ICDCODE15
# is misspelled as ICCODED15 in all claims
drop_cols <- c(
  paste0("ICDCODE", 13:14),
  "ICCODED15",
  paste0("ICDCODE", 16:170)
)

# Seed for reproducibility
# Important for stuff like randomly choosing a pdx among
# multiple possible options
seed <- 123
set.seed(seed)

# Allowing each future_lapply session to use more memory
options(future.globals.maxSize = 1024 * 1024^2)

# global_seed for future_lapply parts
global_seed <- seed # for parallelized operations


# Hide verbose output for script sourcing and library loading
options(verbose = FALSE)
# Hide warnings for script sourcing and library loading
options(warn = -1)
# Library here() so scipts can be loaded
library(here)


# List of scripts to source
scripts_to_source <- c(
  "00_libraries.R",
  "01_data-formats.R",
  "02_file-paths.R",
  "03_general-functions.R",
  "04a_clean-data-functions.R",
  "04b_chunk-functions.R",
  "04c_part-functions.R",
  "05_io-functions.R",
  "06_icd-functions.R",
  "07_rvs-functions.R",
  "08_pdx-functions.R",
  "09_grouper-functions.R",
  "10_timing-functions.R",
  "11_debug-functions.R",
  "12_summary-functions.R"
)

# Loop to source above scripts
for (script in scripts_to_source) {
  source(here("data-cleaning", "r_scripts", script))
}

# Start total execution timer
tic("Total execution time:")


# Reenable warnings; we don't renable verbose outputs
# since it makes it way too verbose
# TODO: figure out a way to return to default outputs
# verbose = TRUE is way too verbose compared to default
options(warn = 1)


# Read in all rvs codes and turn to character for further processing
proc <- fread(here(path_to_excel, "proc.csv"))
proc[, CODE := as.character(CODE)]

# Read in icd9cm equivalents of rvs codes
rvs_icd9 <- fread(here(path_to_aux, "rvs_icd9cm.csv"),
  select = c("rvs", "icd9cm")
)

# Convert to character for further processing
rvs_icd9[, rvs := as.character(rvs)]

# Convert to character and also remove decimals
# whilst keeping trailing zeroes
rvs_icd9[, icd9cm := as.character(icd9cm * 100)]

# Merge with proc from above, to be able to classify by DRGUSE
rvs_icd9 <- merge(rvs_icd9, proc[, .(CODE, DRGUSE)],
  by.x = "icd9cm", by.y = "CODE", all.x = TRUE
)

# Filter by DRGUSE
rvs_icd9[, is_drg := !is.na(DRGUSE) & DRGUSE]

# Remove DRGUSE and filter out NAs
rvs_icd9 <- rvs_icd9[!is.na(rvs) & !is.na(icd9cm), -"DRGUSE"]

# Read in PHIC all case rates
acr_rvs <- fread(here(path_to_aux, "acr_rvs.csv"))

# Read in the thai icd10 library
tdrg_icd10 <- fread(here(path_to_aux, "i10.csv"))

# Set the key if not already set
setkey(tdrg_icd10, "CODE")

# Subset and assign the result to acc_pdx
acc_pdx <- unique(tdrg_icd10[ACCPDX == "Y", CODE])


all_parts_summaries <- list()
processing_times <- numeric(split_parts)
num_cores <- availableCores() - 1

if (to_profvis) {
  p <- profvis({
    # Main execution logic
    split_and_save_parts()

    for (part in 1:split_parts) {
      result <- process_part(
        part, num_cores, to_view_checks, global_seed, intermediate_rows_to_show,
        rvs_icd9, tdrg_icd10, acc_pdx, to_parallelize, to_write, to_group
      )

      all_parts_summaries[[part]] <- result$combined_summary
      processing_times[part] <- result$processing_time

      print_status_update(part, split_parts, processing_times)

      dt <- result$dt
    }
    # End the parallelization session
    plan(sequential)
  })
  htmlwidgets::saveWidget(p,
    file = here("git-ignored-files", "profvis", "profvis.html")
  )
} else {
  # Main execution logic
  split_and_save_parts()

  for (part in 1:split_parts) {
    result <- process_part(
      part, num_cores, to_view_checks, global_seed, intermediate_rows_to_show,
      rvs_icd9, tdrg_icd10, acc_pdx, to_parallelize, to_write, to_group
    )

    all_parts_summaries[[part]] <- result$combined_summary
    processing_times[part] <- result$processing_time

    print_status_update(part, split_parts, processing_times)

    dt <- result$dt
  }
  # End the parallelization session
  plan(sequential)
}
# Print final summaries
print_summary_tables(
  combine_parts_summaries(
    all_parts_summaries, intermediate_rows_to_show
  ), rows_to_show
)


# Stop the timer and capture total time
toc_data <- toc(log = TRUE)
total_time <- toc_data$toc - toc_data$tic


# Calculate how many rows were actually processed
# Not just how many rows exist in the actual dataframe
# TODO: rename total_rows to something else to avoid confusion
if (to_sample) {
  total_rows <- nrow(dt) * split_parts * sample_size_divisor
} else {
  total_rows <- nrow(dt) * split_parts
}


print_time_estimates(dt, total_time, total_rows)


# Consolidate all r_scripts scripts into everything.R
# Useful for debugging

library(here)
source(here("data-cleaning", "r_scripts", "11_debug-functions.R"))

input_path <- here("data-cleaning", "r_scripts")
output_file <- paste0(here("data-cleaning", "everything", "everything.R"))

concatenate_r_files(input_path, output_file)

