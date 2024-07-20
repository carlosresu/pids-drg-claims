# IMPORTANT PARAMETERS:
year_to_load <- "2018"
split_chunks <- 5
rows_to_show <- 10

# Input:
to_read <- FALSE
to_split <- TRUE
to_sample <- TRUE

# Output:
to_write <- TRUE
to_group <- TRUE

# Debug:
to_profvis <- FALSE
to_view_checks <- TRUE
to_view_checks_parallelized <- FALSE
to_parallelize <- TRUE

# Sample size divisor:
sample_size_divisor <- 125

drop_cols <- c(
  paste0("ICDCODE", 13:14),
  "ICCODED15",
  paste0("ICDCODE", 16:170)
)

icd_cols <- paste0("clin_icd", 1:12)
rvs_cols <- paste0("clin_rvs", 1:20)

seed <- 123
set.seed(seed)

options(future.globals.maxSize = 1024 * 1024^2)

global_seed <- seed # for parallelized operations


options(verbose = FALSE)
options(warn = -1)
library(here)


# List of scripts to source in order
scripts_to_source <- c(
  "00_libraries.R",
  "01_data-formats.R",
  "02_file-paths.R"
)

# Generate and execute source commands
for (script in scripts_to_source) {
  source(here("data-cleaning", "r_scripts", script))
}

# Perform other tasks here...
tic("Total execution time:")

# Use the function to count total rows
total_rows <- fread(full_claims_file(), select = 1L, header = TRUE)[, .N]
print(paste("Total Rows via fread:", total_rows))

if (to_split) {
  sample_size <- ceiling(total_rows / split_chunks / sample_size_divisor)
} else {
  sample_size <- ceiling(total_rows / sample_size_divisor)
}

# List of scripts to source in order
scripts_to_source <- c(
  "03_general-functions.R",
  "04_main-functions.R",
  "05_io-functions.R",
  "06_icd-functions.R",
  "07_rvs-functions.R",
  "08_pdx-functions.R",
  "09_grouper-functions.R",
  "10_timing-functions.R",
  "11_debug-functions.R",
  "12_summary-functions.R"
)

# Generate and execute source commands
for (script in scripts_to_source) {
  source(here("data-cleaning", "r_scripts", script))
}


options(warn = 1)


proc <- fread(here(path_to_excel, "proc.csv"))
proc[, CODE := as.character(CODE)]

rvs_icd9 <- fread(here(path_to_aux, "rvs_icd9cm.csv"), select = c("rvs", "icd9cm"))
rvs_icd9[, rvs := as.character(rvs)]
rvs_icd9[, icd9cm := as.character(icd9cm * 100)]
rvs_icd9 <- merge(rvs_icd9, proc[, .(CODE, DRGUSE)], by.x = "icd9cm", by.y = "CODE", all.x = TRUE)
rvs_icd9[, is_drg := !is.na(DRGUSE) & DRGUSE]
rvs_icd9 <- rvs_icd9[!is.na(rvs) & !is.na(icd9cm), -"DRGUSE"]

acr_rvs <- fread(here(path_to_aux, "acr_rvs.csv"))

# Read in the data.table
tdrg_icd10 <- fread(here(path_to_aux, "i10.csv"))

# Set the key if not already set
setkey(tdrg_icd10, "CODE")

# Subset and assign the result to acc_pdx
acc_pdx <- tdrg_icd10[ACCPDX == "Y", CODE]

# Optional: if CODEs are not unique in tdrg_icd10
acc_pdx <- unique(acc_pdx)


num_cores <- availableCores()
all_parts_summaries <- list()

if (to_profvis) {
  p <- profvis({
    split_and_save_chunks()
    for (part in 1:split_chunks) {
      dt <- read_and_process_chunk(part)
      if (!is.null(dt) && nrow(dt) > 0) {
        result <- parallelize_and_summarize_data(
          dt, num_cores - 1, to_view_checks, global_seed,
          rows_to_show, rvs_icd9, tdrg_icd10, acc_pdx, to_parallelize
        )
        dt <- result$dt
        all_parts_summaries[[part]] <- result$combined_summary
      } else {
        print(paste("No data to process for part:", part))
      }
    }
  })
  htmlwidgets::saveWidget(
    p,
    file = here("git-ignored-files", "profvis", "profvis.html"),
    selfcontained = TRUE
  )
} else {
  split_and_save_chunks()
  for (part in 1:split_chunks) {
    dt <- read_and_process_chunk(part)
    if (!is.null(dt) && nrow(dt) > 0) {
      result <- parallelize_and_summarize_data(
        dt, num_cores - 1, to_view_checks, global_seed,
        rows_to_show, rvs_icd9, tdrg_icd10, acc_pdx, to_parallelize
      )
      dt <- result$dt
      all_parts_summaries[[part]] <- result$combined_summary
    } else {
      print(paste("No data to process for part:", part))
    }
  }
}

combine_and_print_summaries(all_parts_summaries, rows_to_show)


# Stop the timer and capture total time
toc_data <- toc(log = TRUE)
total_time <- toc_data$toc - toc_data$tic


if (to_sample) {
  total_rows <- nrow(dt) * split_chunks * sample_size_divisor
} else {
  total_rows <- nrow(dt) * split_chunks
}
print_time_estimates(dt, total_time, total_rows)


library(here)
source(here("data-cleaning", "r_scripts", "11_debug-functions.R"))

input_path <- here("data-cleaning", "r_scripts")
output_file <- paste0(here("data-cleaning", "everything", "everything.R"))

concatenate_r_files(input_path, output_file)

