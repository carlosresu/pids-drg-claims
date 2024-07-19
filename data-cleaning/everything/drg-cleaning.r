knitr::opts_chunk$set(echo = TRUE)

to_read <- FALSE
to_split <- TRUE
to_sample <- TRUE
to_write <- TRUE
to_group <- TRUE
to_filter <- FALSE # unused
to_profvis <- FALSE
to_chunk <- TRUE # doesn't work if false; not chunking is deprecated.
to_view_checks <- TRUE
to_view_checks_parallelized <- FALSE
to_parallelize <- TRUE

year_to_load <- "2018"
version <- "v2"

split_chunks <- 5

# which of the split chunks to process;
# write NA if it should process everything in a loop
split_chunk_to_process <- 1

if (is.na(split_chunk_to_process)) {
  part <- NULL
} else {
  part <- split_chunk_to_process
}

seed <- 123
rows_to_show <- 10

drop_cols <- c(
  paste0("ICDCODE", 13:14),
  "ICCODED15",
  paste0("ICDCODE", 16:170)
)

icd_cols <- paste0("clin_icd", 1:12)
rvs_cols <- paste0("clin_rvs", 1:20)

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

# library(data.table)

# Function to count total rows using fread
count_total_rows <- function(file_path) {
  total_rows <- fread(file_path, select = 1L, header = TRUE)[, .N]
  return(total_rows)
}

# Use the function to count total rows
if (file.exists(full_claims_file(part))) {
  total_rows <- count_total_rows(full_claims_file(part))
} else {
  total_rows <- count_total_rows(full_claims_file())
}
print(paste("Total Rows via fread:", total_rows))

# library(vroom)

# # Function to count rows using vroom
# count_rows_vroom <- function(file_path) {
#   total_lines <- length(vroom_lines(file_path, altrep = TRUE, progress = FALSE))
#   return(total_lines - 1L)  # subtract 1 for the header
# }

# # Use the function to count total rows
# total_rows <- count_rows_vroom(full_claims_file(part))

# print(paste("Total Rows via vroom_lines:", total_rows))

sample_size_divisor <- 5

if (to_split) {
  if (is.null(part)) {
    sample_size <- ceiling(total_rows / split_chunks / sample_size_divisor)
  } else {
    sample_size <- ceiling(total_rows / sample_size_divisor)
  }
} else {
  if (is.null(part)) {
    sample_size <- ceiling(total_rows / sample_size_divisor)
  } else {
    sample_size <- ceiling(total_rows)
  }
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
  "11_debug-functions.R"
)

# Generate and execute source commands
for (script in scripts_to_source) {
  source(here("data-cleaning", "r_scripts", script))
}


options(warn = 1)


# if (file.exists(total_rows_file(part))) {
#   total_rows <- readRDS(total_rows_file(part))
# } else {
#   total_rows <- fread(full_claims_file(part), select = 1L, header = TRUE)[, .N]
#   saveRDS(total_rows, file = total_rows_file(part))
# }


proc <- fread(here(path_to_excel, "proc.csv"))
proc[, CODE := as.character(CODE)]
# head(proc)

rvs_icd9 <- fread(here(path_to_aux, "rvs_icd9cm.csv"),
  select = c("rvs", "icd9cm")
)
rvs_icd9[, rvs := as.character(rvs)]
rvs_icd9[, icd9cm := as.character(icd9cm * 100)]
rvs_icd9 <- merge(rvs_icd9, proc[, .(CODE, DRGUSE)],
  by.x = "icd9cm", by.y = "CODE", all.x = TRUE
)
rvs_icd9[, is_drg := !is.na(DRGUSE) & DRGUSE]
rvs_icd9 <- rvs_icd9[!is.na(rvs) & !is.na(icd9cm), -"DRGUSE"]
# head(rvs_icd9)

acr_rvs <- fread(here(path_to_aux, "acr_rvs.csv"))
# head(acr_rvs)

# Read in the data.table
tdrg_icd10 <- fread(here(path_to_aux, "i10.csv"))

# Set the key if not already set
setkey(tdrg_icd10, "CODE")
# head(tdrg_icd10)

# Subset and assign the result to acc_pdx
acc_pdx <- tdrg_icd10[ACCPDX == "Y", CODE]

# Optional: if CODEs are not unique in tdrg_icd10
acc_pdx <- unique(acc_pdx)


# Main workflow
all_parts_summaries <- list()
all_parts_statistics <- list()

# Suppress interim output
suppress_interim_output <- function(expr) {
  suppressMessages(suppressWarnings(capture.output(expr, file = NULL)))
}

# Main script logic
if (!is.na(split_chunk_to_process)) {
  if (to_profvis) {
    p <- profvis({
      split_and_save_chunks()
      dt <- read_and_process_chunk(split_chunk_to_process)
      if (!is.null(dt) && nrow(dt) > 0) {
        dt <- parallelize_and_summarize_data(split_chunk_to_process, dt)
        write_intermediate_file(split_chunk_to_process, dt)
        group_data(split_chunk_to_process, dt)
      } else {
        print(paste("No data to process for part:", split_chunk_to_process))
      }
      combine_and_print_summaries()
    })
    htmlwidgets::saveWidget(
      p,
      file = here(
        "git-ignored-files", "profvis",
        paste0("profvis_part_", split_chunk_to_process, "_.html")
      ),
      selfcontained = TRUE
    )
  } else {
    split_and_save_chunks()
    dt <- read_and_process_chunk(split_chunk_to_process)
    if (!is.null(dt) && nrow(dt) > 0) {
      dt <- parallelize_and_summarize_data(split_chunk_to_process, dt)
      write_intermediate_file(split_chunk_to_process, dt)
      group_data(split_chunk_to_process, dt)
    }
    combine_and_print_summaries()
  }
} else {
  if (to_profvis) {
    p <- profvis({
      split_and_save_chunks()
      for (part in 1:split_chunks) {
        dt <- read_and_process_chunk(part)
        if (!is.null(dt) && nrow(dt) > 0) {
          dt <- parallelize_and_summarize_data(part, dt)
          write_intermediate_file(part, dt)
          group_data(part, dt)
        } else {
          print(paste("No data to process for part:", part))
        }
      }
      combine_and_print_summaries()
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
        dt <- parallelize_and_summarize_data(part, dt)
        write_intermediate_file(part, dt)
        group_data(part, dt)
      } else {
        print(paste("No data to process for part:", part))
      }
    }
    combine_and_print_summaries()
  }
}


# options(verbose = FALSE)
# options(warn = -1)

# if (!is.na(split_chunk_to_process)) {
#   split_and_save_chunks()
#   part <- split_chunk_to_process
#   dt <- read_and_process_chunk(part)
# }


# options(warn = 1)


# Main code
# if (!is.na(split_chunk_to_process)) {
#   tic("Total execution time:")
#   dt <- parallelize_and_summarize_data(part, dt)
# }


if (!to_chunk) {
  if (to_profvis) {
    p <- profvis({
      clean_result <- clean_data(dt)
      dt <- clean_result$data
    })
    htmlwidgets::saveWidget(
      p,
      file = here("git-ignored-files", "profvis", "clean_data.html"),
      selfcontained = TRUE
    )
  } else {
    clean_result <- clean_data(dt)
    dt <- clean_result$data
  }
}


# Map RVS codes
if (!to_chunk) {
  if (to_profvis) {
    p <- profvis({
      rvs_mapping_result <- map_rvs_icd9(dt$clin_rvs, rvs_icd9)
      dt[, icd9_list := rvs_mapping_result$icd9_list]
      map_then_compare_icd_mappings(
        tdrg_icd10,
        rows_to_show,
        invalid_rows_to_show = rows_to_show
      )
    })
    htmlwidgets::saveWidget(
      p,
      file = here("git-ignored-files", "profvis", "map_rvs.html"),
      selfcontained = TRUE
    )
  } else {
    rvs_mapping_result <- map_rvs_icd9(dt$clin_rvs, rvs_icd9)
    dt[, icd9_list := rvs_mapping_result$icd9_list]
    map_then_compare_icd_mappings(
      tdrg_icd10,
      rows_to_show,
      invalid_rows_to_show = rows_to_show
    )
  }
}


# Find PDX
if (!to_chunk) {
  if (to_profvis) {
    p <- profvis({
      pdx_result <- apply_find_pdx(dt$clin_c1, dt$clin_c2, dt$clin_icd, acc_pdx)
      dt$pdx <- pdx_result$pdx
      dt$pdx_code <- pdx_result$pdx_code
    })
    htmlwidgets::saveWidget(
      p,
      file = here("git-ignored-files", "profvis", "find_pdx.html"),
      selfcontained = TRUE
    )
  } else {
    pdx_result <- apply_find_pdx(dt$clin_c1, dt$clin_c2, dt$clin_icd, acc_pdx)
    dt$pdx <- pdx_result$pdx
    dt$pdx_code <- pdx_result$pdx_code
  }
}


# if (!is.na(split_chunk_to_process)) {
#   write_intermediate_file(part, dt)
# }


# if (!is.na(split_chunk_to_process)) {
#   group_data(part, dt)
# }


# Stop the timer and capture total time
toc_data <- toc(log = TRUE)
total_time <- toc_data$toc - toc_data$tic


# Main script logic
if (!is.na(split_chunk_to_process)) {
  if (to_sample) {
    total_rows <- nrow(dt) * split_chunks * sample_size_divisor
  } else {
    total_rows <- nrow(dt) * split_chunks
  }
  # Your existing logic to set dt, total_time, and total_rows
  print_time_estimates(split_chunk_to_process, dt, total_time, total_rows)
} else {
  if (to_sample) {
    total_rows <- nrow(dt) * split_chunks * sample_size_divisor
  } else {
    total_rows <- nrow(dt) * split_chunks
  }
  # Your existing logic to set dt, total_time, and total_rows
  print_time_estimates(NA, dt, total_time, total_rows)
}


library(here)
source(here("data-cleaning", "r_scripts", "11_debug-functions.R"))
# Example usage within your script
input_path <- here("data-cleaning", "r_scripts")
output_file <- paste0(here("data-cleaning", "everything", "everything.R"))

concatenate_r_files(input_path, output_file)

