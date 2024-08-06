# IMPORTANT PARAMETERS:
year_to_load <- "2018" # Which claims year to load # TODO: maybe add a script that loops through all claims?
split_parts <- 15 # How many (integer) parts to split the 12+m row claims file into # TODO: a value of 10 for claims year 2018 leads to quoted newline errors
end_nrow <- 10 # How many rows/entries to show in summary tables
is_unix <- if (.Platform$OS.type == "unix") TRUE else FALSE # Detect operating system architecture
gcp_proj <- if (is_unix) system("gcloud config get-value project", intern = TRUE) else NULL
max_bq_rows <- 15000 # Max rows to return for bq query
cat(paste("GCP Project:", gcp_proj, "\n"))
ver_to_use <- "latest" # Deprecated, must be set to latest, local and bak have been deleted
encode <- "unknown" # Choices: unknown, UTF-8, Latin-1
sep <- "," # Choices: "," or "\t"

# Input:
to_sample <- TRUE # Whether to sample each split_parts part by sample_size_divisor (useful when iterating through code runs in quick succession)
sample_size_divisor <- 125 # Sample size divisor: Formula for sample size is total_rows / split_parts / sample_size_divisor. Choose between 5, 25, 125, and 625

# File Paths:
intermediate_path <- "data-cleaning/data-claims/intermediate"
cache_path <- "data-cleaning/cache"
aux_path <- "data-cleaning/data-aux-files"
excel_path <- "data-cleaning/data-excel"
cleaned_claims_path <- "data-cleaning/data-claims/cleaned"
grouper_output_path <- "data-cleaning/data-grouper-output"
chunks_path <- "data-cleaning/data-claims/chunked"
raw_claims_parts_path <- "data-cleaning/data-claims/raw/parts"
raw_claims_samples_path <- "data-cleaning/data-claims/raw/samples"
raw_claims_path <- "data-cleaning/data-claims/raw"
profvis_path <- "data-cleaning/profvis/profvis.html"

# Manual Tweaks:
manual_code_replacements <- list(
  "0800" = "O800",
  "080" = "O80",
  "0809" = "O809"
)
drop_cols <- c( # Which columns to drop
  paste0("ICDCODE", 13:14), # Start
  "ICCODED15", # note that ICDCODE15 is misspelled as ICCODED15 in all claims
  paste0("ICDCODE", 16:170), # Continuation
  "MEMCAT_SUBCHILD_DESC"
)

# Output:
to_write <- TRUE # Whether to write out intermediate files (everything up until converting for grouper export)
to_group <- TRUE # Whether to export for the batch grouper or not

# Debug:
to_debug <- FALSE # whether to print debug statements
to_profvis <- FALSE # Conduct runtime duration analysis via profvis or not
to_view_checks <- TRUE # Whether to view checks and print statements
to_view_checks_parallel <- FALSE # Whether to view intermediate per part/chunk checks and print statements (not consolidated) when parallelized
to_parallel <- TRUE # Whether to parallelize each split_parts part into availableCores() chunks. Cuts down processing time from 120min to 15min.
to_split_read <- FALSE # WARNING: TRUE uses a lot of memory!!
to_dec_mem_usage <- FALSE # Whether to run rm() and gc() at every possible step
tmp_nrow <- Inf # Per part/chunk end_nrow (leave at Inf)
diff_chars <- 0

seed <- 123 # Seed for reproducibility (Important for stuff like randomly choosing a pdx among multiple possible options)
set.seed(seed) # Setting the seed
global_seed <- seed # global_seed for future_lapply parts for parallelized operations

ram_size <- 32 # Input virtual or physical machine's RAM size here
ram_buffer <- 0.1 # How much of a buffer to leave for the OS
ram_limit <- (1 - ram_buffer) * (ram_size) * (1024^3) # Compute ram_limit in bytes
# Allowing each future_lapply session to use more memory
options(future.globals.maxSize = ram_limit)

# Compute the RAM limit for R processes, leaving the buffer for the OS
ram_limit_gb <- round((1 - ram_buffer) * ram_size, 0)
# Set the environment variable R_FUTURE_MAX_RAM in GB
# Sys.setenv(R_FUTURE_MAX_RAM = paste0(ram_limit_gb, "G"))

# Print the set RAM limit
# cat("Setting R_FUTURE_MAX_RAM to:", ram_limit_gb, "GB\n")
cat(sprintf("Setting future.globals.maxSize to: %.1f GB", ram_limit / (1024^3)))

if (!split_parts == as.integer(split_parts) || split_parts <= 1) stop("ERROR: split_parts must be an integer greater than or equal to 2!")
if (ram_size <= 64 && split_parts <= 2) stop("Please set split_parts to at least 3 for 64 GB machines or it will likely crash")
if (ram_size <= 32 && split_parts <= 4) stop("Please set split_parts to at least 5 for 32 GB machines or it will likely crash")
if (ram_size <= 32 && to_split_read == TRUE) stop("Please set to_split_read to TRUE for 32 GB machines or it will likely crash")


options(verbose = FALSE) # Hide verbose output for script and library loading
options(warn = -1) # Hide warnings for script sourcing and library loading
library(here) # Library here() so scripts can be loaded


scripts <- list( # List of scripts to source
  lib_params = "00_libraries-params.R",
  general = "01_general-functions.R",
  clinical = "02_clinical-functions.R",
  timing_debug = "03_timing-debug-functions.R",
  summary = "04_summary-functions.R"
)

# Loop to source above scripts
for (script in scripts) source(here("data-cleaning/r_scripts", script))


# TODO: figure out a way to return to default outputs
# since verbose = TRUE is way too verbose compared to default
options(warn = 1) # Reenable warnings; see above comments


# Loop through the years 2018 to 2021
for (year in 2018:2021) {
  file_name <- paste0("claims_extract_CLAIMS_", year, "_", ver_to_use, ".csv")
  bq_name <- paste0("claims_extract_CLAIMS_", year, "_", ver_to_use, ".csv")

  # Check if the file exists in the target directory
  file_path <- here(raw_claims_path, file_name)
  exists <- file.exists(file_path)

  # If the file does not exist, run the gsutil cp command
  if (!exists) {
    if (!is.null(gcp_proj) && gcp_proj == "test-drg-pipeline") {
      system(paste0("cd .. && gsutil cp gs://test-phic-claims-raw/", bq_name, " ", raw_claims_path),
        intern = FALSE, ignore.stderr = FALSE
      )
    } else {
      stop("Error: GCP Project is not null and is not test-drg-pipeline")
    }
  } else {
    cat(paste("File", file_name, "already exists in the target directory. Skipping download.\n"))
  }
}


# Read in all rvs codes and turn to character for further processing
# Run the gcloud bq query command to save the result as a CSV file
if (!file.exists(here(aux_path, "proc.csv"))) {
  system(
    paste0(
      "bq query --use_legacy_sql=false --format=csv --max_bq_rows=", max_bq_rows,
      " 'SELECT * FROM `", gcp_proj, ".grouper_v5.proc`' > ", here(aux_path, "proc.csv")
    ),
    intern = FALSE, ignore.stderr = FALSE
  )
} else {
  warning("proc.csv already exists, skipping bq query")
}

# Read the CSV file into an R data frame
proc <- fread(here(aux_path, "proc.csv"))[, CODE := as.character(CODE)]

# Read in icd9cm equivalents of rvs codes,
# then convert to character and also remove decimals, whilst keeping trailing zeroes
if (!file.exists(here(aux_path, "rvs_icd9cm.csv"))) {
  system(
    paste0(
      "bq query --use_legacy_sql=false --format=csv --max_bq_rows=", max_bq_rows,
      " 'SELECT * FROM `", gcp_proj, ".phic.acr_rvs_map`' > ", here(aux_path, "rvs_icd9cm.csv")
    ),
    intern = FALSE, ignore.stderr = FALSE
  )
} else {
  warning("rvs_icd9cm.csv already exists, skipping bq query")
}

rvs_icd9 <- fread(here(aux_path, "rvs_icd9cm.csv"), select = c("rvs", "icd9cm"))[, rvs := as.character(rvs)][, icd9cm := as.character(icd9cm * 100)]

# Merge with proc from above, to be able to classify by DRGUSE
rvs_icd9 <- merge(rvs_icd9, proc[, .(CODE, DRGUSE)], by.x = "icd9cm", by.y = "CODE", all.x = TRUE)

# Remove DRGUSE and filter out NAs
rvs_icd9 <- rvs_icd9[, is_drg := !is.na(DRGUSE) & DRGUSE][!is.na(rvs) & !is.na(icd9cm), -"DRGUSE"]

if (!file.exists(here(aux_path, "acr_rvs.csv"))) {
  system(
    paste0(
      "bq query --use_legacy_sql=false --format=csv --max_bq_rows=", max_bq_rows,
      " 'SELECT * FROM `", gcp_proj, ".phic.acr_procedure`' > ", here(aux_path, "acr_rvs.csv")
    ),
    intern = FALSE, ignore.stderr = FALSE
  )
} else {
  warning("acr_rvs.csv already exists, skipping bq query")
}

# Read in PHIC all case rates
acr_rvs <- fread(here(aux_path, "acr_rvs.csv"))

if (!file.exists(here(aux_path, "i10.csv"))) {
  system(
    paste0(
      "bq query --use_legacy_sql=false --format=csv --max_bq_rows=", max_bq_rows,
      " 'SELECT * FROM `", gcp_proj, ".grouper_v5.i10`' > ", here(aux_path, "i10.csv")
    ),
    intern = FALSE, ignore.stderr = FALSE
  )
} else {
  warning("i10.csv already exists, skipping bq query")
}

# Read in the thai icd10 library
tdrg_icd10 <- fread(here(aux_path, "i10.csv"))

# Set the key if not already set
setkey(tdrg_icd10, "CODE")

# Subset and assign the result to acc_pdx
acc_pdx <- unique(tdrg_icd10[ACCPDX == "Y", CODE])


# Define a codeblock to avoid repeating it twice when
# to_profvis is TRUE and again if FALSE
# Makes it easier to maintain as well, since we only need
# to modify one section instead of two
unified_block <- function() {
  # Start main execution logic
  # split_and_save_parts() # Read, split, and save partial files

  ### START OF SPLIT AND SAVE PARTS ###
  split_and_save <- function(part) {
    chunk_file <- full_claims_file(part)
    if (!file.exists(chunk_file)) {
      start_row <- (part - 1) * rows_per_part + 1
      end_row <- min(part * rows_per_part, total_rows)
      chunk_dt <- fread(
        full_claims_file(),
        skip = start_row,
        nrows = end_row - start_row + 1,
        na.strings = na_values,
        colClasses = "character",
        header = FALSE,
        encoding = encode,
        sep = sep
      )
      setnames(chunk_dt, colnames(header))
      if (to_debug) print(head(chunk_dt), 2) # debug
      fwrite(chunk_dt, chunk_file, quote = TRUE)
      if (to_dec_mem_usage) rm(chunk_dt)
      if (to_dec_mem_usage) gc()
    }
  }
  lapply(1:split_parts, split_and_save)
  ### END OF SPLIT AND SAVE PARTS ###
  # Start the parallelization session or remain sequential
  if (to_parallel && !is_unix) plan(multisession, workers = nthreads)

  # For each partial file (part) of 1:N (split_parts) files,
  for (part in 1:split_parts) {
    # Process the partial file with or without parallelization
    ### START OF PROCESS PART ###
    start_time <- Sys.time()

    ensure_partial_files_exist(part) # Ensure partial exist
    if (to_sample) ensure_sample_files_exist(part) # Ensure sample files exist

    read_result <- read_appropriate_file(part, to_sample) # Read the appropriate file
    read_in_dt <- read_result$read_result_dt
    read_in_replacement_summary <- read_result$read_result_replacement_summary

    ### START OF PARALLELIZE AND SUMMARIZE DATA
    chunk_size <- ceiling(nrow(read_in_dt) / nthreads)
    chunks <- split(read_in_dt, rep(1:nthreads, each = chunk_size, length.out = nrow(read_in_dt)))

    process_chunk <- function(chunk, to_view_checks, rvs_icd9, tdrg_icd10, acc_pdx) {
      if (to_view_checks) {
        # cat("\rViewing checks")
      } else {
        sink(tempfile())
        on.exit(sink(), add = TRUE)
      }

      clean_data <- function(dt) {
        dt[, SRC_YR := as.integer(year_to_load)] # Convert source year to integer

        setnames(dt, old = old_colnames, new = new_colnames) # Rename columns
        rename_success <- all(new_colnames %in% colnames(dt)) # Check if renaming was successful

        dt <- collapse_and_clean_icd_rvs(dt) # Collapse and clean ICD and RVS columns

        # Clean clin_c1 column
        dt[, clin_c1_orig := dt$clin_c1]
        dt[, clin_c1 := clean_column(clin_c1, na_like_strings)]
        dt[, clin_c1_orig := sapply(clin_c1_orig, toString)]
        dt[, clin_c1 := sapply(clin_c1, toString)]

        # Compare cleaning results for clin_c1
        clin_c1_cleaning_comparison <- dt[
          !is.na(clin_c1_orig) & clin_c1 != clin_c1_orig,
          .(old_code = clin_c1_orig, new_code = clin_c1, count = .N),
          by = .(clin_c1_orig, clin_c1)
        ]

        # Clean clin_c2 column
        dt[, clin_c2_orig := dt$clin_c2]
        dt[, clin_c2 := clean_column(clin_c2, na_like_strings)]
        dt[, clin_c2_orig := sapply(clin_c2_orig, toString)]
        dt[, clin_c2 := sapply(clin_c2, toString)]

        # Compare cleaning results for clin_c2
        clin_c2_cleaning_comparison <- dt[
          !is.na(clin_c2_orig) & clin_c2 != clin_c2_orig,
          .(old_code = clin_c2_orig, new_code = clin_c2, count = .N),
          by = .(clin_c2_orig, clin_c2)
        ]

        manual_multi_replace <- function(code, replacements) {
          # Iterate over each pattern and its corresponding replacement in the list
          for (pattern in names(replacements)) {
            replacement <- replacements[[pattern]]
            code <- gsub(paste0("\\b", pattern, "\\b"), replacement, code)
          }
          return(code)
        }

        # Apply the multi-replacement function using the named list
        dt[, clin_icd := lapply(clin_icd, manual_multi_replace,
          replacements = manual_code_replacements
        )]
        dt[, clin_c1 := lapply(clin_c1, manual_multi_replace,
          replacements = manual_code_replacements
        )]
        dt[, clin_c2 := lapply(clin_c2, manual_multi_replace,
          replacements = manual_code_replacements
        )]

        # Remove lumped ICD codes
        dt[, clin_c1 := remove_lumped_icd_codes(clin_c1)]
        dt[, clin_c2 := remove_lumped_icd_codes(clin_c2)]

        # Clean clinical columns
        clean_clin_col_res <- clean_clinical_columns(dt)
        dt <- clean_clin_col_res$dt
        discard_rvs_one <- clean_clin_col_res$discard_rvs_one
        discard_rvs_two <- clean_clin_col_res$discard_rvs_two

        # Replace empty strings with NA
        replace_result <- replace_empty_with_na(dt = dt, to_view_checks)
        dt <- replace_result$return_data
        empty_strings_replaced_1 <- replace_result$return_replacement_summary

        # Remap patient data
        remapping_results <- remap_patient_data(dt, to_view_checks)
        dt <- remapping_results$data

        return(
          list(
            return_data = dt,
            return_summary = list(
              rename_success = rename_success,
              ICD_replacements_1 = clin_c1_cleaning_comparison,
              ICD_replacements_2 = clin_c2_cleaning_comparison,
              pat_type_mapped = remapping_results$pat_type_mapped,
              pat_memcat_parent_mapped = remapping_results$pat_memcat_parent_mapped,
              pat_memcat_child_mapped = remapping_results$pat_memcat_child_mapped,
              clin_discharge_mapped = remapping_results$clin_discharge_mapped,
              claim_status_mapped = remapping_results$claim_status_mapped,
              pat_type_unmapped = remapping_results$pat_type_unmapped,
              memcat_parent_unmapped = remapping_results$memcat_parent_unmapped,
              memcat_child_unmapped = remapping_results$memcat_child_unmapped,
              discharge_unmapped = remapping_results$discharge_unmapped,
              claim_status_unmapped = remapping_results$claim_status_unmapped,
              discard_rvs_one = discard_rvs_one,
              discard_rvs_two = discard_rvs_two,
              empty_strings_replaced_1 = empty_strings_replaced_1
            )
          )
        )
      }

      clean_result <- clean_data(chunk)
      chunk <- clean_result$return_data
      chunk_summary <- clean_result$return_summary

      map_rvs_icd9 <- function(clin_rvs, rvs_icd9) {
        split_codes <- split_rvs_codes(rvs_icd9)
        rvs_maps <- create_rvs_map_lists(split_codes$with_drg)

        rvs_map_solo_env <- as.environment(rvs_maps$rvs_map_solo)

        return(list(
          icd9_list = get_icd9_codes(clin_rvs, rvs_map_solo_env),
          rvs_map_list = rvs_maps$rvs_map_list,
          rvss = unique(unlist(clin_rvs)),
          mappable_rvs = intersect(unique(unlist(clin_rvs)), rvs_icd9$rvs),
          unmappable_rvs = setdiff(unique(unlist(clin_rvs)), rvs_icd9$rvs),
          multi_mapped_rvs = intersect(unique(unlist(clin_rvs)), names(rvs_maps$rvs_map_list)),
          without_drg = unique(rvs_icd9[!rvs %in% names(rvs_maps$rvs_map_list)]$rvs)
        ))
      }

      rvs_mapping_result <- map_rvs_icd9(chunk$clin_rvs, rvs_icd9)
      chunk[, icd9_list := rvs_mapping_result$icd9_list]

      clin_c1 <- chunk$clin_c1
      clin_c2 <- chunk$clin_c2
      clin_icd <- chunk$clin_icd

      implement_icd10_mapping <- function(clin_c1, clin_c2, clin_icd, tdrg_icd10) {
        icds <- get_unique_icd_codes(clin_c1, clin_c2, clin_icd)

        thai_icd10_env <- create_thai_icd10_environment(
          unique(tdrg_icd10$CODE)
        )
        neoplasms_env <- create_thai_icd10_environment(
          unique(tdrg_icd10[grepl("/", tdrg_icd10$CODE), "CODE"])
        )

        direct_match_codes <- find_direct_icd_matches(
          icds, thai_icd10_env
        )

        icd_mapping_info <- generate_icd10_mapping(
          icds, thai_icd10_env, neoplasms_env
        )
        icd_mapping <- icd_mapping_info$icd_mapping
        modified_count <- icd_mapping_info$modified_count

        unmatched_icds <- setdiff(icds, names(icd_mapping))

        if (length(unmatched_icds) > 0) {
          unmatched_sources <- data.table(
            code = unmatched_icds, source = NA_character_, count = 0
          )
          for (col_name in c("clin_c1", "clin_c2", "clin_icd")) {
            col_values <- get(col_name)
            unmatched_sources[
              code %in% unlist(col_values),
              source := col_name
            ]
            unmatched_sources[
              code %in% unlist(col_values),
              count := count + table(unlist(col_values))[code]
            ]
          }
          unmatched_sources <- unmatched_sources[order(-count)]
        } else {
          unmatched_sources <- data.table()
        }

        icd10_map <- data.table(
          phl_icd10 = names(icd_mapping),
          tdrg_icd10 = unlist(icd_mapping)
        )
        if (to_debug) fwrite(icd10_map, paste0("cache/icd10_map_file_", year_to_load, ".csv"))
        icd10_env <- list2env(
          setNames(as.list(icd10_map$tdrg_icd10), icd10_map$phl_icd10)
        )

        mapped_columns <- apply_icd10_mapping_to_columns(
          clin_c1, clin_c2, clin_icd, icd10_env
        )

        return(list(
          clin_c1 = mapped_columns$clin_c1,
          clin_c2 = mapped_columns$clin_c2,
          clin_icd = mapped_columns$clin_icd,
          icd10_map_dt = icd10_map,
          unique_icds = icds,
          direct_matches = direct_match_codes,
          unmatched = unmatched_icds,
          unmatched_sources = unmatched_sources
        ))
      }

      icd10_mapping_result <- implement_icd10_mapping(
        clin_c1, clin_c2, clin_icd, tdrg_icd10
      )
      chunk[, clin_c1 := icd10_mapping_result$clin_c1]
      chunk[, clin_c2 := icd10_mapping_result$clin_c2]
      chunk[, clin_icd := icd10_mapping_result$clin_icd]

      chunk_replace_result_two <- replace_empty_with_na(dt = chunk, to_view_checks)
      chunk <- chunk_replace_result_two$return_data
      chunk_summary$empty_strings_replaced_2 <- chunk_replace_result_two$return_replacement_summary

      pdx_result <- apply_find_pdx(
        chunk$clin_c1, chunk$clin_c2, chunk$clin_icd, acc_pdx
      )
      chunk$pdx <- pdx_result$pdx
      chunk$pdx_code <- pdx_result$pdx_code

      # Ensure consistent lengths of clin_rvs and icd9_list
      clin_rvs_len <- lengths(chunk$clin_rvs)
      icd9_list_len <- lengths(chunk$icd9_list)

      max_len <- max(c(clin_rvs_len, icd9_list_len))
      chunk$clin_rvs <- lapply(chunk$clin_rvs, function(x) {
        length(x) <- max_len
        return(x)
      })
      chunk$icd9_list <- lapply(chunk$icd9_list, function(x) {
        length(x) <- max_len
        return(x)
      })

      chunk_summary$unique_icds <- icd10_mapping_result$unique_icds
      chunk_summary$direct_matches <- icd10_mapping_result$direct_matches
      chunk_summary$unmatched <- icd10_mapping_result$unmatched
      chunk_summary$unmatched_sources <- icd10_mapping_result$unmatched_sources
      chunk_summary$icd10_map_dt <- icd10_mapping_result$icd10_map_dt

      chunk_summary$rvss <- rvs_mapping_result$rvss
      chunk_summary$mappable_rvs <- rvs_mapping_result$mappable_rvs
      chunk_summary$unmappable_rvs <- rvs_mapping_result$unmappable_rvs
      chunk_summary$multi_mapped_rvs <- rvs_mapping_result$multi_mapped_rvs
      chunk_summary$without_drg <- rvs_mapping_result$without_drg

      if (to_dec_mem_usage) gc() # debug
      return(
        list(
          return_chunk = chunk,
          return_summary = chunk_summary
        )
      )
    }

    if (to_parallel && is_unix) {
      if (to_debug) message("Conducting mclapply")
      parallel_results <- mclapply(
        chunks, process_chunk,
        mc.cores = nthreads,
        to_view_checks = to_view_checks,
        rvs_icd9 = rvs_icd9,
        tdrg_icd10 = tdrg_icd10,
        acc_pdx = acc_pdx
      )
    } else if (to_parallel && !is_unix) {
      if (to_debug) message("Conducting future_lapply")
      parallel_results <- future_lapply(
        chunks, process_chunk,
        to_view_checks = to_view_checks,
        rvs_icd9 = rvs_icd9,
        tdrg_icd10 = tdrg_icd10,
        acc_pdx = acc_pdx,
        future.seed = global_seed
      )
    } else {
      if (to_debug) message("Conducting lapply")
      parallel_results <- lapply(
        chunks, process_chunk,
        to_view_checks = to_view_checks,
        rvs_icd9 = rvs_icd9,
        tdrg_icd10 = tdrg_icd10,
        acc_pdx = acc_pdx
      )
    }

    parallel_summaries <- lapply(parallel_results, function(res) res$return_summary)
    rbound_dt <- rbindlist(lapply(parallel_results, function(res) res$return_chunk))

    if (to_dec_mem_usage) rm(processed_chunks) # debug

    combined_chunk_summary <- combine_chunk_summaries(parallel_summaries, tmp_nrow, diff_chars)

    if (to_dec_mem_usage) rm(parallel_results) # debug
    if (to_dec_mem_usage) gc() # debug

    acc_pdx_env <- new.env(hash = TRUE, parent = emptyenv())
    for (code in acc_pdx) {
      assign(code, TRUE, envir = acc_pdx_env)
    }

    invalid_pdx_indices <- which(
      !is.na(rbound_dt$pdx) & rbound_dt$pdx != "" & !sapply(rbound_dt$pdx, function(x) exists(x, acc_pdx_env))
    )

    if (length(invalid_pdx_indices) > 0) {
      cat(paste("Invalid PDx found:", rbound_dt$pdx[invalid_pdx_indices]))
      combined_chunk_summary$pdx_success <- FALSE
    } else {
      combined_chunk_summary$pdx_success <- TRUE
    }

    if (to_dec_mem_usage) gc() # debug
    ### END OF PARALLELIZE AND SUMMARIZE DATA

    summarized_dt <- rbound_dt
    combined_parallel_summary <- combined_chunk_summary
    combined_parallel_summary$replacement_summary <- read_in_replacement_summary

    write_intermediate_file(to_write, part, summarized_dt)
    if (to_group) export_for_grouper(summarized_dt, year_to_load, output_txt_file(part))
    ### END OF PROCESS PART ###

    all_parts_summaries[[part]] <- combined_parallel_summary # Save partial summaries to a list
    processing_times[[part]] <- as.numeric(difftime(Sys.time(), start_time, units = "secs")) # Save partial processing time to a list
    # Print status update and ETA
    print_status_update(part, split_parts, processing_times)
    if (part == 1) dim_dt <<- dim(summarized_dt)
    nrow_end[[part]] <<- nrow(summarized_dt)

    if (to_dec_mem_usage) {
      rm(read_in_dt, rbound_dt, summarized_dt)
      gc()
    }
  }

  for (part in 1:split_parts) {
    if (nrow_start[[part]] != nrow_end[[part]]) {
      warning(
        "WARNING: Row Count Mismatch! Part ", part, " has ", nrow_start[[part]],
        " starting rows and ", nrow_end[[part]], " ending rows\n"
      )
      stop("ERROR: Row Count Mismatch")
    }
  }

  cat("\nRow Counts Match for All Parts\n") # only prints if above succeeds

  # Summaries are consolidated from 15 split_parts * 8 chunks = 120 sub outputs
  print_summary_tables( # Print final summaries
    combine_parts_summaries(all_parts_summaries, tmp_nrow),
    end_nrow
  )

  if (to_parallel && !is_unix) plan(sequential) # end parallelization

  # End main execution logic
  if (to_debug) {
    # return(NULL)
  } # debug
}


all_parts_summaries <- list() # initialize list for summaries
dim_dt <- vector() # initialize vector for dt dimensions
processing_times <- nrow_start <- nrow_end <- numeric(split_parts)
nthreads <- parallelly::availableCores() # detect available threads
cat(paste0("Utilizing ", nthreads / 2, " cores (", nthreads, " threads)\n"))

# Call the main function with or without profvis
if (to_profvis) saveWidget(profvis({unified_block()}), here(profvis_path)) else {
  unified_block()
}


print_time_estimates() # Print time estimates along with estimate for full claims file


# in case we want to run this cell independently:
source(here::here("data-cleaning/r_scripts", "03_timing-debug-functions.R"))

# Consolidate all r_scripts scripts into everything.R; useful for debugging
concatenate_r_files(
  here::here("data-cleaning/r_scripts"),
  here::here("data-cleaning/everything/everything.R")
)

if (.Platform$OS.type == "unix") system("cd ~/drg-pipeline && jupyter nbconvert --no-prompt --to script data-cleaning/drg-cleaning.ipynb --output everything/drg-cleaning")


rm(list = ls())
gc()

