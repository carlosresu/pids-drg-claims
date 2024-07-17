source(here("data-cleaning", "r_scripts", "libraries.R"))

main_read_function <- function() {
  if (to_read) {
    if (to_view_checks) {
      print("Reading the entire file...")
    }
    dt <- read_entire_file(drop_cols)

    if (to_sample) {
      if (file.exists(sampled_claims)) {
        if (to_view_checks) {
          print("Sampled file exists. Reading the sampled file...")
        }
        dt <- read_sampled_file()
        # Check if the number of rows matches sample_size
        if (nrow(dt) != sample_size) {
          if (to_view_checks) {
            print(paste(
              "Sampled file does not match sample size. Expected:",
              sample_size, "Found:", nrow(dt), "Re-sampling..."
            ))
          }
          dt <- read_entire_file(drop_cols)
          dt <- sample_data(dt)
          if (to_write) {
            if (to_view_checks) {
              print(paste(
                "to_write is TRUE. Writing the new sample data to file:",
                sampled_claims
              ))
            }
            fwrite(dt, sampled_claims)
          } else {
            if (to_view_checks) {
              print("to_write is FALSE. Not writing the sample data to file.")
            }
          }
        } else {
          if (to_view_checks) {
            print("Sampled file matches sample size.")
          }
        }
      } else {
        if (to_view_checks) {
          print("Sampled file does not exist. Creating new sample...")
        }
        dt <- sample_data(dt)
        if (to_write) {
          if (to_view_checks) {
            print(paste(
              "to_write is TRUE. Writing the new sample data to file:",
              sampled_claims
            ))
          }
          fwrite(dt, sampled_claims)
        } else {
          if (to_view_checks) {
            print("to_write is FALSE. Not writing the sample data to file.")
          }
        }
      }
    }
  } else {
    if (to_sample) {
      if (file.exists(sampled_claims)) {
        if (to_view_checks) {
          print("Sampled file exists. Reading the sampled file...")
        }
        dt <- read_sampled_file()
        # Check if the number of rows matches sample_size
        if (nrow(dt) != sample_size) {
          if (to_view_checks) {
            print(paste(
              "Sampled file does not match sample size. Expected:",
              sample_size, "Found:", nrow(dt), "Re-sampling..."
            ))
          }
          dt <- read_entire_file(drop_cols)
          dt <- sample_data(dt)
          if (to_write) {
            if (to_view_checks) {
              print(paste(
                "to_write is TRUE. Writing the new sample data to file:",
                sampled_claims
              ))
            }
            fwrite(dt, sampled_claims)
          } else {
            if (to_view_checks) {
              print("to_write is FALSE. Not writing the sample data to file.")
            }
          }
        } else {
          if (to_view_checks) {
            print("Sampled file matches sample size.")
          }
        }
      } else {
        if (to_view_checks) {
          print("Sampled file does not exist.")
          print("Reading entire file and creating new sample...")
        }
        dt <- read_entire_file(drop_cols)
        dt <- sample_data(dt)
        if (to_write) {
          if (to_view_checks) {
            print(paste(
              "to_write is TRUE. Writing the new sample data to file:",
              sampled_claims
            ))
          }
          fwrite(dt, sampled_claims)
        } else {
          if (to_view_checks) {
            print("to_write is FALSE. Not writing the sample data to file.")
          }
        }
      }
    } else {
      if (!file.exists(intermediate_file)) {
        stop("Cannot proceed: to_read is FALSE and to_sample is FALSE.
           At least one must be TRUE.")
      } else {
        if (to_view_checks) {
          print("Using existing intermediate file.")
        }
      }
    }
  }
  return(dt)
}

clean_data <- function(dt) {
  # Add year column
  dt[, SRC_YR := as.integer(year_to_load)]

  # Rename columns
  setnames(dt, old = old_colnames, new = new_colnames)

  # Check if all columns were successfully renamed
  if (!all(new_colnames %in% colnames(dt))) {
    missing_cols <- setdiff(new_colnames, colnames(dt))
    warning("Failed to rename the following columns: ", paste(missing_cols, collapse = ", "))
    stop("Column renaming failed.")
  }

  if (to_view_checks) {
    print("Successfully renamed columns; All expected columns exist")
  }

  # Collapse columns clin_icd1 to clin_icd12 into clin_icd
  dt[, clin_icd := collapse_columns(
    mget(paste0("clin_icd", 1:12),
      envir = as.environment(dt)
    ),
    na_like_strings
  )]
  dt[, paste0("clin_icd", 1:12) := NULL]

  # Collapse columns clin_rvs1 to clin_rvs20 into clin_rvs
  dt[, clin_rvs := collapse_columns(
    mget(paste0("clin_rvs", 1:20),
      envir = as.environment(dt)
    ),
    na_like_strings
  )]
  dt[, paste0("clin_rvs", 1:20) := NULL]

  # Remove lumped ICD codes from clin_icd
  dt[, clin_icd := remove_lumped_icd_codes(dt$clin_icd)]

  # Turn clin_icd and clin_rvs into lists
  dt[, clin_icd := split_to_vector(clin_icd)]
  dt[, clin_rvs := split_to_vector(clin_rvs)]

  # Clean and unlump clin_c1 and clin_c2
  dt[, clin_c1_orig := clin_c1]
  dt[, clin_c1 := clean_column(dt$clin_c1, na_like_strings)]
  clin_c1_cleaning_comparison <- dt[
    clin_c1 != clin_c1_orig,
    .(clin_c1_orig, clin_c1)
  ]
  if (to_view_checks) {
    print(head(clin_c1_cleaning_comparison)) # Check: Print head of changes
  }
  dt[, clin_c1_orig := NULL]

  dt[, clin_c2_orig := clin_c2]
  dt[, clin_c2 := clean_column(dt$clin_c2, na_like_strings)]
  clin_c2_cleaning_comparison <- dt[
    clin_c2 != clin_c2_orig,
    .(clin_c2_orig, clin_c2)
  ]
  if (to_view_checks) {
    print(head(clin_c2_cleaning_comparison)) # Check: Print head of changes
  }
  dt[, clin_c2_orig := NULL]

  dt[, clin_c1 := remove_lumped_icd_codes(dt$clin_c1)]
  dt[, clin_c2 := remove_lumped_icd_codes(dt$clin_c2)]

  dt[, clin_c1 := split_to_vector(clin_c1)]
  clin_c1_result <- transfer_extra_icd10s_to_clin_icd(
    dt$clin_icd, dt$clin_c1
  )
  dt[, clin_icd := clin_c1_result$clin_icd]
  dt[, clin_c1 := clin_c1_result$col_first]

  dt[, clin_c2 := split_to_vector(clin_c2)]
  clin_c2_result <- transfer_extra_icd10s_to_clin_icd(dt$clin_icd, dt$clin_c2)
  dt[, clin_icd := clin_c2_result$clin_icd]
  dt[, clin_c2 := clin_c2_result$col_first]

  clin_c1_rvs_results <- append_and_remove_rvs(
    dt$clin_rvs, dt$clin_c1, rvs_icd9
  )
  dt[, clin_rvs := clin_c1_rvs_results$clin_rvs]
  dt[, clin_c1 := clin_c1_rvs_results$col]

  clin_c2_rvs_results <- append_and_remove_rvs(
    dt$clin_rvs, dt$clin_c2, rvs_icd9
  )
  dt[, clin_rvs := clin_c2_rvs_results$clin_rvs]
  dt[, clin_c2 := clin_c2_rvs_results$col]

  dt[, clin_rvs := lapply(clin_rvs, unique)]
  dedup_result <- ensure_unique_icd_codes(
    dt$clin_c1, dt$clin_c2, dt$clin_icd
  )
  dt[, clin_c1 := dedup_result$clin_c1]
  dt[, clin_c2 := dedup_result$clin_c2]
  dt[, clin_icd := dedup_result$clin_icd]

  # Replace empty strings in character and factor columns with NA
  dt <- replace_empty_with_na(dt, to_view_checks)

  warning_thrown <- FALSE

  # Remap and check for patient type
  result <- remap_patient_type(dt$pat_type)
  dt$pat_type <- result$remapped
  if (length(result$unmapped) > 0 && to_view_checks) {
    warning_thrown <- TRUE
    print("Unmapped Patient Types:")
    print(result$unmapped)
  }
  if (warning_thrown && to_view_checks) {
    print("Patient Types:")
    print(unique(dt$pat_type))
  }

  warning_thrown <- FALSE

  # Remap and check for member category parent
  result <- remap_memcat_parent_desc(dt$pat_memcat_parent)
  dt$pat_memcat_parent <- result$remapped
  if (length(result$unmapped) > 0 && to_view_checks) {
    warning_thrown <- TRUE
    print("Unmapped Memcat Parent Types:")
    print(result$unmapped)
  }
  if (warning_thrown && to_view_checks) {
    print("Memcat Parent Types:")
    print(unique(dt$pat_memcat_parent))
  }

  warning_thrown <- FALSE

  # Remap and check for member category child
  result <- remap_memcat_child_desc(dt$pat_memcat_child)
  dt$pat_memcat_child <- result$remapped
  if (length(result$unmapped) > 0 && to_view_checks) {
    warning_thrown <- TRUE
    print("Unmapped Memcat Child Types:")
    print(result$unmapped)
  }
  if (warning_thrown && to_view_checks) {
    print("Memcat Child Types:")
    print(unique(dt$pat_memcat_child))
  }

  warning_thrown <- FALSE

  # Remap and check for clinical discharge disposition
  result <- remap_disposition(dt$clin_discharge)
  dt$clin_discharge <- result$remapped
  if (length(result$unmapped) > 0 && to_view_checks) {
    warning_thrown <- TRUE
    print("Unmapped Discharge Types:")
    print(result$unmapped)
  }
  if (warning_thrown && to_view_checks) {
    print("Discharge Types:")
    print(unique(dt$clin_discharge))
  }

  return(dt)
}

process_chunk <- function(chunk) {
  #' @title Process Data Chunk
  #' @description Processes a data chunk by cleaning, mapping codes, replacing
  #' empty values, and finding the primary diagnosis (PDX).
  #' @param chunk A data.table chunk to process.
  #' @return The processed data.table chunk.
  #'
  #' @details
  #' This function performs the following operations on the data chunk:
  #' - Suppresses console output to keep the environment clean.
  #' - Cleans the data using the clean_data function.
  #' - Maps RVS and ICD-10 codes.
  #' - Replaces empty strings with NA values.
  #' - Finds the primary diagnosis (PDX) using the apply_find_pdx function.
  #'

  # Suppress output
  if (to_view_checks) {
    print("Viewing checks")
  } else {
    sink(tempfile())
    on.exit(sink(), add = TRUE)
  }

  # Clean data
  chunk <- clean_data(chunk)

  # Map RVS codes
  chunk[, icd9_list := map_rvs_icd9(clin_rvs, rvs_icd9)]

  # Map ICD codes
  clin_c1 <- chunk$clin_c1
  clin_c2 <- chunk$clin_c2
  clin_icd <- chunk$clin_icd

  mapped_columns <- implement_icd10_mapping(
    clin_c1,
    clin_c2,
    clin_icd,
    tdrg_icd10,
    rows_to_show = 10
  )

  # Save the results back to the data.table
  chunk[, clin_c1 := mapped_columns$clin_c1]
  chunk[, clin_c2 := mapped_columns$clin_c2]
  chunk[, clin_icd := mapped_columns$clin_icd]

  # Replace empty strings with NA values
  chunk <- replace_empty_with_na(chunk, to_view_checks)

  # Find PDX
  # chunk <- apply_find_pdx(chunk)
  pdx_result <- apply_find_pdx(chunk$clin_c1, chunk$clin_c2, chunk$clin_icd, acc_pdx)
  chunk$pdx <- pdx_result$pdx
  chunk$pdx_code <- pdx_result$pdx_code

  return(chunk)
}
