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
    warning(
      "Failed to rename the following columns: ",
      paste(missing_cols, collapse = ", ")
    )
    rename_success <- FALSE
    stop("Column renaming failed.")
  }

  if (to_view_checks) {
    rename_success <- TRUE
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
    clin_c1_cleaning_comparison <- clin_c1_cleaning_comparison
  } else {
    clin_c1_cleaning_comparison <- data.table()
  }
  dt[, clin_c1_orig := NULL]

  dt[, clin_c2_orig := clin_c2]
  dt[, clin_c2 := clean_column(dt$clin_c2, na_like_strings)]
  clin_c2_cleaning_comparison <- dt[
    clin_c2 != clin_c2_orig,
    .(clin_c2_orig, clin_c2)
  ]
  if (to_view_checks) {
    clin_c2_cleaning_comparison <- clin_c2_cleaning_comparison
  } else {
    clin_c2_cleaning_comparison <- data.table()
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
  clin_c1_discarded_rvs <- clin_c1_rvs_results$discarded_rvs

  clin_c2_rvs_results <- append_and_remove_rvs(
    dt$clin_rvs, dt$clin_c2, rvs_icd9
  )
  dt[, clin_rvs := clin_c2_rvs_results$clin_rvs]
  dt[, clin_c2 := clin_c2_rvs_results$col]
  clin_c2_discarded_rvs <- clin_c2_rvs_results$discarded_rvs

  dt[, clin_rvs := lapply(clin_rvs, unique)]
  dedup_result <- ensure_unique_icd_codes(
    dt$clin_c1, dt$clin_c2, dt$clin_icd
  )
  dt[, clin_c1 := dedup_result$clin_c1]
  dt[, clin_c2 := dedup_result$clin_c2]
  dt[, clin_icd := dedup_result$clin_icd]

  # Replace empty strings in character and factor columns with NA
  replace_result <- replace_empty_with_na(dt, to_view_checks)
  dt <- replace_result$data
  empty_strings_replaced_1 <- replace_result$replacement_summary

  pat_unmap <- NULL
  parent_unmap <- NULL
  child_unmap <- NULL
  discharge_unmap <- NULL

  warning_thrown <- FALSE

  # Remap and check for patient type
  result <- remap_patient_type(dt$pat_type)
  dt$pat_type <- result$remapped
  if (length(result$unmapped) > 0 && to_view_checks) {
    warning_thrown <- TRUE
    # print("Unmapped Patient Types:")
    # print(result$unmapped)
    pat_unmap <- result$unmapped
  }

  warning_thrown <- FALSE

  # Remap and check for member category parent
  result <- remap_memcat_parent_desc(dt$pat_memcat_parent)
  dt$pat_memcat_parent <- result$remapped
  if (length(result$unmapped) > 0 && to_view_checks) {
    warning_thrown <- TRUE
    # print("Unmapped Memcat Parent Types:")
    # print(result$unmapped)
    parent_unmap <- result$unmapped
  }

  warning_thrown <- FALSE

  # Remap and check for member category child
  result <- remap_memcat_child_desc(dt$pat_memcat_child)
  dt$pat_memcat_child <- result$remapped
  if (length(result$unmapped) > 0 && to_view_checks) {
    warning_thrown <- TRUE
    # print("Unmapped Memcat Child Types:")
    # print(result$unmapped)
    child_unmap <- result$unmapped
  }

  warning_thrown <- FALSE

  # Remap and check for clinical discharge disposition
  result <- remap_disposition(dt$clin_discharge)
  dt$clin_discharge <- result$remapped
  if (length(result$unmapped) > 0 && to_view_checks) {
    warning_thrown <- TRUE
    # print("Unmapped Discharge Types:")
    # print(result$unmapped)
    discharge_unmap <- result$unmapped
  }

  return(list(
    data = dt,
    rename_success = rename_success,
    ICD_replacements_1 = clin_c1_cleaning_comparison,
    ICD_replacements_2 = clin_c2_cleaning_comparison,
    pat_type_unmapped = pat_unmap,
    memcat_parent_unmapped = parent_unmap,
    memcat_child_unmapped = child_unmap,
    discharge_unmapped = discharge_unmap,
    discard_rvs_one = clin_c1_discarded_rvs,
    discard_rvs_two = clin_c2_discarded_rvs,
    empty_strings_replaced_1 = empty_strings_replaced_1
  ))
}

process_chunk <- function(
    chunk, to_view_checks, rvs_icd9, tdrg_icd10, acc_pdx) {
  # Suppress output
  if (to_view_checks) {
    # print("Viewing checks")
  } else {
    sink(tempfile())
    on.exit(sink(), add = TRUE)
  }

  # Clean data
  clean_result <- clean_data(chunk)
  chunk <- clean_result$data

  # Store chunk summaries
  summary <- list()
  summary$rename_success <- clean_result$rename_success
  summary$ICD_replacements_1 <- clean_result$ICD_replacements_1
  summary$ICD_replacements_2 <- clean_result$ICD_replacements_2
  summary$pat_type_unmapped <- clean_result$pat_type_unmapped
  summary$memcat_parent_unmapped <- clean_result$memcat_parent_unmapped
  summary$memcat_child_unmapped <- clean_result$memcat_child_unmapped
  summary$discharge_unmapped <- clean_result$discharge_unmapped
  summary$discard_rvs_one <- clean_result$discard_rvs_one
  summary$discard_rvs_two <- clean_result$discard_rvs_two
  summary$empty_strings_replaced_1 <- clean_result$empty_strings_replaced_1

  # Map RVS codes and collect summary statistics
  rvs_mapping_result <- map_rvs_icd9(chunk$clin_rvs, rvs_icd9)
  chunk[, icd9_list := rvs_mapping_result$icd9_list]
  summary$rvs_mapping_summary <- rvs_mapping_result$summary_statistics

  # Map ICD codes
  clin_c1 <- chunk$clin_c1
  clin_c2 <- chunk$clin_c2
  clin_icd <- chunk$clin_icd

  mapped_columns <- implement_icd10_mapping(
    clin_c1,
    clin_c2,
    clin_icd,
    tdrg_icd10,
    rows_to_show = Inf
  )

  # Save the results back to the data.table
  chunk[, clin_c1 := mapped_columns$clin_c1]
  chunk[, clin_c2 := mapped_columns$clin_c2]
  chunk[, clin_icd := mapped_columns$clin_icd]

  # Replace empty strings with NA values again after mapping
  chunk_replace_result <- replace_empty_with_na(chunk, to_view_checks)
  chunk <- chunk_replace_result$data
  summary$empty_strings_replaced_2 <- chunk_replace_result$replacement_summary

  # Find PDX
  pdx_result <- apply_find_pdx(
    chunk$clin_c1, chunk$clin_c2, chunk$clin_icd, acc_pdx
  )
  chunk$pdx <- pdx_result$pdx
  chunk$pdx_code <- pdx_result$pdx_code

  # Collect ICD-10 mapping statistics
  summary$total_unique_icd_count <- mapped_columns$var1
  summary$direct_match_count <- mapped_columns$var2
  summary$modified_count <- mapped_columns$var5
  summary$total_mapped_count <- mapped_columns$var4
  summary$unmapped_icd_count <- mapped_columns$var6
  summary$unmapped_icds <- mapped_columns$var7
  summary$comparison_table <- mapped_columns$comparison_table
  summary$invalid_icds_table <- mapped_columns$invalid_icds_table

  # Debug: Print the summary to verify fields
  # print("Summary fields:")
  # print(names(summary))

  return(list(chunk = chunk, summary = summary))
}


parallelize_and_summarize <- function(
    dt, num_cores, to_view_checks, global_seed,
    rows_to_show, rvs_icd9, tdrg_icd10, acc_pdx) {
  chunk_size <- ceiling(nrow(dt) / num_cores)
  chunks <- split(dt, rep(1:num_cores, each = chunk_size, length.out = nrow(dt)))

  # Plan for parallel processing
  plan(multisession, workers = num_cores)

  # Process each chunk in parallel
  parallel_results <- future_lapply(
    chunks, process_chunk,
    to_view_checks = to_view_checks,
    rvs_icd9 = rvs_icd9,
    tdrg_icd10 = tdrg_icd10,
    acc_pdx = acc_pdx,
    future.seed = global_seed
  )

  # Combine processed chunks
  processed_chunks <- lapply(parallel_results, function(res) res$chunk)
  dt <- rbindlist(processed_chunks)

  # Combine summaries
  summaries <- lapply(parallel_results, function(res) res$summary)

  consolidated_summary <- list(
    rename_success = all(sapply(summaries, function(s) s$rename_success)),
    ICD_replacements_1 = combine_comparison_tables(
      summaries, "ICD_replacements_1", rows_to_show
    ),
    ICD_replacements_2 = combine_comparison_tables(
      summaries, "ICD_replacements_2", rows_to_show
    ),
    pat_type_unmapped = unique(
      unlist(lapply(summaries, function(s) s$pat_type_unmapped))
    ),
    memcat_parent_unmapped = unique(
      unlist(lapply(summaries, function(s) s$memcat_parent_unmapped))
    ),
    memcat_child_unmapped = unique(
      unlist(lapply(summaries, function(s) s$memcat_child_unmapped))
    ),
    discharge_unmapped = unique(
      unlist(lapply(summaries, function(s) s$discharge_unmapped))
    ),
    discard_rvs_one = combine_discarded_rvs_tables(
      summaries, "discard_rvs_one", rows_to_show
    ),
    discard_rvs_two = combine_discarded_rvs_tables(
      summaries, "discard_rvs_two", rows_to_show
    ),
    empty_strings_replaced_1 = combine_replace_empty_tables(
      summaries, "empty_strings_replaced_1",
      rows_to_show = Inf
    ),
    empty_strings_replaced_2 = combine_replace_empty_tables(
      summaries, "empty_strings_replaced_2",
      rows_to_show = Inf
    ),
    icd_comparison_table = combine_icd_comparison_table(
      lapply(summaries, function(s) s$comparison_table)
    ),
    invalid_icds_table = combine_invalid_icd_table(
      lapply(summaries, function(s) s$invalid_icds_table)
    )
  )

  # Aggregate summary statistics
  rvss <- unique(unlist(dt$clin_rvs))
  total_rvs_count <- length(rvss)
  rvs_map_list <- create_rvs_map_lists(
    split_rvs_codes(rvs_icd9)$with_drg
  )$rvs_map_list

  without_drg_count <- length(
    unique(rvs_icd9[!rvs %in% names(rvs_map_list)]$rvs)
  )
  mappable_rvs <- intersect(rvss, rvs_icd9$rvs)
  mappable_rvs_count <- length(mappable_rvs)
  mappable_rvs_percentage <- (mappable_rvs_count / length(rvss)) * 100

  multi_mapped_rvs <- intersect(rvss, names(rvs_map_list))
  multi_mapped_rvs_count <- length(multi_mapped_rvs)
  multi_mapped_rvs_percentage <- (
    multi_mapped_rvs_count / mappable_rvs_count) * 100

  unmappable_rvs <- setdiff(rvss, rvs_icd9$rvs)
  unmappable_rvs_count <- length(unmappable_rvs)
  unmappable_rvs_percentage <- (unmappable_rvs_count / length(rvss)) * 100

  aggregate_statistics <- list(
    total_rvs_count = total_rvs_count,
    without_drg_count = without_drg_count,
    mappable_rvs_count = mappable_rvs_count,
    mappable_rvs_percentage = mappable_rvs_percentage,
    multi_mapped_rvs_count = multi_mapped_rvs_count,
    multi_mapped_rvs_percentage = multi_mapped_rvs_percentage,
    unmappable_rvs_count = unmappable_rvs_count,
    unmappable_rvs_percentage = unmappable_rvs_percentage
  )

  # Combine ICD-10 mapping statistics
  icd10_stats <- aggregate_icd10_stats(summaries)
  aggregate_statistics <- c(aggregate_statistics, icd10_stats)

  return(
    list(
      dt = dt,
      consolidated_summary = consolidated_summary,
      aggregate_statistics = aggregate_statistics
    )
  )
}

print_aggregate_summary_stats <- function(aggregate_statistics, rows_to_show) {
  cat(
    sprintf(
      "There are %d RVS codes without an ICD-9CM",
      aggregate_statistics$without_drg_count
    ), "equivalent recognized by the TDRG ICD9CM\n"
  )
  cat(
    sprintf(
      "There are %d unique RVS codes that appear in the claims.\n",
      aggregate_statistics$total_rvs_count
    )
  )
  cat(
    sprintf(
      "Of these, %d (%.2f%%) have a mapping to an ICD-9-CM code.\n",
      aggregate_statistics$mappable_rvs_count,
      aggregate_statistics$mappable_rvs_percentage
    )
  )
  cat(
    sprintf(
      "Of these, there are %d (%.2f%%)",
      aggregate_statistics$multi_mapped_rvs_count,
      aggregate_statistics$multi_mapped_rvs_percentage
    ), "with more than one ICD9 equivalent",
    "recognized by the Thai ICD9 library.\n"
  )
  cat(
    sprintf(
      "There are %d (%.2f%%) with no ICD-9-CM equivalents.\n\n\n",
      aggregate_statistics$unmappable_rvs_count,
      aggregate_statistics$unmappable_rvs_percentage
    )
  )
  cat(
    sprintf(
      "There are %d unique entries for ICD-10 codes, of which %d (%.2f%%)",
      aggregate_statistics$total_unique_icd_count,
      aggregate_statistics$direct_match_count,
      aggregate_statistics$direct_match_percentage
    ), "are directly in the Thai ICD-10 library\n"
  )
  cat(
    sprintf(
      "The modifications led to a total of %d codes ",
      aggregate_statistics$total_mapped_count
    ), "being mapped to an equivalent in the Thai ICD10 library.\n"
  )
  cat(
    sprintf(
      "Out of these, %d were modified to match.\n",
      aggregate_statistics$modified_count
    )
  )
  cat(
    sprintf(
      "There are %d codes that could not",
      aggregate_statistics$unmapped_icd_count
    ), "be mapped to the Thai ICD10 library:\n"
  )
  # print(head(aggregate_statistics$combined_unmapped_icds, rows_to_show))
}

print_summary_tables <- function(result, rows_to_show) {
  dt <- result$dt
  consolidated_summary <- result$consolidated_summary
  aggregate_statistics <- result$aggregate_statistics

  # Print the consolidated summary
  cat("Rename Success:\n", consolidated_summary$rename_success, "\n\n")

  if (nrow(consolidated_summary$ICD_replacements_1) > 0) {
    print(kable(head(consolidated_summary$ICD_replacements_1, rows_to_show),
      format = "markdown",
      caption = "ICD Replacements 1"
    ))
  } else {
    cat("\nNo ICD replacements found in the first set.\n\n")
  }

  if (nrow(consolidated_summary$ICD_replacements_2) > 0) {
    print(kable(head(consolidated_summary$ICD_replacements_2, rows_to_show),
      format = "markdown",
      caption = "ICD Replacements 2"
    ))
  } else {
    cat("\nNo ICD replacements found in the second set.\n\n")
  }

  if (is.null(consolidated_summary$pat_type_unmapped)) {
    cat("Patient Type Unmapped: NULL\n\n")
  } else {
    cat("Patient Type Unmapped:\n", consolidated_summary$pat_type_unmapped, "\n\n")
  }

  if (is.null(consolidated_summary$memcat_parent_unmapped)) {
    cat("Memcat Parent Unmapped: NULL\n\n")
  } else {
    cat("Memcat Parent Unmapped:\n", consolidated_summary$memcat_parent_unmapped, "\n\n")
  }

  if (is.null(consolidated_summary$memcat_child_unmapped)) {
    cat("Memcat Child Unmapped: NULL\n\n")
  } else {
    cat("Memcat Child Unmapped:\n", consolidated_summary$memcat_child_unmapped, "\n\n")
  }

  if (is.null(consolidated_summary$discharge_unmapped)) {
    cat("Discharge Unmapped: NULL\n\n")
  } else {
    cat("Discharge Unmapped:\n", consolidated_summary$discharge_unmapped, "\n\n")
  }

  if (nrow(consolidated_summary$discard_rvs_one) > 0) {
    print(kable(head(consolidated_summary$discard_rvs_one, rows_to_show),
      format = "markdown",
      caption = "Discarded RVS Codes One"
    ))
  } else {
    cat("\nNo RVS codes discarded in the first set.\n\n")
  }

  if (nrow(consolidated_summary$discard_rvs_two) > 0) {
    print(kable(head(consolidated_summary$discard_rvs_two, rows_to_show),
      format = "markdown",
      caption = "Discarded RVS Codes Two"
    ))
  } else {
    cat("\nNo RVS codes discarded in the second set.\n\n")
  }

  if (nrow(consolidated_summary$empty_strings_replaced_1) > 0) {
    print(kable(head(consolidated_summary$empty_strings_replaced_1, rows_to_show),
      format = "markdown",
      caption = "Empty Strings Replaced (First Set)"
    ))
  } else {
    cat("\nNo empty strings replaced in the first set.\n\n")
  }

  if (nrow(consolidated_summary$empty_strings_replaced_2) > 0) {
    print(kable(head(consolidated_summary$empty_strings_replaced_2, rows_to_show),
      format = "markdown",
      caption = "Empty Strings Replaced (Second Set)"
    ))
  } else {
    cat("\nNo empty strings replaced in the second set.\n\n")
  }

  # Print aggregate summary statistics
  print_aggregate_summary_stats(aggregate_statistics, rows_to_show)

  # Print comparison table and invalid ICDs table within the consolidated summary
  if (nrow(consolidated_summary$icd_comparison_table) > 0) {
    print(kable(head(consolidated_summary$icd_comparison_table, rows_to_show),
      format = "markdown",
      caption = "Comparison of ICD Codes Before and After Mapping"
    ))
  } else {
    cat("\nNo ICD codes changed during mapping.\n\n")
  }

  if (nrow(consolidated_summary$invalid_icds_table) > 0) {
    print(kable(head(consolidated_summary$invalid_icds_table, rows_to_show),
      format = "markdown",
      caption = "Invalid ICD Codes Not Found in Thai or PhilHealth Libraries"
    ))
  } else {
    cat("\nAll resulting ICD codes are valid and present in the libraries.\n\n")
  }
}
