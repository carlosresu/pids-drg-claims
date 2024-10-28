print_summary_tables <- function(final_combined_summaries, masterdt = master_dt) {
  summary <- final_combined_summaries

  cat("\nRename Success:\n", summary$final_rename_success, "")

  final_icd_replacements <- unique(rbind(
    summary$final_ICD_replacements_1,
    summary$final_ICD_replacements_2
  ))

  if (nrow(final_icd_replacements) > 0) {
    print(kable(final_icd_replacements,
      format = "markdown",
      caption = "ICD Normalized Text for clin c1 & c2 Before Splitting"
    ))
  } else {
    cat("\nNo ICD replacements found.\nNote: commas, asterisks, plus signs, and whitespaces are ignored.\n")
  }

  # Inline display unique mappings logic
  display_mappings <- function(mapped_data, mapping_name) {
    if (!all(c("Original", "Mapped") %in% names(mapped_data))) {
      stop("Data must contain 'Original' and 'Mapped' columns.")
    }
    unique_mappings <- unique(mapped_data)
    print(kable(unique_mappings,
      format = "markdown",
      caption = sprintf("Unique Mappings for %s", mapping_name)
    ))
  }

  display_mappings(summary$final_pat_type_mapped, "Patient Type")
  display_mappings(summary$final_memcat_parent_mapped, "Memcat Parent")
  display_mappings(summary$final_memcat_child_mapped, "Memcat Child")
  display_mappings(summary$final_clin_discharge_mapped, "Discharge")
  display_mappings(summary$final_claim_status_mapped, "Claim Status")

  final_discard_rvs <- rbind(
    summary$final_discard_rvs_one,
    summary$final_discard_rvs_two
  )[, .(count = sum(count)), by = CODE][order(-count)]

  if (nrow(final_discard_rvs) > 0) {
    print(kable(final_discard_rvs,
      format = "markdown",
      caption = "Discarded RVS Codes"
    ))
  } else {
    cat("\nNo RVS codes discarded.\n\n")
  }

  display_empty_string_replacements <- function(data, set_name) {
    if (nrow(data) > 0) {
      print(kable(data, format = "markdown", caption = paste0("Empty Strings Replaced (", set_name, " Set)")))
    } else {
      cat(paste0("\nNo empty strings replaced in the ", set_name, " set.\n\n"))
    }
  }

  display_empty_string_replacements(summary$final_empty_strings_replaced_0, "Zeroth")
  display_empty_string_replacements(summary$final_empty_strings_replaced_1, "First")
  display_empty_string_replacements(summary$final_empty_strings_replaced_2, "Second")

  cat(
    sprintf("There are %d RVS codes without an ICD-9CM equivalent.\n", summary$final_without_drg),
    sprintf("There are %d unique RVS codes.\n", summary$final_rvss),
    sprintf(
      "%d (%.2f%%) have an ICD-9-CM mapping.\n",
      summary$final_mappable_rvs, (summary$final_mappable_rvs / summary$final_rvss) * 100
    ),
    sprintf(
      "%d (%.2f%%) have multiple ICD-9 equivalents.\n",
      summary$final_multi_mapped_rvs, (summary$final_multi_mapped_rvs / summary$final_rvss) * 100
    )
  )

  cat(
    sprintf("\nThere are %d unique ICD-10 codes.\n", summary$final_unique_icds),
    sprintf(
      "%d (%.2f%%) are directly in the Thai ICD-10 library.\n",
      summary$final_direct_matches, (summary$final_direct_matches / summary$final_unique_icds) * 100
    ),
    sprintf(
      "Total %d codes were mapped to the Thai ICD10 library.\n",
      summary$final_unique_icds - summary$final_unmatched
    ),
    sprintf(
      "%d codes were modified to match.\n",
      summary$final_unique_icds - summary$final_unmatched - summary$final_direct_matches
    ),
    sprintf("%d codes could not be mapped.\n", summary$final_unmatched)
  )

  unique_icd10_map <- summary$final_icd10_map_dt[phl_icd10 != thai_icd10]
  if (nrow(unique_icd10_map) > 0) {
    unique_icd10_map[, char_diff := abs(nchar(phl_icd10) - nchar(thai_icd10))]
    unique_icd10_map <- unique_icd10_map[order(-char_diff)]
    print(kable(unique_icd10_map, format = "markdown", caption = "Modified ICD-10 Codes"))
  } else {
    cat("\nNo modified ICD-10 codes found.\n\n")
  }

  check_unmatched_icd_codes <- function(masterdt, icd_mapping) {
    unmatched_list <- list()
    columns_to_check <- c("c1", "c2", "clin_icd")

    for (col in columns_to_check) {
      flattened_column <- unlist(masterdt[[col]], recursive = TRUE, use.names = FALSE)
      flattened_column <- flattened_column[!is.na(flattened_column) & flattened_column != "NA"]
      unmatched_codes <- flattened_column[!flattened_column %in% names(icd_mapping)]

      if (length(unmatched_codes) > 0) {
        unmatched_list[[col]] <- data.table(column = col, code = unmatched_codes)
      }
    }

    if (length(unmatched_list) > 0) {
      final_unmatched_sources <- rbindlist(unmatched_list, fill = TRUE)[, .(count = .N), by = .(column, code)][order(-count)]
      print(kable(final_unmatched_sources, format = "markdown", caption = "Invalid ICD-10 Codes"))
    } else {
      cat("\nAll resulting ICD-10 codes are present in the Thai library.\n\n")
    }
  }

  icd_mapping <- map_icd10(masterdt$c1, masterdt$c2, masterdt$clin_icd)$icd_mapping_res
  check_unmatched_icd_codes(masterdt, icd_mapping)

  cat("\nAll PDx's are in the list of acceptable PDx's:\n", summary$final_rename_success, "")
}
