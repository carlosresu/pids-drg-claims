print_summary_tables <- function(final_combined_summaries, masterdt = master_dt) {
  summary <- final_combined_summaries
  # to_debug <- TRUE
  # Rename Success
  cat("\nRename Success:\n", summary$final_rename_success, "\n")
  if (to_debug) cat("\nfinal_ICD_replacements_1\n")
  if (to_debug) str(summary$final_ICD_replacements_1)
  if (to_debug) cat("\nfinal_ICD_replacements_2\n")
  if (to_debug) str(summary$final_ICD_replacements_2)

  # Combine ICD Replacements
  final_icd_replacements <- unique(rbind(
    summary$final_ICD_replacements_1,
    summary$final_ICD_replacements_2
  ))

  # Print ICD Replacements
  if (nrow(final_icd_replacements) > 0) {
    print(knitr::kable(final_icd_replacements,
      format = "markdown",
      caption = "ICD Normalized Text for clin c1 & c2 Before Splitting
(Note: differences of only one period symbol are ignored)"
    ))
  } else {
    cat("\nNo ICD replacements found.\nNote: periods and whitespaces are ignored.\n")
  }

  # Inline Display Function for Unique Mappings
  display_mappings <- function(mapped_data, mapping_name) {
    if (!all(c("Original", "Mapped") %in% names(mapped_data))) {
      stop("Data must contain 'Original' and 'Mapped' columns.")
    }
    unique_mappings <- unique(mapped_data)
    print(knitr::kable(unique_mappings,
      format = "markdown",
      caption = sprintf("Unique Mappings for %s", mapping_name)
    ))
  }

  # Display Unique Mappings for Different Categories
  display_mappings(summary$final_pat_type_mapped, "Patient Type")
  display_mappings(summary$final_pat_memcat_parent_mapped, "Memcat Parent")
  display_mappings(summary$final_pat_memcat_child_mapped, "Memcat Child")
  display_mappings(summary$final_clin_discharge_mapped, "Discharge")
  display_mappings(summary$final_claim_status_mapped, "Claim Status")

  # Combine and Display Discarded RVS Codes
  final_discard_rvs <- rbind(
    summary$final_discard_rvs_one,
    summary$final_discard_rvs_two
  )[, .(count = sum(count)), by = CODE][order(-count)]

  if (nrow(final_discard_rvs) > 0) {
    print(knitr::kable(final_discard_rvs,
      format = "markdown",
      caption = "Discarded RVS Codes"
    ))
  } else {
    cat("\nNo RVS codes discarded.\n\n")
  }

  # Inline Display Function for Empty String Replacements
  display_empty_string_replacements <- function(data, set_name) {
    if (nrow(data) > 0) {
      print(knitr::kable(data, format = "markdown", caption = paste0("Empty Strings Replaced (", set_name, " Set)")))
    } else {
      cat(paste0("\nNo empty strings replaced in the ", set_name, " set.\n\n"))
    }
  }

  # Display Empty String Replacements
  display_empty_string_replacements(summary$final_empty_strings_replaced_1, "First")
  display_empty_string_replacements(summary$final_empty_strings_replaced_2, "Second")

  # Print RVS Code Statistics
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

  # Print ICD-10 Mapping Statistics
  cat(
    sprintf("\nThere are %d unique ICD-10 codes.\n", summary$final_unique_icds),
    sprintf(
      "%d (%.2f%%) are directly in the Thai ICD-10 library.\n",
      summary$final_direct_matches, (summary$final_direct_matches / summary$final_unique_icds) * 100
    ),
    sprintf(
      "Total %d codes were mapped to the Thai ICD10 library.\n",
      summary$final_unique_icds - length(summary$final_unmatched_codes)
    ),
    sprintf(
      "%d codes were modified to match.\n",
      summary$final_unique_icds - length(summary$final_unmatched_codes) - summary$final_direct_matches
    ),
    sprintf("%d codes could not be mapped.\n", length(summary$final_unmatched_codes))
  )

  # Print Modified ICD-10 Codes
  print_modified_icd10_codes <- function(summary) {
    unique_icd10_map <- summary$final_modified_matches

    if (nrow(unique_icd10_map) > 0) {
      # Order the data.table by char_diff
      unique_icd10_map <- unique_icd10_map[order(-char_diff)]

      # Print as markdown table
      print(knitr::kable(unique_icd10_map,
        format = "markdown",
        caption = "Modified ICD-10 Codes"
      ))

      # Print the number of rows
      cat("nrow Modified ICD-10 Codes: ", nrow(unique_icd10_map), "\n")
    } else {
      cat("\nNo modified ICD-10 codes found.\n\n")
    }
  }

  # Example usage to print the modified codes
  print_modified_icd10_codes(list(final_modified_matches = final_modified_matches))

  if (length(summary$final_unmatched_codes) > 0) {
    # Convert the unmatched codes into a data.table with counts
    final_unmatched_codes <- data.table(
      code = summary$final_unmatched_codes
    )[, .(count = .N), by = code][order(-count)] # Aggregate by code and order by count

    # Print the final unmatched sources as a markdown table
    print(knitr::kable(final_unmatched_codes,
      format = "markdown",
      caption = "Invalid ICD-10 Codes"
    ))
    cat("\nnrow invalid ICD-10 codes: ", nrow(final_unmatched_codes), "\n")
  } else {
    cat("\nAll resulting ICD-10 codes are present in the Thai library.\n\n")
  }

  cat("\nAll PDx's are in the list of acceptable PDx's:\n", summary$final_pdx_success, "")
}
