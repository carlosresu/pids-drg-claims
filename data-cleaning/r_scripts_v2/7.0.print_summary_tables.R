print_summary_tables <- function(final_combined_summaries) {
  summary <- final_combined_summaries

  # Step 1: Print Rename Success
  cat("\nRename Success:\n", summary$rename_success, "\n")

  # Step 2: Combine and Print ICD Replacements
  final_icd_replacements <- rbind(
    summary$ICD_replacements_1,
    summary$ICD_replacements_2
  )

  final_icd_replacements <- unique(final_icd_replacements)

  print_icd_normalized_table <- function(icd_replacements) {
    # Ensure we only keep one pair of 'old_code' and 'new_code' columns with the 'count'
    relevant_columns <- c("old_code", "new_code", "count")
    unique_replacements <- icd_replacements[, ..relevant_columns]

    # Print the table if there are rows
    if (nrow(unique_replacements) > 0) {
      print(knitr::kable(
        unique_replacements,
        format = "markdown",
        caption = "ICD Normalized Text for clin c1 & c2 Before Splitting
(Note: differences of only one period symbol are ignored)"
      ))
    } else {
      cat("\nNo ICD replacements found.\nNote: periods and whitespaces are ignored.\n")
    }
  }

  print_icd_normalized_table(unique(final_icd_replacements))

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

  display_mappings(summary$pat_type_mapped, "Patient Type")
  display_mappings(summary$pat_memcat_parent_mapped, "Memcat Parent")
  display_mappings(summary$pat_memcat_child_mapped, "Memcat Child")
  display_mappings(summary$clin_discharge_mapped, "Discharge")
  display_mappings(summary$claim_status_mapped, "Claim Status")

  # Step 4: Combine and Display Discarded RVS Codes
  discarded_rvs_one <- combine_discarded_rvs_tables(list(summary), "discard_rvs_one")
  discarded_rvs_two <- combine_discarded_rvs_tables(list(summary), "discard_rvs_two")
  # Combine and Display Discarded RVS Codes
  final_discard_rvs <- rbind(
    discarded_rvs_one,
    discarded_rvs_two
  )[, .(count = sum(count)), by = CODE][order(-count)]

  if (nrow(final_discard_rvs) > 0) {
    print(knitr::kable(final_discard_rvs,
      format = "markdown",
      caption = "Discarded RVS Codes"
    ))
  } else {
    cat("\nNo RVS codes discarded.\n\n")
  }

  display_replacements <- function(data, set_name, replace_with) {
    # Format the data based on the specified replacement type
    formatted_data <- final_combine_replace_tables(data, replace_with)

    # Define the replacement description based on `replace_with`
    replacement_desc <- if (replace_with == "NA_character_") {
      "Empty Strings Replaced"
    } else {
      "NA Strings Replaced"
    }

    # Display the formatted table or a message if no replacements
    if (nrow(formatted_data) > 0) {
      print(knitr::kable(
        formatted_data,
        format = "markdown",
        caption = paste0(replacement_desc, " (", set_name, " Set)")
      ))
    } else {
      cat(paste0("\nNo ", replacement_desc, " in the ", set_name, " set.\n\n"))
    }
  }

  # Display Empty String Replacements
  display_replacements(summary$replacement_summary, "Zeroth", "NA_character_")
  display_replacements(summary$empty_replaced_with_na_1, "First", "NA_character_")
  display_replacements(summary$NA_replaced_with_empty_1, "Second", "character(0)")
  display_replacements(summary$NA_replaced_with_empty_2, "Third", "character(0)")
  display_replacements(summary$NA_replaced_with_empty_3, "Fourth", "character(0)")

  # Step 8: Print RVS Code and ICD-10 Statistics
  cat(sprintf("There are %d valid RVS codes without an ICD-9CM equivalent.\n", length(summary$without_drg)))
  cat(sprintf("There are %d unique RVS codes.\n", length(summary$rvss)))
  cat(sprintf(
    "%d (%.2f%%) have an ICD-9-CM mapping.\n",
    length(summary$mappable_rvs),
    (length(summary$mappable_rvs) / length(summary$rvss)) * 100
  ))
  cat(sprintf(
    "%d (%.2f%%) have multiple ICD-9 equivalents.\n",
    length(summary$multi_mapped_rvs),
    (length(summary$multi_mapped_rvs) / length(summary$rvss)) * 100
  ))

  # Print ICD-10 Mapping Statistics
  cat(
    sprintf("\nThere are %d unique ICD-10 codes.\n", length(summary$unique_icds)),
    sprintf(
      "%d (%.2f%%) are directly in the Thai ICD-10 library.\n",
      length(summary$direct_matches), (length(summary$direct_matches) / length(summary$unique_icds)) * 100
    ),
    sprintf(
      "Total %d codes were mapped to the Thai ICD10 library.\n",
      length(summary$unique_icds) - length(summary$unmatched_codes)
    ),
    sprintf(
      "%d codes were modified to match.\n",
      length(summary$unique_icds) - length(summary$unmatched_codes) - length(summary$direct_matches)
    ),
    sprintf("%d codes could not be mapped.\n", length(summary$unmatched_codes))
  )

  # Print Modified ICD-10 Codes
  print_modified_icd10_codes <- function(summary) {
    # Extract the modified matches from the summary
    unique_icd10_map <- summary$modified_matches

    # Ensure the data is in a data.table format
    if (!is.data.table(unique_icd10_map)) {
      unique_icd10_map <- as.data.table(unique_icd10_map)
    }

    if (nrow(unique_icd10_map) > 0) {
      # Add a character difference column if not already present
      if (!"char_diff" %in% names(unique_icd10_map)) {
        unique_icd10_map[, char_diff := abs(nchar(modified_matches) - nchar(modified_match))]
      }

      # Order the data.table by char_diff in descending order
      unique_icd10_map <- unique_icd10_map[order(-char_diff)]

      # Print the table as markdown
      print(knitr::kable(
        unique_icd10_map,
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
  print_modified_icd10_codes(summary)

  if (length(summary$unmatched_codes) > 0) {
    # Convert the unmatched codes into a data.table with counts
    unmatched_codes <- data.table(
      code = summary$unmatched_codes
    )[, .(count = .N), by = code][order(-count)] # Aggregate by code and order by count

    # Print the final unmatched sources as a markdown table
    print(knitr::kable(unmatched_codes,
      format = "markdown",
      caption = "Invalid ICD-10 Codes"
    ))
    cat("\nnrow invalid ICD-10 codes: ", nrow(unmatched_codes), "\n")
  } else {
    cat("\nAll resulting ICD-10 codes are present in the Thai library.\n\n")
  }

  # Step 9: Print PDX Success
  cat("\nAll PDx's are in the list of acceptable PDx's:\n", summary$pdx_success, "\n")
}
