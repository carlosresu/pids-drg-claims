print_summary_tables <- function(final_combined_summaries, end_nrow) {
  #' @title Print Summary Tables
  #'
  #' @description This function prints summary tables for a given dataset.
  #'
  #' @param final_combined_summaries list. The final combined
  #' summaries to be printed.
  #' @param end_nrow integer. The number of rows to show
  #' in the summary tables.
  #'
  #' @return NULL. Prints the summary tables.

  summary <- final_combined_summaries

  cat("\n\nRename Success:\n", summary$final_rename_success, "\n\n")

  if (nrow(summary$final_ICD_replacements_1) > 0) {
    print(kable(head(summary$final_ICD_replacements_1, end_nrow),
      format = "markdown",
      caption = "ICD Text Normalization for clin_c1 Before Splitting"
    ))
  } else {
    cat(
      sprintf("\nNo ICD replacements found in clin_c1 with more than %d different characters.", diff_chars),
      "\nNote that commas, asterisks, plus signs, and whitespaces are ignored.\n"
    )
  }


  if (nrow(summary$final_ICD_replacements_2) > 0) {
    print(kable(head(summary$final_ICD_replacements_2, end_nrow),
      format = "markdown",
      caption = "ICD Text Normalization for clin_c2 Before Splitting"
    ))
  } else {
    cat(
      sprintf("\nNo ICD replacements found in clin_c2 with more than %d different characters.", diff_chars),
      "\nNote that commas, asterisks, plus signs, and whitespaces are ignored.\n"
    )
  }

  # Display unique before and after mappings for each categorical variable
  display_unique_mappings <- function(mapped_data, mapping_name, tmp_nrow) {
    #' @title Display Unique Mappings
    #'
    #' @description Displays the unique before-and-after mappings for a given dataset.
    #'
    #' @param mapped_data data.table. The data table with Original and Mapped columns.
    #' @param mapping_name character. The name of the mapping being displayed.
    #' @param tmp_nrow integer. Number of rows to display in the output.
    #'
    #' @return NULL. Prints the unique mappings.

    # Ensure the data has the correct columns
    if (!("Original" %in% names(mapped_data)) || !("Mapped" %in% names(mapped_data))) {
      stop("The data table must contain 'Original' and 'Mapped' columns.")
    }

    # Create a data table to display unique before and after mappings
    unique_mappings <- unique(mapped_data)

    # Print the mappings using kable
    print(kable(head(unique_mappings, tmp_nrow),
      format = "markdown",
      caption = sprintf("Unique Before and After Mappings for %s", mapping_name)
    ))
  }

  display_unique_mappings(summary$final_pat_type_mapped, "Patient Type", tmp_nrow)
  display_unique_mappings(summary$final_memcat_parent_mapped, "Memcat Parent", tmp_nrow)
  display_unique_mappings(summary$final_memcat_child_mapped, "Memcat Child", tmp_nrow)
  display_unique_mappings(summary$final_clin_discharge_mapped, "Discharge", tmp_nrow)
  display_unique_mappings(summary$final_claim_status_mapped, "Claim Status", tmp_nrow)

  if (nrow(summary$final_discard_rvs_one) > 0) {
    print(kable(head(summary$final_discard_rvs_one, end_nrow),
      format = "markdown",
      caption = "Discarded RVS Codes One"
    ))
  } else {
    cat("\nNo RVS codes discarded in the first set.\n\n")
  }

  if (nrow(summary$final_discard_rvs_two) > 0) {
    print(kable(head(summary$final_discard_rvs_two, end_nrow),
      format = "markdown",
      caption = "Discarded RVS Codes Two"
    ))
  } else {
    cat("\nNo RVS codes discarded in the second set.\n\n")
  }

  if (nrow(summary$final_empty_strings_replaced_0) > 0) {
    print(kable(
      head(
        summary$final_empty_strings_replaced_0, end_nrow
      ),
      format = "markdown",
      caption = "Empty Strings Replaced (Zeroth Set)"
    ))
  } else {
    cat("\nNo empty strings replaced in the zeroth set.\n\n")
  }

  if (nrow(summary$final_empty_strings_replaced_1) > 0) {
    print(kable(
      head(
        summary$final_empty_strings_replaced_1, end_nrow
      ),
      format = "markdown",
      caption = "Empty Strings Replaced (First Set)"
    ))
  } else {
    cat("\nNo empty strings replaced in the first set.\n\n")
  }

  if (nrow(summary$final_empty_strings_replaced_2) > 0) {
    print(kable(
      head(
        summary$final_empty_strings_replaced_2, end_nrow
      ),
      format = "markdown",
      caption = "Empty Strings Replaced (Second Set)"
    ))
  } else {
    cat("\nNo empty strings replaced in the second set.\n\n")
  }

  cat(
    sprintf(
      "There are %d RVS codes without an ICD-9CM",
      summary$final_without_drg
    ), "equivalent recognized by the TDRG ICD9CM\n"
  )
  cat(
    sprintf(
      "There are %d unique RVS codes that appear in the claims.\n",
      summary$final_rvss
    )
  )
  cat(
    sprintf(
      "Of these, %d (%.2f%%) have a mapping to an ICD-9-CM code.\n",
      summary$final_mappable_rvs,
      (summary$final_mappable_rvs /
        summary$final_rvss) * 100
    )
  )
  cat(
    sprintf(
      "Of these, there are %d (%.2f%%)",
      summary$final_multi_mapped_rvs,
      (summary$final_multi_mapped_rvs /
        summary$final_rvss) * 100
    ), "with more than one ICD9 equivalent",
    "recognized by the Thai ICD9 library.\n"
  )

  cat(
    sprintf(
      "\n\nThere are %d unique entries for ICD-10 codes, of which %d (%.2f%%)",
      summary$final_unique_icds,
      summary$final_direct_matches,
      (summary$final_direct_matches /
        summary$final_unique_icds) * 100
    ), "are directly in the Thai ICD-10 library.\n"
  )

  cat(
    sprintf(
      "The modifications led to a total of %d codes",
      summary$final_unique_icds -
        summary$final_unmatched
    ), "being mapped to an equivalent in the Thai ICD10 library.\n"
  )

  cat(
    sprintf(
      "Out of these, %d were modified to match.\n",
      summary$final_unique_icds -
        summary$final_unmatched -
        summary$final_direct_matches
    )
  )

  cat(
    sprintf(
      "There are %d codes that could not",
      summary$final_unmatched
    ), "be mapped to the Thai ICD10 library.\n"
  )

  unique_icd10_map <- process_final_icd10_map(summary$final_icd10_map_dt, tmp_nrow)

  # Count the number of rows with phl_icd10 length > 5
  num_long_phl_icd10 <- sum(nchar(unique_icd10_map$phl_icd10) > 5)

  # Print the count
  cat(
    "\nNumber of rows with phl_icd10 length greater than 5:",
    num_long_phl_icd10, "of", nrow(unique_icd10_map), "rows"
  )

  # Check if there are any rows and print the table
  if (nrow(unique_icd10_map) > 0) {
    print(
      kable(
        head(
          unique_icd10_map,
          end_nrow
        ),
        format = "markdown",
        caption = "Modified ICD-10 codes ordered by descending NChar distance"
      )
    )
  } else {
    cat("\nNo modified ICD-10 codes found.\n\n")
  }

  if (nrow(summary$final_unmatched_sources) > 0) {
    print(
      kable(
        head(
          summary$final_unmatched_sources,
          end_nrow
        ),
        format = "markdown",
        caption = "Invalid ICD-10 Codes Not Found in Thai Library"
      )
    )
  } else {
    cat("\nAll resulting ICD-10 codes are present in the Thai library.\n\n")
  }

  cat(
    "\n\nAll PDx's are in list of acceptable PDx's:\n",
    summary$final_rename_success, "\n\n"
  )
}

# combine_comparison_tables <- function(
#     summaries, comparison_field, tmp_nrow, diff_chars) {
#   #' @title Combine Comparison Tables
#   #'
#   #' @description This function combines comparison tables from
#   #' multiple summaries into one, and ranks rows by a custom fuzzy match score
#   #' that prioritizes letter differences in ICD codes,
#   #' ignoring '+', '*', ',', '.', and spaces.
#   #'
#   #' @param summaries list. A list of summary tables.
#   #' @param comparison_field character. The field in the summaries to compare.
#   #' @param tmp_nrow integer. The number of
#   #' rows to show in the intermediate summary.
#   #' @param diff_chars numeric. The minimum fuzzy match score to filter.
#   #'
#   #' @return data.table. The combined comparison table.

#   # Helper function to clean strings by removing specified characters
#   clean_string <- function(strings) {
#     # Remove '+', '*', ',', '.', and spaces
#     cleaned_strings <- gsub("[+*,.\\s]", "", strings)
#     return(cleaned_strings)
#   }

#   # Process each summary to extract comparison data
#   comparison_list <- lapply(summaries, function(summary) {
#     summary_data <- summary[[comparison_field]]
#     if (!is.null(summary_data) && nrow(summary_data) > 0) {
#       summary_data <- summary_data[, .(old_code, new_code, count)]
#     }
#     return(summary_data)
#   })

#   # Combine all comparison data into a single data.table
#   combined_comparison <- rbindlist(comparison_list, fill = TRUE)

#   if (nrow(combined_comparison) == 0) {
#     return(data.table(
#       old_code = character(),
#       new_code = character(),
#       count = integer(),
#       differing_chars = numeric()
#     ))
#   }

#   # Clean both old_code and new_code columns
#   combined_comparison[, `:=`(
#     clean_old = clean_string(old_code),
#     clean_new = clean_string(new_code)
#   )]

#   # Calculate the difference in number of characters between clean_old and clean_new
#   combined_comparison[, differing_chars := abs(nchar(clean_old) - nchar(clean_new))]

#   # Filter rows based on diff_chars
#   combined_comparison <- combined_comparison[differing_chars > diff_chars]

#   if (nrow(combined_comparison) == 0) {
#     return(data.table(
#       old_code = character(),
#       new_code = character(),
#       count = integer(),
#       differing_chars = numeric()
#     ))
#   }

#   # Sum counts, sort by differing_chars, and order by descending count
#   combined_comparison <- combined_comparison[,
#     .(count = sum(count, na.rm = TRUE), differing_chars = max(differing_chars, na.rm = TRUE)),
#     by = .(old_code, new_code)
#   ][order(-differing_chars, -count)]

#   # Select the top rows based on tmp_nrow
#   combined_comparison <- head(combined_comparison, tmp_nrow)

#   return(combined_comparison)
# }

combine_comparison_tables <- function(
    summaries, comparison_field, tmp_nrow = 10) {
  #' @title Combine Comparison Tables
  #'
  #' @description This function combines comparison tables from
  #' multiple summaries into one.
  #'
  #' @param summaries list. A list of summary tables.
  #' @param comparison_field character. The field in the summaries to compare.
  #' @param tmp_nrow integer. The number of
  #' rows to show in the intermediate summary.
  #'
  #' @return data.table. The combined comparison table with absolute differences in character lengths.

  comparison_list <- lapply(summaries, function(summary) {
    summary_data <- summary[[comparison_field]]
    if (!is.null(summary_data) && nrow(summary_data) > 0) {
      summary_data <- summary_data[, .(old_code, new_code)]
    }
    return(summary_data)
  })

  # Combine all the data
  combined_comparison <- rbindlist(comparison_list, fill = TRUE)

  if (nrow(combined_comparison) == 0) {
    return(data.table(
      old_code = character(),
      new_code = character(),
      diff_chars = integer()
    ))
  }

  # Trim whitespaces and calculate the absolute difference in character lengths for unique pairs
  combined_comparison[, `:=`(
    old_code = gsub("\\s", "", old_code),
    new_code = gsub("\\s", "", new_code)
  )]
  combined_comparison[, diff_chars := abs(nchar(old_code) - nchar(new_code))]

  # Keep only unique old_code to new_code combinations
  unique_combinations <- unique(combined_comparison)

  # Order by the absolute character difference and limit the number of rows
  unique_combinations <- unique_combinations[order(-diff_chars)]
  unique_combinations <- head(unique_combinations, tmp_nrow)

  return(unique_combinations)
}


# Function to filter and sort the final ICD-10 map
process_final_icd10_map <- function(icd10_map_dt, tmp_nrow = 10) {
  #' @title Process Final ICD-10 Map
  #'
  #' @description This function processes the final ICD-10 map data table
  #' by filtering out rows where the original and mapped codes are the same
  #' and sorts the result by descending absolute difference in the number
  #' of characters between the codes.
  #'
  #' @param icd10_map_dt data.table. The ICD-10 map data table.
  #' @param tmp_nrow integer. The number of rows to show in the final summary.
  #'
  #' @return data.table. The processed and sorted ICD-10 map.

  # Filter rows where phl_icd10 and tdrg_icd10 are different
  icd10_map_dt <- icd10_map_dt[phl_icd10 != tdrg_icd10]

  if (nrow(icd10_map_dt) == 0) {
    return(data.table(
      phl_icd10 = character(),
      tdrg_icd10 = character(),
      char_diff = numeric()
    ))
  }

  # Calculate the absolute difference in character length and add char_diff column
  icd10_map_dt[, char_diff := abs(nchar(phl_icd10) - nchar(tdrg_icd10))]

  # Sort by descending absolute difference in character length
  icd10_map_dt <- icd10_map_dt[order(-char_diff)]

  # Select the top rows based on tmp_nrow
  final_icd10_map <- head(icd10_map_dt, tmp_nrow)

  return(final_icd10_map)
}

combine_discarded_rvs_tables <- function(
    summaries, field, tmp_nrow = 10) {
  #' @title Combine Discarded RVS Tables
  #'
  #' @description This function combines discarded RVS tables
  #' from multiple summaries into one.
  #'
  #' @param summaries list. A list of summary tables.
  #' @param field character. The field in the summaries that
  #' contains discarded RVS codes.
  #' @param tmp_nrow integer. The number of
  #' rows to show in the intermediate summary.
  #'
  #' @return data.table. The combined discarded RVS table.

  discarded_list <- lapply(summaries, function(summary) summary[[field]])
  combined_discarded <- rbindlist(discarded_list, fill = TRUE)

  if (nrow(combined_discarded) == 0) {
    return(data.table(CODE = character(), count = integer()))
  }

  combined_discarded <- combined_discarded[, .(count = sum(count)), by = CODE]
  combined_discarded <- combined_discarded[order(-count)]
  combined_discarded <- head(combined_discarded, tmp_nrow)

  return(combined_discarded)
}

combine_unmatched_icd10_codes <- function(
    summaries, field, tmp_nrow = 10) {
  #' @title Combine Unmatched ICD-10 Codes
  #'
  #' @description This function combines unmatched ICD-10 codes
  #' from multiple summaries into one.
  #'
  #' @param summaries list. A list of summary tables.
  #' @param field character. The field in the summaries that
  #' contains unmatched ICD-10 codes.
  #' @param tmp_nrow integer. The number of
  #' rows to show in the intermediate summary.
  #'
  #' @return data.table. The combined unmatched ICD-10 codes table.

  combined_list <- lapply(summaries, function(summary) summary[[field]])
  combined_table <- rbindlist(combined_list, fill = TRUE)
  combined_table <- combined_table[, .(count = sum(count)),
    by = .(code, source)
  ]
  combined_table <- combined_table[order(-count)]
  return(combined_table)
}

combine_replace_empty_tables <- function(
    summaries, field, tmp_nrow = 10) {
  #' @title Combine Replace Empty Tables
  #'
  #' @description This function combines tables for replaced
  #' empty values from multiple summaries into one.
  #'
  #' @param summaries list. A list of summary tables.
  #' @param field character. The field in the summaries that
  #' contains information on replaced empty values.
  #' @param tmp_nrow integer. The number of
  #' rows to show in the intermediate summary.
  #'
  #' @return data.table. The combined replace empty tables.

  replace_empty_list <- lapply(summaries, function(summary) summary[[field]])
  combined_replace_empty <- rbindlist(replace_empty_list, fill = TRUE)

  if (nrow(combined_replace_empty) == 0) {
    return(data.table(
      Column = character(),
      Empty_Replaced = integer(),
      NA_Replaced = integer(),
      Character0_Replaced = integer()
    ))
  }

  combined_replace_empty <- combined_replace_empty[, .(
    Empty_Replaced = sum(Empty_Replaced, na.rm = TRUE),
    NA_Replaced = sum(NA_Replaced, na.rm = TRUE),
    Character0_Replaced = sum(Character0_Replaced, na.rm = TRUE)
  ), by = Column]
  combined_replace_empty <- combined_replace_empty[
    order(-Empty_Replaced, -NA_Replaced, -Character0_Replaced)
  ]
  combined_replace_empty <- head(
    combined_replace_empty,
    tmp_nrow
  )

  return(combined_replace_empty)
}

combine_chunk_summaries <- function(
    summaries, tmp_nrow) {
  #' @title Combine Chunk Summaries
  #'
  #' @description This function combines summaries from
  #' multiple chunks into one summary.
  #'
  #' @param summaries list. A list of results from
  #' summaries from parallel processing.
  #' @param tmp_nrow integer. The number
  #' of rows to show in the intermediate summary.
  #'
  #' @return list. The combined summary.

  combined_summary <- combine_summaries(summaries, tmp_nrow)
  return(combined_summary)
}

combine_summaries <- function(summaries, tmp_nrow) {
  #' @title Combine Summaries
  #'
  #' @description This function combines multiple summaries into one summary.
  #'
  #' @param summaries list. A list of summary tables.
  #' @param tmp_nrow integer.
  #' The number of rows to show in the intermediate summary.
  #'
  #' @return list. The combined summary.

  combined_summary <- list(
    rename_success = all(unlist(sapply(
      summaries,
      function(summary) summary$rename_success
    )), na.rm = TRUE),
    ICD_replacements_1 = combine_comparison_tables(
      summaries, "ICD_replacements_1", tmp_nrow
    ),
    ICD_replacements_2 = combine_comparison_tables(
      summaries, "ICD_replacements_2", tmp_nrow
    ),
    pat_type_unmapped = unique(unlist(lapply(
      summaries,
      function(summary) summary$pat_type_unmapped
    ))),
    memcat_parent_unmapped = unique(unlist(lapply(
      summaries,
      function(summary) summary$memcat_parent_unmapped
    ))),
    memcat_child_unmapped = unique(unlist(lapply(
      summaries,
      function(summary) summary$memcat_child_unmapped
    ))),
    discharge_unmapped = unique(unlist(lapply(
      summaries,
      function(summary) summary$discharge_unmapped
    ))),
    claim_status_unmapped = unique(unlist(lapply(
      summaries,
      function(summary) summary$claim_status_unmapped
    ))),
    discard_rvs_one = combine_discarded_rvs_tables(
      summaries, "discard_rvs_one", tmp_nrow
    ),
    discard_rvs_two = combine_discarded_rvs_tables(
      summaries, "discard_rvs_two", tmp_nrow
    ),
    empty_strings_replaced_1 = combine_replace_empty_tables(
      summaries, "empty_strings_replaced_1", tmp_nrow
    ),
    empty_strings_replaced_2 = combine_replace_empty_tables(
      summaries, "empty_strings_replaced_2", tmp_nrow
    ),
    unique_icds_count = unique(unlist(lapply(
      summaries,
      function(summary) summary$unique_icds
    ))),
    direct_matches_count = unique(unlist(lapply(
      summaries,
      function(summary) summary$direct_matches
    ))),
    unmatched_count = unique(unlist(lapply(
      summaries,
      function(summary) summary$unmatched
    ))),
    unmatched_sources = combine_unmatched_icd10_codes(
      summaries, "unmatched_sources", tmp_nrow
    ),
    rvss = unique(na.omit(unlist(lapply(
      summaries,
      function(summary) summary$rvss
    )))),
    mappable_rvs = unique(na.omit(unlist(lapply(
      summaries,
      function(summary) summary$mappable_rvs
    )))),
    unmappable_rvs = unique(na.omit(unlist(lapply(
      summaries,
      function(summary) summary$unmappable_rvs
    )))),
    multi_mapped_rvs = unique(na.omit(unlist(lapply(
      summaries,
      function(summary) summary$multi_mapped_rvs
    )))),
    without_drg = unique(na.omit(unlist(lapply(
      summaries,
      function(summary) summary$without_drg
    )))),
    pdx_success = all(unlist(sapply(
      summaries,
      function(summary) summary$pdx_success
    )), na.rm = TRUE),
    icd10_map_dt = rbindlist(lapply(
      summaries,
      function(summary) summary$icd10_map_dt
    )),
    pat_type_mapped = rbindlist(lapply(
      summaries,
      function(summary) summary$pat_type_mapped
    )),
    pat_memcat_parent_mapped = rbindlist(lapply(
      summaries,
      function(summary) summary$pat_memcat_parent_mapped
    )),
    pat_memcat_child_mapped = rbindlist(lapply(
      summaries,
      function(summary) summary$pat_memcat_child_mapped
    )),
    clin_discharge_mapped = rbindlist(lapply(
      summaries,
      function(summary) summary$clin_discharge_mapped
    )),
    claim_status_mapped = rbindlist(lapply(
      summaries,
      function(summary) summary$claim_status_mapped
    ))
  )

  return(combined_summary)
}

combine_parts_summaries <- function(combined_summary, end_nrow) {
  #' @title Combine Parts Summaries
  #'
  #' @description This function combines summaries from multiple
  #' parts into one final summary.
  #'
  #' @param combined_summary list. A list of combined summaries.
  #' @param end_nrow integer. The number of rows to show in
  #' the final summary.
  #'
  #' @return list. The final combined summary.

  final_combined_summaries <- list(
    final_rename_success = all(unlist(sapply(
      combined_summary,
      function(summary) summary$rename_success
    )), na.rm = TRUE),
    final_ICD_replacements_1 = combine_comparison_tables(
      combined_summary, "ICD_replacements_1", end_nrow
    ),
    final_ICD_replacements_2 = combine_comparison_tables(
      combined_summary, "ICD_replacements_2", end_nrow
    ),
    final_pat_type_unmapped = unique(unlist(lapply(
      combined_summary,
      function(summary) summary$pat_type_unmapped
    ))),
    final_memcat_parent_unmapped = unique(unlist(lapply(
      combined_summary,
      function(summary) summary$memcat_parent_unmapped
    ))),
    final_memcat_child_unmapped = unique(unlist(lapply(
      combined_summary,
      function(summary) summary$memcat_child_unmapped
    ))),
    final_discharge_unmapped = unique(unlist(lapply(
      combined_summary,
      function(summary) summary$discharge_unmapped
    ))),
    final_claim_status_unmapped = unique(unlist(lapply(
      combined_summary,
      function(summary) summary$claim_status_unmapped
    ))),
    final_discard_rvs_one = combine_discarded_rvs_tables(
      combined_summary, "discard_rvs_one", end_nrow
    ),
    final_discard_rvs_two = combine_discarded_rvs_tables(
      combined_summary, "discard_rvs_two", end_nrow
    ),
    final_empty_strings_replaced_0 = combine_replace_empty_tables(
      combined_summary, "replacement_summary", end_nrow
    ),
    final_empty_strings_replaced_1 = combine_replace_empty_tables(
      combined_summary, "empty_strings_replaced_1", end_nrow
    ),
    final_empty_strings_replaced_2 = combine_replace_empty_tables(
      combined_summary, "empty_strings_replaced_2", end_nrow
    ),
    final_unique_icds = length(unique(unlist(lapply(
      combined_summary,
      function(summary) summary$unique_icds_count
    )))),
    final_direct_matches = length(unique(unlist(lapply(
      combined_summary,
      function(summary) summary$direct_matches_count
    )))),
    final_unmatched = length(unique(unlist(lapply(
      combined_summary,
      function(summary) summary$unmatched_count
    )))),
    final_unmatched_sources = combine_unmatched_icd10_codes(
      combined_summary, "unmatched_sources", end_nrow
    ),
    final_rvss = length(unique(na.omit(unlist(lapply(
      combined_summary,
      function(summary) summary$rvss
    ))))),
    final_mappable_rvs = length(unique(na.omit(unlist(lapply(
      combined_summary,
      function(summary) summary$mappable_rvs
    ))))),
    final_unmappable_rvs = length(unique(na.omit(unlist(lapply(
      combined_summary,
      function(summary) summary$unmappable_rvs
    ))))),
    final_multi_mapped_rvs = length(unique(na.omit(unlist(lapply(
      combined_summary,
      function(summary) summary$multi_mapped_rvs
    ))))),
    final_without_drg = length(unique(na.omit(unlist(lapply(
      combined_summary,
      function(summary) summary$without_drg
    ))))),
    final_pdx_success = all(unlist(sapply(
      combined_summary,
      function(summary) summary$pdx_success
    )), na.rm = TRUE),
    final_icd10_map_dt = unique(rbindlist(lapply(
      combined_summary,
      function(summary) summary$icd10_map_dt
    ))),
    final_pat_type_mapped = unique(rbindlist(lapply(
      combined_summary,
      function(summary) summary$pat_type_mapped
    ))),
    final_memcat_parent_mapped = unique(rbindlist(lapply(
      combined_summary,
      function(summary) summary$pat_memcat_parent_mapped
    ))),
    final_memcat_child_mapped = unique(rbindlist(lapply(
      combined_summary,
      function(summary) summary$pat_memcat_child_mapped
    ))),
    final_clin_discharge_mapped = unique(rbindlist(lapply(
      combined_summary,
      function(summary) summary$clin_discharge_mapped
    ))),
    final_claim_status_mapped = unique(rbindlist(lapply(
      combined_summary,
      function(summary) summary$claim_status_mapped
    )))
  )

  return(final_combined_summaries)
}
