format_large_numbers <- function(x) {
  if (x >= 1e9) {
    return(sprintf("%.1fb", x / 1e9))
  } else if (x >= 1e6) {
    return(sprintf("%.1fm", x / 1e6))
  } else if (x >= 1e3) {
    return(sprintf("%.1fk", x / 1e3))
  } else {
    return(as.character(x))
  }
}

print_summary_tables <- function(result, rows_to_show) {
  dt <- result$dt
  consolidated_summary <- result$consolidated_summary

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
    cat(
      "Patient Type Unmapped:\n",
      consolidated_summary$pat_type_unmapped, "\n\n"
    )
  }

  if (is.null(consolidated_summary$memcat_parent_unmapped)) {
    cat("Memcat Parent Unmapped: NULL\n\n")
  } else {
    cat(
      "Memcat Parent Unmapped:\n",
      consolidated_summary$memcat_parent_unmapped, "\n\n"
    )
  }

  if (is.null(consolidated_summary$memcat_child_unmapped)) {
    cat("Memcat Child Unmapped: NULL\n\n")
  } else {
    cat(
      "Memcat Child Unmapped:\n",
      consolidated_summary$memcat_child_unmapped, "\n\n"
    )
  }

  if (is.null(consolidated_summary$discharge_unmapped)) {
    cat("Discharge Unmapped: NULL\n\n")
  } else {
    cat(
      "Discharge Unmapped:\n",
      consolidated_summary$discharge_unmapped, "\n\n"
    )
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
    print(kable(
      head(
        consolidated_summary$empty_strings_replaced_1, rows_to_show
      ),
      format = "markdown",
      caption = "Empty Strings Replaced (First Set)"
    ))
  } else {
    cat("\nNo empty strings replaced in the first set.\n\n")
  }

  if (nrow(consolidated_summary$empty_strings_replaced_2) > 0) {
    print(kable(
      head(
        consolidated_summary$empty_strings_replaced_2, rows_to_show
      ),
      format = "markdown",
      caption = "Empty Strings Replaced (Second Set)"
    ))
  } else {
    cat("\nNo empty strings replaced in the second set.\n\n")
  }
}

combine_comparison_tables <- function(
    summaries, comparison_field, rows_to_show = 10) {
  comparison_list <- lapply(summaries, function(summary) {
    summary_data <- summary[[comparison_field]]
    if (!is.null(summary_data) && nrow(summary_data) > 0) {
      summary_data <- summary_data[, .(old_code, new_code, count)]
    }
    return(summary_data)
  })

  combined_comparison <- rbindlist(comparison_list, fill = TRUE)

  if (nrow(combined_comparison) == 0) {
    return(data.table(
      old_code = character(),
      new_code = character(), count = integer()
    ))
  }

  combined_comparison <- combined_comparison[,
    .(count = sum(count, na.rm = TRUE)),
    by = .(old_code, new_code)
  ]
  combined_comparison <- combined_comparison[order(-count)]
  combined_comparison <- head(combined_comparison, rows_to_show)

  return(combined_comparison)
}

combine_discarded_rvs_tables <- function(summaries, field, rows_to_show = 10) {
  discarded_list <- lapply(summaries, function(summary) summary[[field]])
  combined_discarded <- rbindlist(discarded_list, fill = TRUE)

  if (nrow(combined_discarded) == 0) {
    return(data.table(CODE = character(), count = integer()))
  }

  combined_discarded <- combined_discarded[, .(count = sum(count)), by = CODE]
  combined_discarded <- combined_discarded[order(-count)]
  combined_discarded <- head(combined_discarded, rows_to_show)

  return(combined_discarded)
}

combine_replace_empty_tables <- function(summaries, field, rows_to_show = 10) {
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
  combined_replace_empty <- head(combined_replace_empty, rows_to_show)

  return(combined_replace_empty)
}

combine_and_print_summaries <- function() {
  final_combined_summary <- combine_all_parts_summaries(
    all_parts_summaries, rows_to_show
  )

  print_summary_tables(
    list(
      dt = NULL,
      consolidated_summary = final_combined_summary
    ),
    rows_to_show
  )
}

# Function to combine all parts summaries
combine_all_parts_summaries <- function(all_parts_summaries, rows_to_show = 10) {
  combined_summary <- list(
    rename_success = all(unlist(sapply(all_parts_summaries, function(summary) summary$rename_success)), na.rm = TRUE),
    ICD_replacements_1 = combine_comparison_tables(all_parts_summaries, "ICD_replacements_1", rows_to_show),
    ICD_replacements_2 = combine_comparison_tables(all_parts_summaries, "ICD_replacements_2", rows_to_show),
    pat_type_unmapped = unique(unlist(lapply(all_parts_summaries, function(summary) summary$pat_type_unmapped))),
    memcat_parent_unmapped = unique(unlist(lapply(all_parts_summaries, function(summary) summary$memcat_parent_unmapped))),
    memcat_child_unmapped = unique(unlist(lapply(all_parts_summaries, function(summary) summary$memcat_child_unmapped))),
    discharge_unmapped = unique(unlist(lapply(all_parts_summaries, function(summary) summary$discharge_unmapped))),
    discard_rvs_one = combine_discarded_rvs_tables(all_parts_summaries, "discard_rvs_one", rows_to_show),
    discard_rvs_two = combine_discarded_rvs_tables(all_parts_summaries, "discard_rvs_two", rows_to_show),
    empty_strings_replaced_1 = combine_replace_empty_tables(all_parts_summaries, "empty_strings_replaced_1", rows_to_show),
    empty_strings_replaced_2 = combine_replace_empty_tables(all_parts_summaries, "empty_strings_replaced_2", rows_to_show),
    rvs_mapping_summary = unique(rbindlist(lapply(all_parts_summaries, function(summary) summary$rvs_mapping_summary), fill = TRUE)),
    icd_mapping_summary = unique(rbindlist(lapply(all_parts_summaries, function(summary) summary$icd_mapping_summary), fill = TRUE))
  )

  return(combined_summary)
}

# Function to print combined statistics
print_combined_statistics <- function(final_combined_summary, rows_to_show) {
  rvs_stats <- final_combined_summary$rvs_mapping_summary[!is.na(icd9_list)]
  icd_stats <- final_combined_summary$icd_mapping_summary[!is.na(icd)]

  total_rvs_codes <- length(unique(rvs_stats$rvs))
  unique_rvs_with_mapping <- unique(rvs_stats[!is.na(icd9_list), rvs])
  rvs_with_icd_mapping <- length(unique_rvs_with_mapping)
  rvs_with_multiple_icd <- sum(sapply(unique_rvs_with_mapping, function(x) length(rvs_stats[rvs == x & !is.na(icd9_list)]$icd9_list) > 1))
  rvs_without_icd <- total_rvs_codes - rvs_with_icd_mapping

  total_icd_codes <- length(unique(icd_stats$icd))
  icd_direct_match <- sum(icd_stats$direct_match, na.rm = TRUE)
  icd_modified <- sum(icd_stats$modified, na.rm = TRUE)
  icd_not_mapped <- total_icd_codes - icd_direct_match - icd_modified

  cat(sprintf("There are %d unique RVS codes that appear in the claims.\n", total_rvs_codes))
  cat(sprintf("Of these, %d (%.2f %%) have a mapping to an ICD-9-CM code.\n", rvs_with_icd_mapping, (rvs_with_icd_mapping / total_rvs_codes) * 100))
  if (rvs_with_multiple_icd > 0) {
    cat(sprintf("Of these, there are %d (%.2f %%) with more than one ICD9 equivalent recognized by the Thai ICD9 library.\n", rvs_with_multiple_icd, (rvs_with_multiple_icd / total_rvs_codes) * 100))
  }
  if (rvs_without_icd > 0) {
    cat(sprintf("There are %d (%.2f %%) with no ICD-9-CM equivalents.\n", rvs_without_icd, (rvs_without_icd / total_rvs_codes) * 100))
  } else {
    cat("All RVS codes have an ICD-9-CM equivalent.\n")
  }

  cat(sprintf("\nThere are %d unique entries for ICD-10 codes, of which %d (%.2f %%) are directly in the Thai ICD-10 library\n", total_icd_codes, icd_direct_match, (icd_direct_match / total_icd_codes) * 100))
  cat(sprintf("The modifications led to a total of %d codes being mapped to an equivalent in the Thai ICD10 library.\n", icd_direct_match + icd_modified))
  if (icd_modified > 0) {
    cat(sprintf("Out of these, %d were modified to match.\n", icd_modified))
  }
  if (icd_not_mapped > 0) {
    cat(sprintf("There are %d codes that could not be mapped to the Thai ICD10 library.\n", icd_not_mapped))
  } else {
    cat("All codes were successfully mapped to the Thai ICD10 library.\n")
  }

  # Print codes that could not be mapped to the Thai ICD10 library
  not_mapped_codes <- icd_stats[!(direct_match | modified), .(icd)]
  not_mapped_codes <- not_mapped_codes[!is.na(icd)] # Filter out NA values

  if (nrow(not_mapped_codes) > 0) {
    cat("\nCodes that could not be mapped to the Thai ICD10 library:\n")
    print(kable(head(not_mapped_codes, rows_to_show), format = "markdown"))
  }
}
