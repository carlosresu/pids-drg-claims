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

  print(paste("Total Unique ICDs", consolidated_summary$unique_icds_count))
  print(paste("Total Unique Direct Matches", consolidated_summary$direct_matches_count))
  print(paste("Total Unique Unmapped", consolidated_summary$unmatched_count))
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

combine_and_print_summaries <- function(all_parts_summaries, rows_to_show) {
  combined_summary <- combine_all_parts_summaries(
    all_parts_summaries, rows_to_show
  )
  print_summary_tables(
    list(
      dt = NULL,
      consolidated_summary = combined_summary
    ),
    rows_to_show
  )
}

combine_chunk_summaries <- function(parallel_results, rows_to_show) {
  summaries <- lapply(parallel_results, function(res) res$summary)
  combined_summary <- combine_summaries(summaries, rows_to_show)
  return(combined_summary)
}

combine_all_parts_summaries <- function(all_parts_summaries, rows_to_show) {
  combined_summary <- combine_summaries(all_parts_summaries, rows_to_show)
  return(combined_summary)
}

combine_summaries <- function(summaries, rows_to_show) {
  combined_summary <- list(
    rename_success = all(unlist(sapply(summaries, function(summary) summary$rename_success)), na.rm = TRUE),
    ICD_replacements_1 = combine_comparison_tables(summaries, "ICD_replacements_1", rows_to_show),
    ICD_replacements_2 = combine_comparison_tables(summaries, "ICD_replacements_2", rows_to_show),
    pat_type_unmapped = unique(unlist(lapply(summaries, function(summary) summary$pat_type_unmapped))),
    memcat_parent_unmapped = unique(unlist(lapply(summaries, function(summary) summary$memcat_parent_unmapped))),
    memcat_child_unmapped = unique(unlist(lapply(summaries, function(summary) summary$memcat_child_unmapped))),
    discharge_unmapped = unique(unlist(lapply(summaries, function(summary) summary$discharge_unmapped))),
    discard_rvs_one = combine_discarded_rvs_tables(summaries, "discard_rvs_one", rows_to_show),
    discard_rvs_two = combine_discarded_rvs_tables(summaries, "discard_rvs_two", rows_to_show),
    empty_strings_replaced_1 = combine_replace_empty_tables(summaries, "empty_strings_replaced_1", rows_to_show),
    empty_strings_replaced_2 = combine_replace_empty_tables(summaries, "empty_strings_replaced_2", rows_to_show),
    unique_icds_count = 0, # Initialize counts to be updated later
    direct_matches_count = 0, # Initialize counts to be updated later
    unmatched_count = 0 # Initialize counts to be updated later
  )

  combined_unique_icds <- unique(unlist(lapply(summaries, function(summary) summary$unique_icds)))
  combined_direct_matches <- unique(unlist(lapply(summaries, function(summary) summary$direct_matches)))
  combined_unmatched <- unique(unlist(lapply(summaries, function(summary) summary$unmatched)))

  combined_summary$unique_icds_count <- length(unlist(combined_unique_icds))
  combined_summary$direct_matches_count <- length(unlist(combined_direct_matches))
  combined_summary$unmatched_count <- length(unlist(combined_unmatched))

  return(combined_summary)
}
