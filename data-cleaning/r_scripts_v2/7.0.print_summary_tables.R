print_summary_tables <- function(final_combined_summaries) {
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
    cat(
      sprintf(
        "\nNo ICD replacements found in clin c1 & c2 with diff chars > %d",
        diff_chars
      ),
      "\nNote: commas, asterisks, plus signs, and whitespaces are ignored.\n"
    )
  }

  display_unique_mappings(
    summary$final_pat_type_mapped, "Patient Type"
  )
  display_unique_mappings(
    summary$final_memcat_parent_mapped, "Memcat Parent"
  )
  display_unique_mappings(
    summary$final_memcat_child_mapped, "Memcat Child"
  )
  display_unique_mappings(
    summary$final_clin_discharge_mapped, "Discharge"
  )
  display_unique_mappings(
    summary$final_claim_status_mapped, "Claim Status"
  )

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

  if (nrow(summary$final_empty_strings_replaced_0) > 0) {
    print(kable(
      summary$final_empty_strings_replaced_0,
      format = "markdown",
      caption = "Empty Strings Replaced (Zeroth Set)"
    ))
  } else {
    cat("\nNo empty strings replaced in the zeroth set.\n\n")
  }

  if (nrow(summary$final_empty_strings_replaced_1) > 0) {
    print(kable(
      summary$final_empty_strings_replaced_1,
      format = "markdown",
      caption = "Empty Strings Replaced (First Set)"
    ))
  } else {
    cat("\nNo empty strings replaced in the first set.\n\n")
  }

  if (nrow(summary$final_empty_strings_replaced_2) > 0) {
    print(kable(
      summary$final_empty_strings_replaced_2,
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

  unique_icd10_map <- process_final_icd10_map(summary$final_icd10_map_dt)

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
        unique_icd10_map,
        format = "markdown",
        caption = "Modified ICD-10 codes ordered by descending NChar distance"
      )
    )
  } else {
    cat("\nNo modified ICD-10 codes found.\n\n")
  }

  icd10_mapping_result <- map_icd10(
    master_dt$c1, master_dt$c2, master_dt$clin_icd
  )

  icd_mapping <- icd10_mapping_result$icd_mapping_res

  check_unmatched_icd_codes(master_dt, icd_mapping)

  cat(
    "\nAll PDx's are in list of acceptable PDx's:\n",
    summary$final_rename_success, ""
  )
}
