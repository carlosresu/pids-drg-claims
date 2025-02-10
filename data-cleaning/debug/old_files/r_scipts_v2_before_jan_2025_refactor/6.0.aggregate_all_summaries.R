# Recursive function to aggregate summaries across all levels
aggregate_all_summaries <- function(summaries) {
  combine_summaries <- function(summaries) {
    # Aggregating all required fields
    combined_summary <- list(
      rename_success = all(safe_unlist(lapply(summaries, function(s) safe_access(s, "rename_success")))),
      ICD_replacements_1 = rbindlist(lapply(summaries, function(s) safe_access(s, "ICD_replacements_1")), fill = TRUE),
      ICD_replacements_2 = rbindlist(lapply(summaries, function(s) safe_access(s, "ICD_replacements_2")), fill = TRUE),
      pat_type_unmapped = safe_unlist(lapply(summaries, function(s) safe_access(s, "pat_type_unmapped"))),
      memcat_parent_unmapped = safe_unlist(lapply(summaries, function(s) safe_access(s, "memcat_parent_unmapped"))),
      memcat_child_unmapped = safe_unlist(lapply(summaries, function(s) safe_access(s, "memcat_child_unmapped"))),
      discharge_unmapped = safe_unlist(lapply(summaries, function(s) safe_access(s, "discharge_unmapped"))),
      claim_status_unmapped = safe_unlist(lapply(summaries, function(s) safe_access(s, "claim_status_unmapped"))),
      discard_rvs_one = rbindlist(lapply(summaries, function(s) safe_access(s, "discard_rvs_one")), fill = TRUE),
      discard_rvs_two = rbindlist(lapply(summaries, function(s) safe_access(s, "discard_rvs_two")), fill = TRUE),
      replacement_summary = combine_replacement_tables(rbindlist(lapply(summaries, function(s) safe_extract(s, "replacement_summary")), fill = TRUE), "NA_character_"),
      empty_replaced_with_na_1 = combine_replacement_tables(rbindlist(lapply(summaries, function(s) safe_extract(s, "empty_replaced_with_na_1")), fill = TRUE), "NA_character_"),
      NA_replaced_with_empty_1 = combine_replacement_tables(rbindlist(lapply(summaries, function(s) safe_extract(s, "NA_replaced_with_empty_1")), fill = TRUE), "character(0)"),
      NA_replaced_with_empty_2 = combine_replacement_tables(rbindlist(lapply(summaries, function(s) safe_extract(s, "NA_replaced_with_empty_2")), fill = TRUE), "character(0)"),
      NA_replaced_with_empty_3 = combine_replacement_tables(rbindlist(lapply(summaries, function(s) safe_extract(s, "NA_replaced_with_empty_3")), fill = TRUE), "character(0)"),
      # aggregated_unique_icds_for_checks = unique(safe_unlist(lapply(summaries, function(s) safe_access(s, "returned_unique_icds_for_checks")))),
      # direct_matches = unique(safe_unlist(lapply(summaries, function(s) safe_access(s, "direct_matches")))),
      # modified_matches = extract_modified_matches(summaries),
      # aggregated_unmatched_codes_for_checks = unique(safe_unlist(lapply(summaries, function(s) safe_access(s, "returned_unmatched_codes_for_checks")))),
      # aggregated_unmatched_sources_for_checks = rbindlist(lapply(summaries, function(s) safe_access(s, "returned_unmatched_sources_for_checks")), fill = TRUE),
      # icd10_map_dt_for_checks = rbindlist(lapply(summaries, function(s) safe_access(s, "icd10_map_dt_for_checks")), fill = TRUE),
      rvss = unique(safe_unlist(lapply(summaries, function(s) safe_access(s, "rvss")))),
      mappable_rvs = unique(safe_unlist(lapply(summaries, function(s) safe_access(s, "mappable_rvs")))),
      unmappable_rvs = unique(safe_unlist(lapply(summaries, function(s) safe_access(s, "unmappable_rvs")))),
      multi_mapped_rvs = unique(safe_unlist(lapply(summaries, function(s) safe_access(s, "multi_mapped_rvs")))),
      without_drg = unique(safe_unlist(lapply(summaries, function(s) safe_access(s, "without_drg")))),
      pdx_success = all(safe_unlist(lapply(summaries, function(s) safe_access(s, "pdx_success")))),
      pat_type_mapped = rbindlist(lapply(summaries, function(s) safe_access(s, "pat_type_mapped")), fill = TRUE),
      pat_memcat_parent_mapped = rbindlist(lapply(summaries, function(s) safe_access(s, "pat_memcat_parent_mapped")), fill = TRUE),
      pat_memcat_child_mapped = rbindlist(lapply(summaries, function(s) safe_access(s, "pat_memcat_child_mapped")), fill = TRUE),
      clin_discharge_mapped = rbindlist(lapply(summaries, function(s) safe_access(s, "clin_discharge_mapped")), fill = TRUE),
      claim_status_mapped = rbindlist(lapply(summaries, function(s) safe_access(s, "claim_status_mapped")), fill = TRUE)
    )

    return(combined_summary)
  }
  # If summaries are nested lists (i.e., chunks within parts), aggregate them first
  if (all(sapply(summaries, is.list))) {
    summaries <- lapply(summaries, function(part) {
      combine_summaries(part) # Aggregate chunks within the part
    })
  }

  # Combine aggregated parts into the final summary
  combine_summaries(summaries)
}
