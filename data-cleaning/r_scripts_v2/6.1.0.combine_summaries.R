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
    empty_strings_replaced_1 = combine_replace_empty_tables(
      rbindlist(lapply(summaries, function(s) safe_extract(s, "empty_strings_replaced_1")), fill = TRUE)
    ),
    empty_strings_replaced_2 = combine_replace_empty_tables(
      rbindlist(lapply(summaries, function(s) safe_extract(s, "empty_strings_replaced_2")), fill = TRUE)
    ),
    unique_icds = unique(safe_unlist(lapply(summaries, function(s) safe_access(s, "unique_icds")))),
    direct_matches = unique(safe_unlist(lapply(summaries, function(s) safe_access(s, "direct_matches")))),
    modified_matches = extract_modified_matches(summaries),
    unmatched_codes = unique(safe_unlist(lapply(summaries, function(s) safe_access(s, "unmatched_codes")))),
    unmatched_sources = rbindlist(lapply(summaries, function(s) safe_access(s, "unmatched_sources")), fill = TRUE),
    rvss = unique(safe_unlist(lapply(summaries, function(s) safe_access(s, "rvss")))),
    mappable_rvs = unique(safe_unlist(lapply(summaries, function(s) safe_access(s, "mappable_rvs")))),
    unmappable_rvs = unique(safe_unlist(lapply(summaries, function(s) safe_access(s, "unmappable_rvs")))),
    multi_mapped_rvs = unique(safe_unlist(lapply(summaries, function(s) safe_access(s, "multi_mapped_rvs")))),
    without_drg = unique(safe_unlist(lapply(summaries, function(s) safe_access(s, "without_drg")))),
    pdx_success = all(safe_unlist(lapply(summaries, function(s) safe_access(s, "pdx_success")))),
    icd10_map_dt = rbindlist(lapply(summaries, function(s) safe_access(s, "icd10_map_dt")), fill = TRUE),
    pat_type_mapped = rbindlist(lapply(summaries, function(s) safe_access(s, "pat_type_mapped")), fill = TRUE),
    pat_memcat_parent_mapped = rbindlist(lapply(summaries, function(s) safe_access(s, "pat_memcat_parent_mapped")), fill = TRUE),
    pat_memcat_child_mapped = rbindlist(lapply(summaries, function(s) safe_access(s, "pat_memcat_child_mapped")), fill = TRUE),
    clin_discharge_mapped = rbindlist(lapply(summaries, function(s) safe_access(s, "clin_discharge_mapped")), fill = TRUE),
    claim_status_mapped = rbindlist(lapply(summaries, function(s) safe_access(s, "claim_status_mapped")), fill = TRUE),
    replacement_summary = combine_replace_empty_tables(
      rbindlist(lapply(summaries, function(s) safe_extract(s, "replacement_summary")), fill = TRUE)
    )
  )

  return(combined_summary)
}
