combine_chunk_summaries <- function(summaries) {
  combined_summary <- list(
    rename_success = all(safe_unlist(lapply(summaries, function(s) s[["rename_success"]]))),
    ICD_replacements_1 = rbindlist(lapply(summaries, function(s) s[["ICD_replacements_1"]]), fill = TRUE),
    ICD_replacements_2 = rbindlist(lapply(summaries, function(s) s[["ICD_replacements_2"]]), fill = TRUE),
    pat_type_unmapped = safe_unlist(lapply(summaries, function(s) s[["pat_type_unmapped"]])),
    memcat_parent_unmapped = safe_unlist(lapply(summaries, function(s) s[["memcat_parent_unmapped"]])),
    memcat_child_unmapped = safe_unlist(lapply(summaries, function(s) s[["memcat_child_unmapped"]])),
    discharge_unmapped = safe_unlist(lapply(summaries, function(s) s[["discharge_unmapped"]])),
    claim_status_unmapped = safe_unlist(lapply(summaries, function(s) s[["claim_status_unmapped"]])),
    discard_rvs_one = rbindlist(lapply(summaries, function(s) s[["discard_rvs_one"]]), fill = TRUE),
    discard_rvs_two = rbindlist(lapply(summaries, function(s) s[["discard_rvs_two"]]), fill = TRUE),
    empty_strings_replaced_1 = rbindlist(lapply(summaries, function(s) s[["empty_strings_replaced_1"]]), fill = TRUE),
    empty_strings_replaced_2 = rbindlist(lapply(summaries, function(s) s[["empty_strings_replaced_2"]]), fill = TRUE),
    unique_icds_count = unique(safe_unlist(lapply(summaries, function(s) s[["unique_icds"]]))),
    direct_matches = unique(safe_unlist(lapply(summaries, function(s) s[["direct_matches"]]))),
    modified_matches = list(
      modified_matches = unique(safe_unlist(lapply(summaries, function(s) s[["modified_matches"]][["modified_matches"]]))),
      modified_match = unique(safe_unlist(lapply(summaries, function(s) s[["modified_matches"]][["modified_match"]])))
    ),
    unmatched_codes = unique(safe_unlist(lapply(summaries, function(s) s[["unmatched_codes"]]))),
    unmatched_sources = rbindlist(lapply(summaries, function(s) s[["unmatched_sources"]]), fill = TRUE),
    rvss = unique(safe_unlist(lapply(summaries, function(s) s[["rvss"]]))),
    mappable_rvs = unique(safe_unlist(lapply(summaries, function(s) s[["mappable_rvs"]]))),
    unmappable_rvs = unique(safe_unlist(lapply(summaries, function(s) s[["unmappable_rvs"]]))),
    multi_mapped_rvs = unique(safe_unlist(lapply(summaries, function(s) s[["multi_mapped_rvs"]]))),
    without_drg = unique(safe_unlist(lapply(summaries, function(s) s[["without_drg"]]))),
    pdx_success = all(safe_unlist(lapply(summaries, function(s) s[["pdx_success"]]))),
    icd10_map_dt = rbindlist(lapply(summaries, function(s) s[["icd10_map_dt"]]), fill = TRUE),
    pat_type_mapped = rbindlist(lapply(summaries, function(s) s[["pat_type_mapped"]]), fill = TRUE),
    pat_memcat_parent_mapped = rbindlist(lapply(summaries, function(s) s[["pat_memcat_parent_mapped"]]), fill = TRUE),
    pat_memcat_child_mapped = rbindlist(lapply(summaries, function(s) s[["pat_memcat_child_mapped"]]), fill = TRUE),
    clin_discharge_mapped = rbindlist(lapply(summaries, function(s) s[["clin_discharge_mapped"]]), fill = TRUE),
    claim_status_mapped = rbindlist(lapply(summaries, function(s) s[["claim_status_mapped"]]), fill = TRUE)
  )

  return(combined_summary)
}
