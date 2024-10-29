combine_chunk_summaries <- function(summaries) {
  combined_summary <- list(
    rename_success = all(safe_unlist(lapply(summaries, `[[`, "rename_success"))),
    ICD_replacements_1 = combine_comparison_tables(summaries, "ICD_replacements_1"),
    ICD_replacements_2 = combine_comparison_tables(summaries, "ICD_replacements_2"),
    pat_type_unmapped = safe_unlist(lapply(summaries, `[[`, "pat_type_unmapped")),
    memcat_parent_unmapped = safe_unlist(lapply(summaries, `[[`, "memcat_parent_unmapped")),
    memcat_child_unmapped = safe_unlist(lapply(summaries, `[[`, "memcat_child_unmapped")),
    discharge_unmapped = safe_unlist(lapply(summaries, `[[`, "discharge_unmapped")),
    claim_status_unmapped = safe_unlist(lapply(summaries, `[[`, "claim_status_unmapped")),
    discard_rvs_one = combine_discarded_rvs_tables(summaries, "discard_rvs_one"),
    discard_rvs_two = combine_discarded_rvs_tables(summaries, "discard_rvs_two"),
    empty_strings_replaced_1 = combine_replace_empty_tables(summaries, "empty_strings_replaced_1"),
    empty_strings_replaced_2 = combine_replace_empty_tables(summaries, "empty_strings_replaced_2"),
    unique_icds_count = safe_unlist(lapply(summaries, `[[`, "unique_icds")),
    direct_matches = safe_unlist(lapply(summaries, `[[`, "direct_matches")),
    unmatched_codes = safe_unlist(lapply(summaries, `[[`, "unmatched_codes")),
    unmatched_sources = combine_unmatched_icd10_codes(summaries, "unmatched_sources"),
    rvss = safe_unlist(lapply(summaries, `[[`, "rvss")),
    mappable_rvs = safe_unlist(lapply(summaries, `[[`, "mappable_rvs")),
    unmappable_rvs = safe_unlist(lapply(summaries, `[[`, "unmappable_rvs")),
    multi_mapped_rvs = safe_unlist(lapply(summaries, `[[`, "multi_mapped_rvs")),
    without_drg = safe_unlist(lapply(summaries, `[[`, "without_drg")),
    pdx_success = all(safe_unlist(lapply(summaries, `[[`, "pdx_success"))),
    icd10_map_dt = rbindlist(lapply(summaries, `[[`, "icd10_map_dt")),
    pat_type_mapped = rbindlist(lapply(summaries, `[[`, "pat_type_mapped")),
    pat_memcat_parent_mapped = rbindlist(lapply(summaries, `[[`, "pat_memcat_parent_mapped")),
    pat_memcat_child_mapped = rbindlist(lapply(summaries, `[[`, "pat_memcat_child_mapped")),
    clin_discharge_mapped = rbindlist(lapply(summaries, `[[`, "clin_discharge_mapped")),
    claim_status_mapped = rbindlist(lapply(summaries, `[[`, "claim_status_mapped"))
  )

  return(combined_summary)
}
