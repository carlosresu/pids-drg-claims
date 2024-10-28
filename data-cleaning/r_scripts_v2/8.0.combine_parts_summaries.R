combine_parts_summaries <- function(combined_summary) {
  final_combined_summaries <- list(
    final_rename_success = all(safe_unlist(lapply(combined_summary, `[[`, "rename_success"))),
    final_ICD_replacements_1 = combine_comparison_tables(combined_summary, "ICD_replacements_1"),
    final_ICD_replacements_2 = combine_comparison_tables(combined_summary, "ICD_replacements_2"),
    final_pat_type_unmapped = safe_unlist(lapply(combined_summary, `[[`, "pat_type_unmapped")),
    final_memcat_parent_unmapped = safe_unlist(lapply(combined_summary, `[[`, "memcat_parent_unmapped")),
    final_memcat_child_unmapped = safe_unlist(lapply(combined_summary, `[[`, "memcat_child_unmapped")),
    final_discharge_unmapped = safe_unlist(lapply(combined_summary, `[[`, "discharge_unmapped")),
    final_claim_status_unmapped = safe_unlist(lapply(combined_summary, `[[`, "claim_status_unmapped")),
    final_discard_rvs_one = combine_discarded_rvs_tables(combined_summary, "discard_rvs_one"),
    final_discard_rvs_two = combine_discarded_rvs_tables(combined_summary, "discard_rvs_two"),
    final_empty_strings_replaced_0 = final_combine_replace_empty_tables(combined_summary, "replacement_summary"),
    final_empty_strings_replaced_1 = final_combine_replace_empty_tables(combined_summary, "empty_strings_replaced_1"),
    final_empty_strings_replaced_2 = final_combine_replace_empty_tables(combined_summary, "empty_strings_replaced_2"),
    final_unique_icds = length(safe_unlist(lapply(combined_summary, `[[`, "unique_icds_count"))),
    final_direct_matches = length(safe_unlist(lapply(combined_summary, `[[`, "direct_matches_count"))),
    final_unmatched = length(safe_unlist(lapply(combined_summary, `[[`, "unmatched_count"))),
    final_unmatched_sources = combine_unmatched_icd10_codes(combined_summary, "unmatched_sources"),
    final_rvss = length(safe_unlist(lapply(combined_summary, `[[`, "rvss"))),
    final_mappable_rvs = length(safe_unlist(lapply(combined_summary, `[[`, "mappable_rvs"))),
    final_unmappable_rvs = length(safe_unlist(lapply(combined_summary, `[[`, "unmappable_rvs"))),
    final_multi_mapped_rvs = length(safe_unlist(lapply(combined_summary, `[[`, "multi_mapped_rvs"))),
    final_without_drg = length(safe_unlist(lapply(combined_summary, `[[`, "without_drg"))),
    final_pdx_success = all(safe_unlist(lapply(combined_summary, `[[`, "pdx_success"))),
    final_icd10_map_dt = unique(rbindlist(lapply(combined_summary, `[[`, "icd10_map_dt"))),
    final_pat_type_mapped = unique(rbindlist(lapply(combined_summary, `[[`, "pat_type_mapped"))),
    final_memcat_parent_mapped = unique(rbindlist(lapply(combined_summary, `[[`, "pat_memcat_parent_mapped"))),
    final_memcat_child_mapped = unique(rbindlist(lapply(combined_summary, `[[`, "pat_memcat_child_mapped"))),
    final_clin_discharge_mapped = unique(rbindlist(lapply(combined_summary, `[[`, "clin_discharge_mapped"))),
    final_claim_status_mapped = unique(rbindlist(lapply(combined_summary, `[[`, "claim_status_mapped")))
  )

  return(final_combined_summaries)
}
