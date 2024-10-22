combine_parts_summaries <- function(combined_summary) {
  final_combined_summaries <- list(
    final_rename_success = all(unlist(sapply(
      combined_summary,
      function(summary) summary$rename_success
    )), na.rm = TRUE),
    final_ICD_replacements_1 = combine_comparison_tables(
      combined_summary, "ICD_replacements_1"
    ),
    final_ICD_replacements_2 = combine_comparison_tables(
      combined_summary, "ICD_replacements_2"
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
      combined_summary, "discard_rvs_one"
    ),
    final_discard_rvs_two = combine_discarded_rvs_tables(
      combined_summary, "discard_rvs_two"
    ),
    final_empty_strings_replaced_0 = final_combine_replace_empty_tables(
      combined_summary, "replacement_summary"
    ),
    final_empty_strings_replaced_1 = final_combine_replace_empty_tables(
      combined_summary, "empty_strings_replaced_1"
    ),
    final_empty_strings_replaced_2 = final_combine_replace_empty_tables(
      combined_summary, "empty_strings_replaced_2"
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
      combined_summary, "unmatched_sources"
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
