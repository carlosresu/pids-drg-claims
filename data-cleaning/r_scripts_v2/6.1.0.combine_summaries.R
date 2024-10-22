combine_summaries <- function(summaries) {
  #' @title Combine Summaries
  #'
  #' @description This function combines multiple summaries into one summary.
  #'
  #' @param summaries list. A list of summary tables.
  #' @return list. The combined summary.

  combined_summary <- list(
    rename_success = all(unlist(sapply(
      summaries,
      function(summary) summary$rename_success
    )), na.rm = TRUE),
    ICD_replacements_1 = combine_comparison_tables(
      summaries, "ICD_replacements_1"
    ),
    ICD_replacements_2 = combine_comparison_tables(
      summaries, "ICD_replacements_2"
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
      summaries, "discard_rvs_one"
    ),
    discard_rvs_two = combine_discarded_rvs_tables(
      summaries, "discard_rvs_two"
    ),
    empty_strings_replaced_1 = combine_replace_empty_tables(
      summaries, "empty_strings_replaced_1"
    ),
    empty_strings_replaced_2 = combine_replace_empty_tables(
      summaries, "empty_strings_replaced_2"
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
      summaries, "unmatched_sources"
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
