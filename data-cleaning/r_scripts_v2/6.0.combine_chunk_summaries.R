combine_chunk_summaries <- function(summaries) {
  safe_unlist <- function(x) unique(na.omit(unlist(x)))

  combine_comparison_tables <- function(comparison_field) {
    comparison_list <- lapply(summaries, function(summary) {
      data <- summary[[comparison_field]]
      if (!is.null(data) && nrow(data) > 0) {
        data[, .(old_code, new_code)]
      } else {
        NULL
      }
    })
    combined <- rbindlist(comparison_list, fill = TRUE)

    if (nrow(combined) == 0) {
      return(data.table(old_code = character(), new_code = character(), diff_chars = integer()))
    }

    combined[, `:=`(
      old_code = gsub("\\s", "", iconv(old_code, to = "UTF-8")),
      new_code = gsub("\\s", "", iconv(new_code, to = "UTF-8"))
    )]
    combined[, diff_chars := abs(nchar(old_code) - nchar(new_code))]
    return(unique(combined[order(-diff_chars)]))
  }

  combine_discarded_rvs_tables <- function(field) {
    combined <- rbindlist(lapply(summaries, function(summary) summary[[field]]), fill = TRUE)
    if (nrow(combined) == 0) {
      return(data.table(CODE = character(), count = integer()))
    }
    return(combined[, .(count = sum(count)), by = CODE][order(-count)])
  }

  combine_replace_empty_tables <- function(field) {
    combined <- rbindlist(lapply(summaries, function(summary) summary[[field]]), fill = TRUE)
    if (nrow(combined) == 0) {
      return(data.table(Column = character(), Empty_Replaced = integer(), NA_Replaced = integer(), Character0_Replaced = integer()))
    }
    return(combined[, .(
      Empty_Replaced = sum(Empty_Replaced, na.rm = TRUE),
      NA_Replaced = sum(NA_Replaced, na.rm = TRUE),
      Character0_Replaced = sum(Character0_Replaced, na.rm = TRUE)
    ), by = Column][order(-Empty_Replaced, -NA_Replaced, -Character0_Replaced)])
  }

  combine_unmatched_icd10_codes <- function(field) {
    combined <- rbindlist(lapply(summaries, function(summary) summary[[field]]), fill = TRUE)
    return(combined[, .(count = sum(count)), by = .(code, source)][order(-count)])
  }

  combined_summary <- list(
    rename_success = all(safe_unlist(lapply(summaries, `[[`, "rename_success"))),
    ICD_replacements_1 = combine_comparison_tables("ICD_replacements_1"),
    ICD_replacements_2 = combine_comparison_tables("ICD_replacements_2"),
    pat_type_unmapped = safe_unlist(lapply(summaries, `[[`, "pat_type_unmapped")),
    memcat_parent_unmapped = safe_unlist(lapply(summaries, `[[`, "memcat_parent_unmapped")),
    memcat_child_unmapped = safe_unlist(lapply(summaries, `[[`, "memcat_child_unmapped")),
    discharge_unmapped = safe_unlist(lapply(summaries, `[[`, "discharge_unmapped")),
    claim_status_unmapped = safe_unlist(lapply(summaries, `[[`, "claim_status_unmapped")),
    discard_rvs_one = combine_discarded_rvs_tables("discard_rvs_one"),
    discard_rvs_two = combine_discarded_rvs_tables("discard_rvs_two"),
    empty_strings_replaced_1 = combine_replace_empty_tables("empty_strings_replaced_1"),
    empty_strings_replaced_2 = combine_replace_empty_tables("empty_strings_replaced_2"),
    unique_icds_count = safe_unlist(lapply(summaries, `[[`, "unique_icds")),
    direct_matches_count = safe_unlist(lapply(summaries, `[[`, "direct_matches")),
    unmatched_count = safe_unlist(lapply(summaries, `[[`, "unmatched")),
    unmatched_sources = combine_unmatched_icd10_codes("unmatched_sources"),
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
