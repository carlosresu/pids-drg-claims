# Safely unlists while removing NULLs and NAs
safe_unlist <- function(x) if (length(x) > 0) unlist(x, recursive = TRUE) else character(0)

# Safely extracts fields from summaries with error handling
safe_extract <- function(summary, field) {
  tryCatch(summary[[field]], error = function(e) NULL)
}

# Flattens nested summaries into a list
flatten_summaries <- function(summaries) {
  if (is.list(summaries) && !inherits(summaries, "data.table")) {
    return(do.call(c, lapply(summaries, flatten_summaries)))
  } else {
    return(list(summaries))
  }
}

# Combines comparison tables from summaries
combine_comparison_tables <- function(summaries, field) {
  comparison_list <- lapply(summaries, function(summary) {
    safe_extract(summary, field) %||%
      data.table(old_code = character(), new_code = character(), diff_chars = integer())
  })

  combined <- rbindlist(comparison_list, fill = TRUE)
  combined[, `:=`(
    old_code = gsub("\\s", "", iconv(old_code, to = "UTF-8")),
    new_code = gsub("\\s", "", iconv(new_code, to = "UTF-8"))
  )]
  combined[, diff_chars := abs(nchar(old_code) - nchar(new_code))]

  return(unique(combined[order(-diff_chars)]))
}

# Combines discarded RVS tables
combine_discarded_rvs_tables <- function(summaries, field) {
  combined <- rbindlist(lapply(summaries, function(s) safe_extract(s, field)), fill = TRUE)
  if (nrow(combined) == 0) {
    return(data.table(CODE = character(), count = integer()))
  }
  return(combined[, .(count = sum(count)), by = CODE][order(-count)])
}

# Combines empty string replacements
combine_replace_empty_tables <- function(summaries, field) {
  combined <- rbindlist(lapply(summaries, function(s) safe_extract(s, field)), fill = TRUE)
  if (nrow(combined) == 0) {
    return(data.table(
      Column = character(), Empty_Replaced = integer(),
      NA_Replaced = integer(), Character0_Replaced = integer()
    ))
  }
  return(combined[, .(
    Empty_Replaced = sum(Empty_Replaced, na.rm = TRUE),
    NA_Replaced = sum(NA_Replaced, na.rm = TRUE),
    Character0_Replaced = sum(Character0_Replaced, na.rm = TRUE)
  ), by = Column][order(-Empty_Replaced, -NA_Replaced, -Character0_Replaced)])
}

# Combines modified matches
combine_modified_matches <- function(summaries, field) {
  combined_original <- unlist(lapply(summaries, function(s) safe_extract(s, field)$modified_matches), use.names = FALSE)
  combined_modified <- unlist(lapply(summaries, function(s) safe_extract(s, field)$modified_match), use.names = FALSE)

  if (length(combined_original) > 0 && length(combined_modified) > 0) {
    return(data.table(
      modified_matches = combined_original,
      modified_match = combined_modified,
      char_diff = abs(nchar(combined_original) - nchar(combined_modified))
    )[, .(count = .N), by = .(modified_matches, modified_match, char_diff)])
  } else {
    return(data.table(
      modified_matches = character(), modified_match = character(),
      char_diff = integer(), count = integer()
    ))
  }
}

# Combines unmatched ICD-10 codes
combine_unmatched_icd10_codes <- function(summaries, field) {
  combined_list <- lapply(summaries, function(s) safe_extract(s, field))
  combined_list <- Filter(function(x) !is.null(x) && nrow(x) > 0, combined_list)

  if (length(combined_list) == 0) {
    cat("No valid data found for field:", field, "\n")
    return(data.table(code = character(), source = character(), count = integer()))
  }

  combined <- rbindlist(combined_list, fill = TRUE)
  combined <- combined[!is.na(code) & code != ""]

  return(combined[, .(count = sum(count)), by = .(code, source)][order(-count)])
}

# Aggregates all summaries
aggregate_all_summaries <- function(all_parts_summaries) {
  flattened_summaries <- flatten_summaries(all_parts_summaries)

  final_summary <- list(
    final_rename_success = all(safe_unlist(lapply(flattened_summaries, function(s) safe_extract(s, "rename_success")))),
    final_ICD_replacements_1 = combine_comparison_tables(flattened_summaries, "ICD_replacements_1"),
    final_ICD_replacements_2 = combine_comparison_tables(flattened_summaries, "ICD_replacements_2"),
    final_pat_type_unmapped = safe_unlist(lapply(flattened_summaries, function(s) safe_extract(s, "pat_type_unmapped"))),
    final_memcat_parent_unmapped = safe_unlist(lapply(flattened_summaries, function(s) safe_extract(s, "memcat_parent_unmapped"))),
    final_memcat_child_unmapped = safe_unlist(lapply(flattened_summaries, function(s) safe_extract(s, "memcat_child_unmapped"))),
    final_discharge_unmapped = safe_unlist(lapply(flattened_summaries, function(s) safe_extract(s, "discharge_unmapped"))),
    final_claim_status_unmapped = safe_unlist(lapply(flattened_summaries, function(s) safe_extract(s, "claim_status_unmapped"))),
    final_discard_rvs_one = combine_discarded_rvs_tables(flattened_summaries, "discard_rvs_one"),
    final_discard_rvs_two = combine_discarded_rvs_tables(flattened_summaries, "discard_rvs_two"),
    final_empty_strings_replaced_1 = combine_replace_empty_tables(flattened_summaries, "empty_strings_replaced_1"),
    final_empty_strings_replaced_2 = combine_replace_empty_tables(flattened_summaries, "empty_strings_replaced_2"),
    final_unique_icds = length(unique(safe_unlist(lapply(flattened_summaries, function(s) safe_extract(s, "unique_icds"))))),
    final_direct_matches = length(unique(safe_unlist(lapply(flattened_summaries, function(s) safe_extract(s, "direct_matches"))))),
    final_modified_matches = combine_modified_matches(flattened_summaries, "modified_matches"),
    final_unmatched_codes = unique(safe_unlist(lapply(flattened_summaries, function(s) safe_extract(s, "unmatched_codes")))),
    final_unmatched_sources = combine_unmatched_icd10_codes(flattened_summaries, "unmatched_sources"),
    final_rvss = length(unique(safe_unlist(lapply(flattened_summaries, function(s) safe_extract(s, "rvss"))))),
    final_mappable_rvs = length(unique(safe_unlist(lapply(flattened_summaries, function(s) safe_extract(s, "mappable_rvs"))))),
    final_unmappable_rvs = length(unique(safe_unlist(lapply(flattened_summaries, function(s) safe_extract(s, "unmappable_rvs"))))),
    final_multi_mapped_rvs = length(unique(safe_unlist(lapply(flattened_summaries, function(s) safe_extract(s, "multi_mapped_rvs"))))),
    final_without_drg = length(unique(safe_unlist(lapply(flattened_summaries, function(s) safe_extract(s, "without_drg"))))),
    final_pdx_success = all(safe_unlist(lapply(flattened_summaries, function(s) safe_extract(s, "pdx_success")))),
    final_icd10_map_dt = unique(rbindlist(lapply(flattened_summaries, function(s) safe_extract(s, "icd10_map_dt")), fill = TRUE)),
    final_pat_type_mapped = unique(rbindlist(lapply(flattened_summaries, function(s) safe_extract(s, "pat_type_mapped")), fill = TRUE)),
    final_pat_memcat_parent_mapped = unique(rbindlist(lapply(flattened_summaries, function(s) safe_extract(s, "pat_memcat_parent_mapped")), fill = TRUE)),
    final_pat_memcat_child_mapped = unique(rbindlist(lapply(flattened_summaries, function(s) safe_extract(s, "pat_memcat_child_mapped")), fill = TRUE)),
    final_clin_discharge_mapped = unique(rbindlist(lapply(flattened_summaries, function(s) safe_extract(s, "clin_discharge_mapped")), fill = TRUE)),
    final_claim_status_mapped = unique(rbindlist(lapply(flattened_summaries, function(s) safe_extract(s, "claim_status_mapped")), fill = TRUE))
  )

  return(final_summary)
}
