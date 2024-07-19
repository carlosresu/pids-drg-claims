# source(here("data-cleaning", "r_scripts", "libraries.R"))

remove_lumped_icd_codes <- function(column) {
  modified_column <- gsub("(?<=\\d)(?=[A-Za-z])", "||", column, perl = TRUE)
  return(modified_column)
}

transfer_extra_icd10s_to_clin_icd <- function(clin_icd, col) {
  clin_icd <- lapply(clin_icd, function(x) if (is.null(x)) character() else x)
  col_first <- lapply(col, function(x) x[1])

  clin_icd <- mapply(function(icd, c1) {
    c(icd, c1[-1])
  }, clin_icd, col, SIMPLIFY = FALSE)

  return(list(clin_icd = clin_icd, col_first = col_first))
}

get_unique_icd_codes <- function(clin_c1, clin_c2, clin_icd) {
  icds <- unique(c(unlist(clin_c1), unlist(clin_c2), unlist(clin_icd)))
  icds <- icds[!is.na(icds)]
  return(icds)
}

create_thai_icd10_environment <- function(thai_icd10_codes) {
  thai_icd10_env <- list2env(
    setNames(as.list(rep(TRUE, length(thai_icd10_codes))), thai_icd10_codes)
  )
  return(thai_icd10_env)
}

find_direct_icd_matches <- function(icds, thai_icd10_env) {
  direct_matches <- mget(
    icds, thai_icd10_env,
    ifnotfound = as.list(rep(FALSE, length(icds)))
  )
  direct_match_codes <- names(
    unlist(direct_matches[unlist(direct_matches) == TRUE])
  )
  return(direct_match_codes)
}

generate_icd10_mapping <- function(icds, thai_icd10_env, neoplasms_env) {
  icd_mapping <- list()
  modified_count <- 0
  for (d in icds) {
    d <- str_trim(d)
    if (exists(d, thai_icd10_env)) {
      icd_mapping[[d]] <- d
    } else if (
      !exists(d, neoplasms_env) && grepl("[A-Za-z]", d) && grepl("[0-9]", d)) {
      if (nchar(d) == 3 && exists(paste0(d, "9"), thai_icd10_env)) {
        icd_mapping[[d]] <- paste0(d, "9")
        modified_count <- modified_count + 1
      } else if (nchar(d) >= 4) {
        for (i in seq_len(nchar(d) - 3)) {
          new_d <- substr(d, 1, nchar(d) - i)
          if (exists(new_d, thai_icd10_env)) {
            icd_mapping[[d]] <- new_d
            modified_count <- modified_count + 1
            break
          }
        }
      }
    }
  }
  return(list(icd_mapping = icd_mapping, modified_count = modified_count))
}

# Helper function to map ICD-10 codes to columns
apply_icd10_mapping_to_columns <- function(
    clin_c1, clin_c2, clin_icd, icd10_env) {
  map_icd10_helper <- function(codes) {
    mapped <- mget(codes, icd10_env, ifnotfound = as.list(codes))
    return(unname(unlist(mapped)))
  }

  clin_c1_mapped <- lapply(clin_c1, map_icd10_helper)
  clin_c2_mapped <- lapply(clin_c2, map_icd10_helper)
  clin_icd_mapped <- lapply(clin_icd, map_icd10_helper)

  return(
    list(
      clin_c1 = clin_c1_mapped,
      clin_c2 = clin_c2_mapped,
      clin_icd = clin_icd_mapped
    )
  )
}

implement_icd10_mapping <- function(
    clin_c1, clin_c2, clin_icd, tdrg_icd10, rows_to_show = Inf) {
  icds <- get_unique_icd_codes(clin_c1, clin_c2, clin_icd)

  thai_icd10_env <- create_thai_icd10_environment(
    unique(tdrg_icd10$CODE)
  )
  neoplasms_env <- create_thai_icd10_environment(
    unique(tdrg_icd10[grepl("/", tdrg_icd10$CODE), "CODE"])
  )

  direct_match_codes <- find_direct_icd_matches(icds, thai_icd10_env)
  cat(
    sprintf(
      "\n\nThere are %d unique entries for ICD-10 codes, of which %d (%.2f%%)",
      length(icds), length(direct_match_codes),
      length(direct_match_codes) * 100 / length(icds)
    ),
    " are directly in the Thai ICD-10 library\n"
  )

  icd_mapping_info <- generate_icd10_mapping(
    icds, thai_icd10_env, neoplasms_env
  )
  icd_mapping <- icd_mapping_info$icd_mapping
  modified_count <- icd_mapping_info$modified_count
  cat(sprintf(
    "The modifications led to a total of %d",
    length(icd_mapping)
  ), " codes being mapped to an equivalent in the Thai ICD10 library.\n")
  cat(sprintf("Out of these, %d were modified to match.\n", modified_count))

  unmatched_icds <- setdiff(icds, names(icd_mapping))
  if (length(unmatched_icds) > 0) {
    cat(sprintf(
      "There are %d codes that could not",
      length(unmatched_icds)
    ), "be mapped to the Thai ICD10 library:\n")
    unmatched_sources <- data.table(
      code = unmatched_icds, source = NA_character_, count = 0
    )

    for (col_name in c("clin_c1", "clin_c2", "clin_icd")) {
      col_values <- get(col_name)
      unmatched_sources[
        code %in% unlist(col_values),
        source := col_name
      ]
      unmatched_sources[
        code %in% unlist(col_values),
        count := count + table(unlist(col_values))[code]
      ]
    }

    unmatched_sources <- unmatched_sources[order(-count)]
  } else {
    unmatched_sources <- data.table()
  }

  icd10_map <- data.table(
    phl_icd10 = names(icd_mapping),
    tdrg_icd10 = unlist(icd_mapping)
  )
  fwrite(icd10_map, paste0(
    "cache/icd10_map_file_",
    year_to_load, ".csv"
  ))
  icd10_env <- list2env(setNames(
    as.list(icd10_map$tdrg_icd10),
    icd10_map$phl_icd10
  ))

  mapped_columns <- apply_icd10_mapping_to_columns(
    clin_c1, clin_c2, clin_icd, icd10_env
  )

  # Generate comparison table
  original_data <- list(
    clin_c1 = clin_c1,
    clin_c2 = clin_c2, clin_icd = clin_icd
  )
  modified_data <- list(
    clin_c1 = mapped_columns$clin_c1,
    clin_c2 = mapped_columns$clin_c2,
    clin_icd = mapped_columns$clin_icd
  )

  padded_data <- lapply(
    names(original_data),
    function(name) {
      pad_list_elements(
        original_data[[name]],
        modified_data[[name]]
      )
    }
  )

  comparison_table <- rbind(
    generate_comparison_table(padded_data[[1]][[1]], padded_data[[1]][[2]]),
    generate_comparison_table(padded_data[[2]][[1]], padded_data[[2]][[2]]),
    generate_comparison_table(padded_data[[3]][[1]], padded_data[[3]][[2]])
  )

  comparison_table <- comparison_table[order(-count)]

  # Check if all resulting ICD codes are in either the
  # Thai library or the PhilHealth library
  all_icds <- unique(c(
    unlist(mapped_columns$clin_c1),
    unlist(mapped_columns$clin_c2), unlist(mapped_columns$clin_icd)
  ))
  valid_icds <- unique(c(tdrg_icd10$CODE, rvs_icd9$icd9cm))
  invalid_icds <- setdiff(all_icds, valid_icds)
  invalid_icds <- invalid_icds[!is.na(invalid_icds) & invalid_icds != "NA"]

  if (length(invalid_icds) > 0) {
    invalid_icds_table <- data.table(
      code = invalid_icds,
      count = sapply(
        invalid_icds,
        function(icd) {
          sum(c(
            unlist(mapped_columns$clin_c1),
            unlist(mapped_columns$clin_c2),
            unlist(mapped_columns$clin_icd)
          ) == icd, na.rm = TRUE)
        }
      )
    )

    invalid_icds_table <- invalid_icds_table[!is.na(code) & code != ""]
    invalid_icds_table <- invalid_icds_table[order(-count)]
  } else {
    invalid_icds_table <- data.table()
  }

  return(list(
    clin_c1 = mapped_columns$clin_c1,
    clin_c2 = mapped_columns$clin_c2,
    clin_icd = mapped_columns$clin_icd,
    var1 = length(icds),
    var2 = length(direct_match_codes),
    var4 = length(icd_mapping),
    var5 = modified_count,
    var6 = length(unmatched_icds),
    var7 = unmatched_sources,
    comparison_table = comparison_table,
    invalid_icds_table = invalid_icds_table
  ))
}



ensure_unique_icd_codes <- function(clin_c1, clin_c2, clin_icd) {
  # Convert lists to data.table for efficient processing
  dt <- data.table(clin_c1 = clin_c1, clin_c2 = clin_c2, clin_icd = clin_icd)

  # Deduplicate each column
  dt[, clin_c1 := lapply(clin_c1, unique)]
  dt[, clin_c2 := lapply(clin_c2, unique)]
  dt[, clin_icd := lapply(clin_icd, unique)]

  # Remove entries in clin_icd that are in clin_c1 or clin_c2
  dt[, clin_icd := Map(function(c1, c2, icd) {
    setdiff(icd, union(c1, c2))
  }, clin_c1, clin_c2, clin_icd)]

  # Remove entries in clin_c1 that are in clin_c2
  dt[, clin_c1 := Map(function(c1, c2) {
    setdiff(c1, c2)
  }, clin_c1, clin_c2)]

  # Remove entries in clin_c2 that are in clin_c1
  dt[, clin_c2 := Map(function(c1, c2) {
    setdiff(c2, c1)
  }, clin_c1, clin_c2)]

  return(list(clin_c1 = dt$clin_c1, clin_c2 = dt$clin_c2, clin_icd = dt$clin_icd))
}

generate_comparison_table <- function(original, modified) {
  original_unlisted <- unlist(original, use.names = FALSE)
  modified_unlisted <- unlist(modified, use.names = FALSE)

  comparison <- data.table(
    old_code = original_unlisted,
    new_code = modified_unlisted
  )

  comparison <- comparison[old_code != new_code,
    .(count = .N),
    by = .(old_code, new_code)
  ]

  return(comparison)
}


pad_list_elements <- function(list1, list2) {
  max_length <- max(lengths(list1), lengths(list2))

  pad_with_na <- function(lst, max_length) {
    lapply(lst, function(x) {
      if (length(x) < max_length) {
        x <- c(x, rep(NA, max_length - length(x)))
      }
      return(x)
    })
  }

  list1 <- pad_with_na(list1, max_length)
  list2 <- pad_with_na(list2, max_length)

  return(list(list1, list2))
}

aggregate_icd10_stats <- function(summaries) {
  # Check the structure of summaries
  if (length(summaries) == 0) {
    stop("The summaries list is empty.")
  }
  if (!all(sapply(summaries, is.list))) {
    stop("All elements in summaries should be lists.")
  }

  required_fields <- c(
    "total_unique_icd_count", "direct_match_count",
    "modified_count", "total_mapped_count",
    "unmapped_icd_count", "unmapped_icds"
  )
  for (i in seq_along(summaries)) {
    summary <- summaries[[i]]
    missing_fields <- setdiff(required_fields, names(summary))
    if (length(missing_fields) > 0) {
      stop(sprintf(
        "Summary %d is missing fields: %s", i,
        paste(missing_fields, collapse = ", ")
      ))
    }
  }

  total_unique_icd_count <- sum(sapply(
    summaries,
    function(res) res$total_unique_icd_count
  ))
  direct_match_count <- sum(sapply(
    summaries,
    function(res) res$direct_match_count
  ))
  modified_count <- sum(sapply(
    summaries,
    function(res) res$modified_count
  ))
  total_mapped_count <- sum(sapply(
    summaries,
    function(res) res$total_mapped_count
  ))
  unmapped_icd_count <- sum(sapply(
    summaries,
    function(res) res$unmapped_icd_count
  ))

  unmapped_icds_list <- lapply(
    summaries,
    function(res) res$unmapped_icds
  )
  combined_unmapped_icds <- rbindlist(unmapped_icds_list, fill = TRUE)
  combined_unmapped_icds <- combined_unmapped_icds[,
    .(count = sum(count)),
    by = code
  ][order(-count)]

  return(list(
    total_unique_icd_count = total_unique_icd_count,
    direct_match_count = direct_match_count,
    direct_match_percentage = ifelse(total_unique_icd_count > 0,
      (direct_match_count / total_unique_icd_count) * 100, 0
    ),
    total_mapped_count = total_mapped_count,
    modified_count = modified_count,
    unmapped_icd_count = unmapped_icd_count,
    combined_unmapped_icds = combined_unmapped_icds
  ))
}
