source(here("data-cleaning", "r_scripts", "libraries.R"))

read_entire_file <- function(drop_cols) {
  dt <- fread(full_claims,
    na.strings = na_values, drop = drop_cols,
    colClasses = col_classes
  )
  return(dt)
}

read_sampled_file <- function() {
  dt <- fread(sampled_claims, na.strings = na_values, colClasses = col_classes)
  return(dt)
}

sample_data <- function(dt) {
  dt <- dt[sample(.N, min(sample_size, .N))]
  return(dt)
}

rename_columns <- function(dt) {
  setnames(dt, old = old_colnames, new = new_colnames)
  return(dt)
}

clean_column <- function(column_to_clean, na_like_strings) {
  column_to_clean <- as.character(column_to_clean)
  cleaned_col <- iconv(column_to_clean, to = "UTF-8", sub = "byte")
  cleaned_col <- toupper(cleaned_col)
  cleaned_col <- stri_replace_all_regex(cleaned_col, "[ \n]", "")
  cleaned_col <- stri_replace_all_regex(cleaned_col, "[^\\w\\d\\/\\s]+", "")
  cleaned_col <- stri_trim_both(cleaned_col)
  cleaned_col <- ifelse(cleaned_col %in% na_like_strings,
    NA_character_, cleaned_col
  )

  return(cleaned_col)
}

collapse_columns <- function(cols_to_process, na_like_strings) {
  cleaned_columns <- lapply(cols_to_process, function(col) {
    clean_column(col, na_like_strings)
  })
  collapsed_column <- do.call(paste, c(cleaned_columns, sep = "||"))
  collapsed_column <- stri_replace_all_regex(collapsed_column, "\\|\\|NA", "")
  collapsed_column <- stri_replace_all_regex(collapsed_column, "NA\\|\\|", "")
  collapsed_column <- stri_replace_all_regex(collapsed_column, "\\|\\|$", "")
  collapsed_column <- ifelse(collapsed_column %in% na_like_strings,
    NA_character_, collapsed_column
  )
  return(collapsed_column)
}

replace_empty_with_na <- function(dt, to_view_checks) {
  char_factor_cols <- names(dt)[sapply(
    dt,
    function(col) is.character(col) || is.factor(col) || is.list(col)
  )]

  replacement_summary <- data.table(
    Column = character(),
    Empty_Replaced = integer(),
    NA_Replaced = integer(),
    Character0_Replaced = integer()
  )

  for (col_name in char_factor_cols) {
    col <- dt[[col_name]]
    if (to_view_checks) {
      empty_count <- sum(col == "", na.rm = TRUE)
      na_count <- sum(col == "NA", na.rm = TRUE)
      char0_count <- sum(col == "character(0)", na.rm = TRUE)
    }

    # Using set to avoid copying
    dt[
      get(col_name) == "" |
        get(col_name) == "NA" |
        get(col_name) == "character(0)", (col_name) := NA_character_
    ]

    if (is.factor(col)) {
      set(dt, j = col_name, value = factor(dt[[col_name]], levels = c(levels(col), NA)))
    }

    if (to_view_checks) {
      replacement_summary <- rbind(replacement_summary, data.table(
        Column = col_name,
        Empty_Replaced = empty_count,
        NA_Replaced = na_count,
        Character0_Replaced = char0_count
      ))
    }
  }

  if (to_view_checks) {
    # Filter out rows where all counts are zero
    replacement_summary <- replacement_summary[
      Empty_Replaced > 0 | NA_Replaced > 0 | Character0_Replaced > 0
    ]
    # print("Replacement Summary:")
    # print(replacement_summary)
  }

  return(list(data = dt, replacement_summary = replacement_summary))
}


split_to_vector <- function(column) {
  result <- lapply(column, function(x) {
    if (is.na(x)) {
      return(NA_character_)
    } else {
      return(unlist(strsplit(x, "||", fixed = TRUE)))
    }
  })
  return(result)
}

remap_patient_type <- function(pat_type) {
  known_types <- c("MEMBER", "DEPENDENT")
  remapped_pat_type <- fcase(
    pat_type == "MEMBER", "MEM",
    pat_type == "DEPENDENT", "DEP"
  )
  unknown_types <- setdiff(
    pat_type[!is.na(pat_type)],
    known_types
  )
  list(remapped = remapped_pat_type, unmapped = unknown_types)
}

remap_memcat_parent_desc <- function(pat_memcat_parent) {
  known_parents <- c("DIRECT CONTRIBUTOR", "INDIRECT CONTRIBUTOR")
  remapped_memcat_parent <- fcase(
    pat_memcat_parent == "DIRECT CONTRIBUTOR", "DIRECT",
    pat_memcat_parent == "INDIRECT CONTRIBUTOR", "INDIRECT"
  )
  unknown_parents <- setdiff(
    pat_memcat_parent[!is.na(pat_memcat_parent)],
    known_parents
  )
  list(remapped = remapped_memcat_parent, unmapped = unknown_parents)
}

remap_memcat_child_desc <- function(pat_memcat_child) {
  known_children <- c(
    "EMPLOYED PRIVATE", "SELF-EARNING INDIVIDUAL", "SENIOR CITIZEN", "INDIGENT",
    "LIFETIME MEMBER", "SPONSORED", "MIGRANT WORKER", "EMPLOYED GOVERNMENT",
    "INFORMAL ECONOMY", "HOUSEHOLD HELP/KASAMBAHAY", "FOREIGN NATIONAL",
    "FILIPINOS WITH DUAL CITIZENSHIP / LIVING ABROAD",
    "SELF EARNING INDIVIDUAL", "FAMILY DRIVER"
  )
  remapped_memcat_child <- fcase(
    pat_memcat_child == "EMPLOYED PRIVATE", "FORMAL",
    pat_memcat_child == "SELF-EARNING INDIVIDUAL", "INFORMAL",
    pat_memcat_child == "SENIOR CITIZEN", "SENIOR",
    pat_memcat_child == "INDIGENT", "INDIGENT",
    pat_memcat_child == "LIFETIME MEMBER", "LIFETIME",
    pat_memcat_child == "SPONSORED", "SPONSORED",
    pat_memcat_child == "MIGRANT WORKER", "INFORMAL",
    pat_memcat_child == "EMPLOYED GOVERNMENT", "FORMAL",
    pat_memcat_child == "INFORMAL ECONOMY", "INFORMAL",
    pat_memcat_child == "HOUSEHOLD HELP/KASAMBAHAY", "FORMAL",
    pat_memcat_child == "FOREIGN NATIONAL", "INFORMAL",
    pat_memcat_child == "FILIPINOS WITH DUAL CITIZENSHIP / LIVING ABROAD",
    "INFORMAL",
    pat_memcat_child == "SELF EARNING INDIVIDUAL", "INFORMAL",
    pat_memcat_child == "FAMILY DRIVER", "FORMAL"
  )
  unknown_children <- setdiff(
    pat_memcat_child[!is.na(pat_memcat_child)],
    known_children
  )
  list(remapped = remapped_memcat_child, unmapped = unknown_children)
}

remap_disposition <- function(clin_discharge) {
  known_dispositions <- c(
    "IMPROVED", "RECOVERED", "HOME/DISCHARGED AGAINST MEDICAL ADVICE",
    "ABSCONDED", "TRANSFERRED/REFERRED", "EXPIRED", "UNDEFINED"
  )
  remapped_discharge <- fcase(
    clin_discharge == "IMPROVED", 1L,
    clin_discharge == "RECOVERED", 1L,
    clin_discharge == "HOME/DISCHARGED AGAINST MEDICAL ADVICE", 2L,
    clin_discharge == "ABSCONDED", 3L,
    clin_discharge == "TRANSFERRED/REFERRED", 4L,
    clin_discharge == "EXPIRED", 9L,
    clin_discharge == "UNDEFINED", NA_integer_
  )
  unknown_dispositions <- setdiff(
    clin_discharge[!is.na(clin_discharge)],
    known_dispositions
  )
  list(remapped = remapped_discharge, unmapped = unknown_dispositions)
}


format_large_numbers <- function(x) {
  if (x >= 1e9) {
    return(sprintf("%.1fb", x / 1e9))
  } else if (x >= 1e6) {
    return(sprintf("%.1fm", x / 1e6))
  } else if (x >= 1e3) {
    return(sprintf("%.1fk", x / 1e3))
  } else {
    return(as.character(x))
  }
}

combine_comparison_tables <- function(
    summaries, comparison_field, rows_to_show = 10) {
  comparison_list <- lapply(
    summaries, function(summary) summary[[comparison_field]]
  )
  combined_comparison <- rbindlist(comparison_list)

  if (nrow(combined_comparison) == 0) {
    return(
      data.table(
        old_code = character(),
        new_code = character(),
        count = integer()
      )
    )
  }

  combined_comparison <- combined_comparison[
    , .(count = .N),
    by = .(
      old_code = get(names(combined_comparison)[1]),
      new_code = get(names(combined_comparison)[2])
    )
  ]
  combined_comparison <- combined_comparison[order(-count)]
  combined_comparison <- head(combined_comparison, rows_to_show)

  return(combined_comparison)
}

combine_discarded_rvs_tables <- function(summaries, field, rows_to_show = 10) {
  discarded_list <- lapply(summaries, function(summary) summary[[field]])
  combined_discarded <- rbindlist(discarded_list, fill = TRUE)

  if (nrow(combined_discarded) == 0) {
    return(data.table(CODE = character(), count = integer()))
  }

  combined_discarded <- combined_discarded[, .(count = sum(N)), by = CODE]
  combined_discarded <- combined_discarded[order(-count)]
  combined_discarded <- head(combined_discarded, rows_to_show)

  return(combined_discarded)
}

combine_replace_empty_tables <- function(summaries, field, rows_to_show = 10) {
  replace_empty_list <- lapply(summaries, function(summary) summary[[field]])
  combined_replace_empty <- rbindlist(replace_empty_list, fill = TRUE)

  if (nrow(combined_replace_empty) == 0) {
    return(data.table(Column = character(), Empty_Replaced = integer(), NA_Replaced = integer(), Character0_Replaced = integer()))
  }

  combined_replace_empty <- combined_replace_empty[, .(
    Empty_Replaced = sum(Empty_Replaced, na.rm = TRUE),
    NA_Replaced = sum(NA_Replaced, na.rm = TRUE),
    Character0_Replaced = sum(Character0_Replaced, na.rm = TRUE)
  ), by = Column]
  combined_replace_empty <- combined_replace_empty[order(-Empty_Replaced, -NA_Replaced, -Character0_Replaced)]
  combined_replace_empty <- head(combined_replace_empty, rows_to_show)

  return(combined_replace_empty)
}
