process_chunk <- function(chunk,
                          yr_to_load = year_to_load,
                          col_maps = column_mappings,
                          known_vals = known_values,
                          remap_cols = remapped_column,
                          avail_cols = available_columns) {
  saveWidget(profvis({
    # chunk_summary <- list()
    setnames(chunk,
      old = avail_cols[avail_cols %in% names(col_maps)],
      new = sapply(
        avail_cols[avail_cols %in% names(col_maps)],
        function(col) col_maps[[col]]
      )
    )
    renamesuccess <- all(sapply(
      avail_cols[avail_cols %in% names(col_maps)],
      function(col) col_maps[[col]]
    ) %in% colnames(chunk))
    if (!"id_year" %in% colnames(chunk)) {
      chunk[, id_year := as.integer(yr_to_load)]
    }
    chunk[, `:=`(
      id_series = trimws(id_series),
      id_pin = trimws(id_pin)
    )]
    chunk[, `:=`(
      c1 = clin_c1,
      c2 = clin_c2
    )]
    chunk[, (c("time_adm", "time_dis")) := lapply(.SD, function(col) {
      ifelse(
        grepl("AM|PM", col),
        format(as.POSIXct(sub("\\.\\d+ ", " ", col), format = "%m/%d/%Y %I:%M:%S %p"), "%H:%M"),
        col
      )
    }), .SDcols = c("time_adm", "time_dis")]
    cols_to_extract <- grep("^(clin_icd\\d+|clin_rvs\\d+)$", names(chunk), value = TRUE)
    col_list <- collapse_and_clean_icd_rvs_cols(
      lapply(cols_to_extract[grepl("^clin_icd", cols_to_extract)], function(col) chunk[[col]]),
      lapply(cols_to_extract[grepl("^clin_rvs", cols_to_extract)], function(col) chunk[[col]])
    )
    chunk[, `:=`(clin_icd = col_list$clin_icd, clin_rvs = col_list$clin_rvs)]
    chunk[, (grep("^(clin_icd\\d+|clin_rvs\\d+)$", names(chunk), value = TRUE)) := NULL]
    # cat("After collapsing multiple columns into onef\n")
    # str(chunk$clin_icd[1:10])
    c1_result <- clean_column(chunk$c1)
    chunk[, `:=`(c1_orig = c1, c1 = c1_result$cleaned_col)]
    is_covid_c1 <- c1_result$is_covid
    c1_cleaning_comparison <- data.table(
      old_code = sapply(chunk$c1_orig, toString),
      new_code = sapply(chunk$c1, toString)
    )[
      remove_periods_and_whitespaces(old_code) != remove_periods_and_whitespaces(new_code),
      .(old_code, new_code, count = .N),
      by = .(old_code, new_code)
    ]
    c2_result <- clean_column(chunk$c2)
    chunk[, `:=`(c2_orig = c2, c2 = c2_result$cleaned_col)]
    is_covid_c2 <- c2_result$is_covid
    c2_cleaning_comparison <- data.table(
      old_code = sapply(chunk$c2_orig, toString),
      new_code = sapply(chunk$c2, toString)
    )[
      remove_periods_and_whitespaces(old_code) != remove_periods_and_whitespaces(new_code),
      .(old_code, new_code, count = .N),
      by = .(old_code, new_code)
    ]
    chunk[, is_covid := (is_covid_c1 | is_covid_c2)]
    # OLD CODE:
    # chunk[, c1 := split_to_vector(remove_lumped_icd_codes(lapply(c1, manual_replacement)))]
    # chunk[, c2 := split_to_vector(remove_lumped_icd_codes(lapply(c2, manual_replacement)))]
    # REFACTORED CODE:
    # cat("Processing c1\n")
    chunk[, c1 := lapply(c1, function(text) {
      replaced_text <- manual_replacement(text)
      collapsed_text <- collapse_to_string(replaced_text)
      split_result <- split_to_vector(collapsed_text)
      cleaned_result <- remove_lumped_icd_codes(split_result)
      return(flatten_and_clean(cleaned_result))
    })]
    # cat("Processing c2\n")
    chunk[, c2 := lapply(c2, function(text) {
      replaced_text <- manual_replacement(text)
      collapsed_text <- collapse_to_string(replaced_text)
      split_result <- split_to_vector(collapsed_text)
      cleaned_result <- remove_lumped_icd_codes(split_result)
      return(flatten_and_clean(cleaned_result))
    })]
    # cat("Processing clin_icd\n")
    chunk[, clin_icd := lapply(seq_len(.N), function(i) {
      clin_icd_list <- c(manual_replacement(clin_icd[[i]]), c1[[i]], c2[[i]])
      return(flatten_and_clean(clin_icd_list))
    })]
    # cat("after flatten, clean, and manual replacement\n")
    # str(chunk$clin_icd[1:10])
    replace_result <- replace_na_or_empty(dt = chunk, replace_with = "NA_character_")
    chunk <- replace_result$return_data
    empty_replaced_with_na_1 <- replace_result$return_replacement_summary
    replace_empty_result_1 <- replace_na_or_empty(dt = chunk, replace_with = "character(0)")
    chunk <- replace_empty_result_1$return_data
    NA_replaced_with_empty_1 <- replace_empty_result_1$return_replacement_summary

    # # Print description and each element of chunk$c1
    # cat("Contents of c1:\n")
    # lapply(chunk$c1, function(x) if (length(x) > 0) cat(x, "\n"))

    # # Repeat for each section
    # cat("\nContents of c2:\n")
    # lapply(chunk$c2, function(x) if (length(x) > 0) cat(x, "\n"))

    # cat("\nContents of clin_icd after replace with NA and replace with empty:\n")
    # lapply(chunk$clin_icd, function(x) if (length(x) > 0) cat(x, "\n"))

    # cat("\nContents of clin_rvs:\n")
    # lapply(chunk$clin_rvs, function(x) if (length(x) > 0) cat(x, "\n"))

    c1_results <- append_copy_and_remove_icd_rvs(chunk$c1, chunk$clin_rvs, chunk$clin_icd)
    chunk[, clin_rvs := c1_results$clin_rvs]
    chunk[, c1 := c1_results$col]
    chunk[, clin_icd := c1_results$clin_icd]
    c1_discarded_rvs <- c1_results$discarded_rvs
    # cat("\nContents of clin_icd after append copy and remove icd rvs 1:\n")
    # lapply(chunk$clin_icd, function(x) if (length(x) > 0) cat(x, "\n"))
    c2_results <- append_copy_and_remove_icd_rvs(chunk$c2, chunk$clin_rvs, chunk$clin_icd)
    chunk[, clin_rvs := c2_results$clin_rvs]
    chunk[, c2 := c2_results$col]
    chunk[, clin_icd := c2_results$clin_icd]
    c2_discarded_rvs <- c2_results$discarded_rvs
    # cat("\nContents of clin_icd after append copy and remove icd rvs 2:\n")
    # lapply(chunk$clin_icd, function(x) if (length(x) > 0) cat(x, "\n"))

    replace_result <- replace_na_or_empty(dt = chunk, replace_with = "NA_character_")
    chunk <- replace_result$return_data
    empty_replaced_with_na_1 <- replace_result$return_replacement_summary
    replace_empty_result_1 <- replace_na_or_empty(dt = chunk, replace_with = "character(0)")
    chunk <- replace_empty_result_1$return_data
    NA_replaced_with_empty_1 <- replace_empty_result_1$return_replacement_summary

    # # Print description and each element of chunk$c1
    # cat("Contents of c1:\n")
    # lapply(chunk$c1, function(x) if (length(x) > 0) cat(x, "\n"))

    # # Repeat for each section
    # cat("\nContents of c2:\n")
    # lapply(chunk$c2, function(x) if (length(x) > 0) cat(x, "\n"))

    # cat("\nContents of clin_icd after replace with NA and replace with empty:\n")
    # lapply(chunk$clin_icd, function(x) if (length(x) > 0) cat(x, "\n"))

    # cat("\nContents of clin_rvs:\n")
    # lapply(chunk$clin_rvs, function(x) if (length(x) > 0) cat(x, "\n"))

    remap_res <- remap_patient_data(
      pat_type = chunk$pat_type,
      pat_memcat_parent = chunk$pat_memcat_parent,
      pat_memcat_child = chunk$pat_memcat_child,
      clin_discharge = chunk$clin_discharge,
      claim_status = chunk$claim_status,
      known_values = known_vals,
      remapped_column = remap_cols
    )
    chunk[, pat_type := remap_res$remapped$pat_type]
    chunk[, pat_memcat_parent := remap_res$remapped$pat_memcat_parent]
    chunk[, pat_memcat_child := remap_res$remapped$pat_memcat_child]
    chunk[, clin_discharge := remap_res$remapped$clin_discharge]
    chunk[, claim_status := remap_res$remapped$claim_status]

    # print(unique(unlist(chunk$clin_rvs)))

    rvs_mapping_result <- map_rvs_icd9(chunk$clin_rvs)
    chunk[, icd9_list := rvs_mapping_result$icd9_list]
    # str(chunk$clin_icd[1:10])
    modified_c1 <- lapply(chunk$c1, function(x) if (is.null(x) || all(is.na(x))) character(0) else x)
    modified_c2 <- lapply(chunk$c2, function(x) if (is.null(x) || all(is.na(x))) character(0) else x)
    chunk[, c1 := modified_c1]
    chunk[, c2 := modified_c2]
    c1 <- chunk$c1
    c2 <- chunk$c2
    clin_icd <- chunk$clin_icd
    # cat("Before map_icd10 function\n")
    # str(chunk$clin_icd[1:10])
    icd10_mapping_result <- map_icd10(c1, c2, clin_icd)
    chunk[, c1 := icd10_mapping_result$c1]
    chunk[, c2 := icd10_mapping_result$c2]
    chunk[, clin_icd := icd10_mapping_result$clin_icd]
    # cat("After map_icd10 function\n")
    # str(chunk$clin_icd[1:10])
    replace_empty_result_2 <- replace_na_or_empty(dt = chunk, replace_with = "character(0)")
    chunk <- replace_empty_result_2$return_data
    NA_replaced_with_empty_2 <- replace_empty_result_2$return_replacement_summary
    # cat("After replace with empty\n")
    # str(chunk$clin_icd[1:10])
    chunk[, c1 := lapply(c1, remove_whitespace)]
    chunk[, c2 := lapply(c2, remove_whitespace)]
    chunk[, clin_icd := lapply(clin_icd, remove_whitespace)]
    # cat("After remove whitespace\n")
    # str(chunk$clin_icd[1:10])
    pdx_result <- find_pdx(chunk$c1, chunk$c2, chunk$clin_icd)
    chunk[, pdx := pdx_result$pdx]
    chunk[, pdx_code := pdx_result$pdx_code]
    chunk[, c1 := lapply(seq_len(.N), function(i) {
      lst <- c1[[i]]
      pdx_val <- pdx[i]
      if (!is.na(pdx_val)) {
        lst <- setdiff(lst, pdx_val)
      }
      as.character(lst)
    })]
    chunk[, c2 := lapply(seq_len(.N), function(i) {
      lst <- c2[[i]]
      pdx_val <- pdx[i]
      if (!is.na(pdx_val)) {
        lst <- setdiff(lst, pdx_val)
      }
      as.character(lst)
    })]
    chunk[, clin_icd := lapply(seq_len(.N), function(i) {
      lst <- clin_icd[[i]]
      pdx_val <- pdx[i]
      if (!is.na(pdx_val)) {
        lst <- setdiff(lst, pdx_val)
      }
      as.character(lst)
    })]
    # cat("After removing pdx\n")
    # str(chunk$clin_icd[1:10])
    invalid_age_before <- nrow(chunk[pat_age < -1 | pat_age > 124, .(id_series)])
    fwrite(
      chunk[pat_age <= -1, .(id_series, pat_age, c1, c2)],
      here(debug_path, "age_less_than_or_equal_to_neg_one.csv")
    )
    fwrite(
      chunk[pat_age < 0 & pat_age > -1, .(id_series, pat_age, c1, c2)],
      here(debug_path, "age_between_zero_and_neg_one.csv")
    )
    chunk[, clin_c1 := c1_orig]
    chunk[, clin_c2 := c2_orig]
    chunk[, c("c1_orig", "c2_orig") := NULL]
    chunk[, clin_proc := icd9_list]
    chunk[, icd9_list := NULL]
    chunk[, clin_icd := Map(function(pdx_var, sdx_var) {
      sdx_var[sdx_var != pdx_var]
    }, pdx, clin_icd)]
    chunk[, clin_sdx := clin_icd]
    chunk[, clin_icd := NULL]
    if (!"pat_bdate" %in% colnames(chunk)) {
      chunk[, pat_bdate := NA_Date_]
    }
    date_cols <- c(
      "date_adm", "date_dis", "date_rec", "date_ref",
      "date_check", "pat_bdate", "date_ext"
    )
    chunk[, (date_cols) := lapply(
      .SD,
      function(x) {
        converted_dates <- as.Date(x, format = "%m/%d/%Y")
        converted_dates[converted_dates < as.Date("1900-01-01")] <- NA_Date_
        return(converted_dates)
      }
    ), .SDcols = date_cols]
    time_cols <- c("time_adm", "time_dis")
    chunk[, (time_cols) := lapply(
      .SD,
      function(x) {
        x <- ifelse(is.na(x), "00:00:00", paste0(x, ":00"))
        as.character(x)
      }
    ), .SDcols = time_cols]
    chunk[, date_adm := as.POSIXct(
      paste(date_adm, time_adm),
      format = "%Y-%m-%d %H:%M:%S", tz = "Asia/Manila"
    )]
    chunk[, date_dis := as.POSIXct(
      paste(date_dis, time_dis),
      format = "%Y-%m-%d %H:%M:%S", tz = "Asia/Manila"
    )]
    chunk[, clin_outpatient := as.logical(as.integer(clin_outpatient))]
    chunk[, clin_emergency := as.logical(as.integer(clin_emergency))]
    if (!"pat_bwt" %in% colnames(chunk)) {
      chunk[, pat_bwt := NA_real_]
    }
    num_cols <- c(
      "pat_age", "pat_bwt", "clin_discharge", "claim_payout",
      "claim_charge", "id_year", "pdx_code"
    )
    chunk[, (num_cols) := lapply(.SD, as.numeric), .SDcols = num_cols]
    int_cols <- c("clin_discharge", "id_year", "pdx_code")
    chunk[, (int_cols) := lapply(.SD, as.integer), .SDcols = int_cols]
    char_cols <- c(
      "id_hcp", "pat_type", "clin_acc", "pat_rel", "pat_sex",
      "pat_memcat_parent", "pat_memcat_child", "claim_status", "pdx"
    )
    chunk[, (char_cols) := lapply(.SD, as.character), .SDcols = char_cols]
    chunk[, pat_ageday := NA_integer_]
    invalid_ages_before_correction <- chunk[pat_age < 0 | pat_age > 124, .N]
    invalid_age_ids_before <- chunk[pat_age < 0 | pat_age > 124, id_series]
    chunk[!is.na(pat_age) & pat_age > 0, pat_age := floor(pat_age)]
    chunk[pat_age < 0 & pat_age >= -1, pat_age := 0]
    chunk[pat_age < -1 | pat_age > 124, pat_age := NA_integer_]
    recalculated_rows <- chunk[
      !is.na(pat_bdate) & !is.na(pat_age) &
        pat_age != floor(as.numeric(as.Date(date_adm) - pat_bdate) / 365.25)
    ]
    chunk[
      !is.na(pat_bdate) & !is.na(pat_age) &
        pat_age != floor(as.numeric(as.Date(date_adm) - pat_bdate) / 365.25),
      pat_age_recalculated := floor(as.numeric(as.Date(date_adm) - pat_bdate) / 365.25)
    ]
    chunk[
      !is.na(pat_age_recalculated) & pat_age_recalculated > 0,
      pat_age := pat_age_recalculated
    ]
    chunk[, pat_age_recalculated := NULL]
    invalid_bdate_before <- chunk[is.na(pat_bdate), .N]
    invalid_bdate_ids_before <- chunk[is.na(pat_bdate), id_series]
    invalid_age_path <- here("data-cleaning", "debug", "invalid_age.csv")
    fwrite(data.table(id_series = invalid_age_ids_before), invalid_age_path)
    invalid_bdate_path <- here("data-cleaning", "debug", "invalid_bdate.csv")
    fwrite(data.table(id_series = invalid_bdate_ids_before), invalid_bdate_path)
    invalid_ages_after_correction <- chunk[pat_age < 0 | pat_age > 124, .N]
    chunk[, clin_sdx := lapply(clin_sdx, function(codes) {
      valid_codes <- codes[codes %chin% acc_icd_set]
      if (length(valid_codes) > 0) {
        return(valid_codes)
      } else {
        return(NA_character_)
      }
    })]
    chunk[, clin_sdx := lapply(clin_sdx, function(x) if (is.null(x)) character(0) else unlist(x))]
    # cat("After unlisting or replacing with character(0)\n")
    # str(chunk$clin_sdx[1:10])
    replace_empty_result_3 <- replace_na_or_empty(dt = chunk, replace_with = "character(0)", additional_columns = c("c1", "c2", "pdx"))
    chunk <- replace_empty_result_3$return_data
    NA_replaced_with_empty_3 <- replace_empty_result_3$return_replacement_summary
    # cat("After replacing with empty\n")
    # str(chunk$clin_sdx[1:10])
    chunk[, (char_cols) := lapply(.SD, function(col) iconv(col, from = "", to = "UTF-8")), .SDcols = char_cols]
    char_cols <- names(chunk)[sapply(chunk, is.character)]
    chunk[, (char_cols) := lapply(.SD, function(col) {
      col[col %chin% c("None", "")] <- NA_character_
      return(col)
    }), .SDcols = char_cols]
    num_cols <- names(chunk)[sapply(chunk, is.numeric)]
    chunk[, (num_cols) := lapply(.SD, function(col) {
      col[is.nan(col)] <- NA_real_
      return(col)
    }), .SDcols = num_cols]
    # list_cols <- names(chunk)[sapply(chunk, is.list)]
    # chunk[, (list_cols) := lapply(.SD, function(col) {
    #   lapply(col, function(x) {
    #     if (is.character(x)) x[x %in% c("None", "")] <- NA_character_
    #     return(x)
    #   })
    # }), .SDcols = list_cols]
    array_columns <- c("id_hcp")
    split_pattern <- "\\s*,\\s*|\\|\\||\\|"
    chunk[, (array_columns) := lapply(.SD, function(x) {
      x <- strsplit(x, split_pattern)
      lapply(x, function(y) {
        if (length(y) == 0L || all(is.na(y))) {
          character(0)
        } else {
          y
        }
      })
    }), .SDcols = array_columns]
    # list_columns <- c("clin_sdx", "clin_proc", "id_hcp")
    # chunk[, (list_columns) := lapply(.SD, function(col) {
    #   lapply(col, function(x) {
    #     if (is.null(x) || length(x) == 0L || all(is.na(x))) {
    #       character(0)
    #     } else {
    #       x
    #     }
    #   })
    # }), .SDcols = list_columns]
    list_columns <- c("id_hcp")
    chunk[, (list_columns) := lapply(.SD, function(col) {
      lapply(col, function(x) {
        if (is.null(x) || length(x) == 0L || all(is.na(x))) {
          character(0)
        } else {
          x
        }
      })
    }), .SDcols = list_columns]
    setnames(chunk, c("pdx", "pdx_code"), c("clin_pdx", "clin_pdx_source"))
    setcolorder(chunk, c(
      "id_year", "id_series", "id_pin", "id_hci", "id_hcp", "date_adm",
      "time_adm", "date_dis", "time_dis", "date_rec", "date_ref",
      "date_check", "date_ext", "pat_type", "pat_rel", "pat_bdate", "pat_age",
      "pat_ageday", "pat_sex", "pat_bwt", "pat_memcat_parent",
      "pat_memcat_child", "is_covid", "claim_status", "claim_payout",
      "claim_charge", "clin_discharge", "clin_outpatient", "clin_emergency",
      "clin_acc", "clin_c1", "c1", "clin_c2", "c2", "clin_sdx", "clin_proc",
      "clin_rvs", "clin_pdx", "clin_pdx_source"
    ))
    chunk[, clin_discharge := as.integer(clin_discharge)]

    chunk_summary <- list(
      rename_success = renamesuccess,
      ICD_replacements_1 = c1_cleaning_comparison,
      ICD_replacements_2 = c2_cleaning_comparison,
      pat_type_mapped = remap_res$pat_type_mapped,
      pat_memcat_parent_mapped = remap_res$pat_memcat_parent_mapped,
      pat_memcat_child_mapped = remap_res$pat_memcat_child_mapped,
      clin_discharge_mapped = remap_res$clin_discharge_mapped,
      claim_status_mapped = remap_res$claim_status_mapped,
      pat_type_unmapped = remap_res$pat_type_unmapped,
      memcat_parent_unmapped = remap_res$memcat_parent_unmapped,
      memcat_child_unmapped = remap_res$memcat_child_unmapped,
      discharge_unmapped = remap_res$discharge_unmapped,
      claim_status_unmapped = remap_res$claim_status_unmapped,
      discard_rvs_one = c1_discarded_rvs,
      discard_rvs_two = c2_discarded_rvs,
      empty_replaced_with_na_1 = empty_replaced_with_na_1,
      NA_replaced_with_empty_1 = NA_replaced_with_empty_1,
      NA_replaced_with_empty_2 = NA_replaced_with_empty_2,
      NA_replaced_with_empty_3 = NA_replaced_with_empty_3,
      unique_icds = icd10_mapping_result$unique_icds,
      direct_matches = icd10_mapping_result$direct_matches,
      unmatched_codes = icd10_mapping_result$unmatched_codes,
      unmatched_sources = icd10_mapping_result$unmatched_sources,
      icd10_map_dt = icd10_mapping_result$icd10_map_dt,
      modified_matches = icd10_mapping_result$modified_matches,
      rvss = rvs_mapping_result$rvss,
      mappable_rvs = rvs_mapping_result$mappable_rvs,
      unmappable_rvs = rvs_mapping_result$unmappable_rvs,
      multi_mapped_rvs = rvs_mapping_result$multi_mapped_rvs,
      without_drg = rvs_mapping_result$without_drg
    )
  }), profvis_fpath)
  # str(chunk)
  # invisible(gc())
  return(list(
    return_chunk = chunk,
    return_summary = chunk_summary
  ))
}
