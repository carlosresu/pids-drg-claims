process_chunk <- function(chunk,
                          yr_to_load = year_to_load,
                          col_maps = column_mappings,
                          known_vals = known_values,
                          remap_cols = remapped_column,
                          avail_cols = available_columns) {
  # Rename Columns
  setnames(chunk,
    old = avail_cols[avail_cols %in% names(col_maps)],
    new = sapply(
      avail_cols[avail_cols %in% names(col_maps)],
      function(col) col_maps[[col]]
    )
  )
  # Check Rename Success
  # renamesuccess <- all(sapply(
  #   avail_cols[avail_cols %in% names(col_maps)],
  #   function(col) col_maps[[col]]
  # ) %in% colnames(chunk))

  if (!"id_year" %in% colnames(chunk)) {
    chunk[, id_year := as.integer(yr_to_load)]
  }

  # Trim whitespace for id columns
  chunk[, `:=`(
    id_series = trimws(id_series),
    id_pin = trimws(id_pin)
  )]

  # Make a copy of clin_c1 and clin_c2 for later use

  chunk[, `:=`(
    c1 = clin_c1,
    c2 = clin_c2
  )]

  # Handle decimal seconds for 2022 and 2023 data
  chunk[, (c("time_adm", "time_dis")) := lapply(.SD, function(col) {
    ifelse(
      grepl("AM|PM", col),
      format(as.POSIXct(sub("\\.\\d+ ", " ", col), format = "%m/%d/%Y %I:%M:%S %p"), "%H:%M"),
      col
    )
  }), .SDcols = c("time_adm", "time_dis")]

  # Collapse icd rvs columns
  cols_to_extract <- grep("^(clin_icd\\d+|clin_rvs\\d+)$", names(chunk), value = TRUE)
  col_list <- collapse_and_clean_icd_rvs_cols(
    lapply(cols_to_extract[grepl("^clin_icd", cols_to_extract)], function(col) chunk[[col]]),
    lapply(cols_to_extract[grepl("^clin_rvs", cols_to_extract)], function(col) chunk[[col]])
  )
  chunk[, `:=`(clin_icd = col_list$clin_icd, clin_rvs = col_list$clin_rvs)]
  chunk[, (grep("^(clin_icd\\d+|clin_rvs\\d+)$", names(chunk), value = TRUE)) := NULL]

  # Clean c1 and c2 columns
  c1_result <- clean_column(chunk$c1)
  chunk[, `:=`(c1_orig = c1, c1 = c1_result$cleaned_col)]

  # c1_cleaning_comparison <- data.table(
  #   old_code = sapply(chunk$c1_orig, toString),
  #   new_code = sapply(chunk$c1, toString)
  # )[
  #   remove_periods_and_whitespaces(old_code) != remove_periods_and_whitespaces(new_code),
  #   .(old_code, new_code, count = .N),
  #   by = .(old_code, new_code)
  # ]
  c2_result <- clean_column(chunk$c2)
  chunk[, `:=`(c2_orig = c2, c2 = c2_result$cleaned_col)]

  # c2_cleaning_comparison <- data.table(
  #   old_code = sapply(chunk$c2_orig, toString),
  #   new_code = sapply(chunk$c2, toString)
  # )[
  #   remove_periods_and_whitespaces(old_code) != remove_periods_and_whitespaces(new_code),
  #   .(old_code, new_code, count = .N),
  #   by = .(old_code, new_code)
  # ]

  # Apply is_covid boolean
  is_covid_c1 <- c1_result$is_covid
  is_covid_c2 <- c2_result$is_covid
  chunk[, is_covid := (is_covid_c1 | is_covid_c2)]

  # prepare icds for mapping
  chunk[, `:=`(
    c1 = lapply(c1, prep_icd_for_mapping),
    c2 = lapply(c2, prep_icd_for_mapping)
  )]

  chunk[, clin_icd := lapply(seq_len(.N), function(i) {
    clin_icd_list <- c(manual_replacement(clin_icd[[i]]), c1[[i]], c2[[i]])
    return(flatten_then_check_null_na(clin_icd_list))
  })]

  # Replace empty with NA, then replace with character(0)
  replace_result <- replace_na_or_empty(
    dt = chunk, replace_with = "NA_character_"
  )
  chunk <- replace_result$return_data
  # empty_replaced_with_na_1 <- replace_result$return_replacement_summary
  replace_empty_result_1 <- replace_na_or_empty(
    dt = chunk, replace_with = "character(0)"
  )
  chunk <- replace_empty_result_1$return_data
  # NA_replaced_with_empty_1 <- replace_empty_result_1$return_replacement_summary


  # Move rvs icd rvs codes to proper columns
  # Process c1
  c1_results <- append_copy_and_remove_icd_rvs(
    chunk$c1, chunk$clin_rvs, chunk$clin_icd
  )
  chunk[, `:=`(
    clin_rvs = c1_results$clin_rvs,
    c1 = c1_results$col,
    clin_icd = c1_results$clin_icd
  )]
  # c1_discarded_rvs <- c1_results$discarded_rvs

  # Process c2
  c2_results <- append_copy_and_remove_icd_rvs(
    chunk$c2, chunk$clin_rvs, chunk$clin_icd
  )
  chunk[, `:=`(
    clin_rvs = c2_results$clin_rvs,
    c2 = c2_results$col,
    clin_icd = c2_results$clin_icd
  )]
  # c2_discarded_rvs <- c2_results$discarded_rvs


  # Replace empty with NA, then replace with character(0)
  replace_result <- replace_na_or_empty(
    dt = chunk, replace_with = "NA_character_"
  )
  chunk <- replace_result$return_data
  # empty_replaced_with_na_1 <- replace_result$return_replacement_summary
  replace_empty_result_1 <- replace_na_or_empty(
    dt = chunk, replace_with = "character(0)"
  )
  chunk <- replace_empty_result_1$return_data
  # NA_replaced_with_empty_1 <- replace_empty_result_1$return_replacement_summary

  # Remap patient data
  remap_res <- remap_patient_data(
    pat_type = chunk$pat_type,
    pat_memcat_parent = chunk$pat_memcat_parent,
    pat_memcat_child = chunk$pat_memcat_child,
    clin_discharge = chunk$clin_discharge,
    claim_status = chunk$claim_status,
    known_values = known_vals,
    remapped_column = remap_cols
  )

  chunk[, `:=`(
    pat_type = remap_res$pat_type,
    pat_memcat_parent = remap_res$pat_memcat_parent,
    pat_memcat_child = remap_res$pat_memcat_child,
    clin_discharge = remap_res$clin_discharge,
    claim_status = remap_res$claim_status
  )]

  # Map RVS codes
  chunk[, icd9_list := map_rvs_icd9(clin_rvs)$icd9_list]

  # Prepare codes for mapping
  chunk[, `:=`(
    c1 = lapply(
      c1, function(x) if (is.null(x) || all(is.na(x))) character(0) else x
    ),
    c2 = lapply(
      c2, function(x) if (is.null(x) || all(is.na(x))) character(0) else x
    )
  )]


  # Map ICD 10 codes
  chunk[, `:=`(
    c1 = map_icd10(c1),
    c2 = map_icd10(c2),
    clin_icd = map_icd10(clin_icd)
  )]

  # Replace na/empty with character(0)
  replace_empty_result_2 <- replace_na_or_empty(
    dt = chunk, replace_with = "character(0)"
  )
  chunk <- replace_empty_result_2$return_data
  # NA_replaced_with_empty_2 <- replace_empty_result_2$return_replacement_summary

  # Find pdx
  pdx_inputs <- prep_pdx_inputs(
    # inputs to prep (the below have been extracted above)
    chunk$c1, chunk$c2, chunk$clin_icd,
    # dependencies to prep (the below are global variables)
    acc_pdx, neoplasms_dt_actual, acr_rvs, covid_rvs
  )
  pdx_result <- find_pdx(
    # inputs to find pdx for
    pdx_inputs$c1, pdx_inputs$c2, pdx_inputs$clin_icd,
    # parameters
    global_seed
  )
  chunk[, c("pdx", "pdx_code") := .(pdx_result$pdx, pdx_result$pdx_code)]
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

  # Restore clin_c1 and clin_c2
  chunk[, clin_c1 := c1_orig]
  chunk[, clin_c2 := c2_orig]
  chunk[, c("c1_orig", "c2_orig") := NULL]

  # Save rvs mappings to clin_proc
  chunk[, clin_proc := icd9_list]
  chunk[, icd9_list := NULL]

  # Save icd mappings to clin_sdx
  chunk[, clin_icd := Map(function(pdx_var, sdx_var) {
    sdx_var[sdx_var != pdx_var]
  }, pdx, clin_icd)]
  chunk[, clin_sdx := clin_icd]
  chunk[, clin_icd := NULL]

  # Safeguard: If pat_bdate doesn't exist, set it to NA_Date_
  if (!"pat_bdate" %in% colnames(chunk)) {
    chunk[, pat_bdate := NA_Date_]
  }

  # Format date columns and check for dates before 1900-01-01
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

  # Format time columns
  time_cols <- c("time_adm", "time_dis")
  chunk[, (time_cols) := lapply(
    .SD,
    function(x) {
      x <- ifelse(is.na(x), "00:00:00", paste0(x, ":00"))
      as.character(x)
    }
  ), .SDcols = time_cols]

  # Set time to UTC so that datetime timestamp matches
  # up with time_adm and time_dis for groupings
  chunk[, date_adm := as.POSIXct(
    paste(date_adm, time_adm),
    format = "%Y-%m-%d %H:%M:%S", tz = "UTC"
  )]
  chunk[, date_dis := as.POSIXct(
    paste(date_dis, time_dis),
    format = "%Y-%m-%d %H:%M:%S", tz = "UTC"
  )]

  # Format clin_outpatient and clin_emergency
  chunk[, clin_outpatient := as.logical(as.integer(clin_outpatient))]
  chunk[, clin_emergency := as.logical(as.integer(clin_emergency))]

  # Safeguard: If pat_bwt doesn't exist, set it to NA_real_
  if (!"pat_bwt" %in% colnames(chunk)) {
    chunk[, pat_bwt := NA_real_]
  }

  # Format numeric columns as numeric
  num_cols <- c(
    "pat_age", "pat_bwt", "clin_discharge", "claim_payout",
    "claim_charge", "id_year", "pdx_code"
  )
  chunk[, (num_cols) := lapply(.SD, as.numeric), .SDcols = num_cols]

  # Format integer columns as integer
  int_cols <- c("clin_discharge", "id_year", "pdx_code")
  chunk[, (int_cols) := lapply(.SD, as.integer), .SDcols = int_cols]

  # Format character columns as character
  char_cols <- c(
    "id_hcp", "pat_type", "clin_acc", "pat_rel", "pat_sex",
    "pat_memcat_parent", "pat_memcat_child", "claim_status", "pdx"
  )

  chunk[, (char_cols) := lapply(.SD, as.character), .SDcols = char_cols]
  char_cols <- names(chunk)[sapply(chunk, is.character)]

  # Initialize pat_ageday to NA_integer_
  chunk[, pat_ageday := NA_integer_]

  # Subset clin_sdx to valid icd codes only
  chunk[, clin_sdx := lapply(clin_sdx, function(codes) {
    valid_codes <- codes[codes %chin% acc_icd_set]
    if (length(valid_codes) > 0) {
      return(valid_codes)
    } else {
      return(NA_character_)
    }
  })]
  chunk[, clin_sdx := lapply(
    clin_sdx, function(x) if (is.null(x)) character(0) else unlist(x)
  )]

  # Replace with character(0)
  replace_empty_result_3 <- replace_na_or_empty(
    dt = chunk, replace_with = "character(0)",
    additional_columns = c("c1", "c2", "pdx")
  )
  chunk <- replace_empty_result_3$return_data
  # NA_replaced_with_empty_3 <- replace_empty_result_3$return_replacement_summary

  # Convert char cols to UTF-8, then replace empty with NA_character_
  chunk[, (char_cols) := lapply(.SD, function(col) {
    iconv(col, from = "", to = "UTF-8")
  }), .SDcols = char_cols]
  chunk[, (char_cols) := lapply(.SD, function(col) {
    col[col %chin% c("None", "")] <- NA_character_
    return(col)
  }), .SDcols = char_cols]

  # Replace nan with NA_real_
  num_cols <- names(chunk)[sapply(chunk, is.numeric)]
  chunk[, (num_cols) := lapply(.SD, function(col) {
    col[is.nan(col)] <- NA_real_
    return(col)
  }), .SDcols = num_cols]


  # Split Id_hcp then replace empty with character(0)
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

  # Rename pdx code columns
  setnames(chunk, c("pdx", "pdx_code"), c("clin_pdx", "clin_pdx_source"))

  # Set final column order
  setcolorder(chunk, c(
    "id_year", "id_series", "id_pin", "id_hci", "id_hcp", "date_adm",
    "time_adm", "date_dis", "time_dis", "date_rec", "date_ref",
    "date_check", "date_ext", "pat_type", "pat_rel", "pat_bdate", "pat_age",
    "pat_ageday", "pat_sex", "pat_bwt", "pat_memcat_parent",
    "pat_memcat_child", "claim_status", "claim_payout",
    "claim_charge", "is_covid", "clin_discharge", "clin_outpatient",
    "clin_emergency", "clin_acc", "clin_c1", "c1", "clin_c2", "c2",
    "clin_sdx", "clin_proc", "clin_rvs", "clin_pdx", "clin_pdx_source"
  ))

  # Set clin_discharge as integer
  chunk[, clin_discharge := as.integer(clin_discharge)]

  # Chop off excess clin_sdx and clin_proc from the RIGHT hand side (i.e. least priority ones)
  chunk[, clin_sdx := lapply(clin_sdx, function(x) head(x, 12))]
  chunk[, clin_proc := lapply(clin_proc, function(x) head(x, 20))]

  # Set pat_age to 0 for specific cases

  chunk[
    !is.na(pat_bdate) & !is.na(date_adm),
    pat_age := floor(as.numeric(as.Date(date_adm) - pat_bdate) / 365.25)
  ]

  chunk[
    !is.na(pat_bdate) & !is.na(date_adm) & !is.na(pat_age) & pat_bdate > as.Date(date_adm),
    pat_bdate := NA_Date_
  ]

  chunk[
    grepl("99432", c1) & !is.na(pat_age) & pat_age < 0 & pat_age >= -1,
    pat_age := 0
  ]

  chunk[
    !is.na(pat_age) & pat_age > 0 & pat_age <= 124,
    pat_age := floor(pat_age)
  ]

  chunk[
    !is.na(pat_age) & (pat_age < 0 | pat_age > 124),
    pat_age := NA_integer_
  ]
  # chunk_summary <- list(
  #   rename_success = renamesuccess,
  #   ICD_replacements_1 = c1_cleaning_comparison,
  #   ICD_replacements_2 = c2_cleaning_comparison,
  #   pat_type_mapped = remap_res$pat_type_mapped,
  #   pat_memcat_parent_mapped = remap_res$pat_memcat_parent_mapped,
  #   pat_memcat_child_mapped = remap_res$pat_memcat_child_mapped,
  #   clin_discharge_mapped = remap_res$clin_discharge_mapped,
  #   claim_status_mapped = remap_res$claim_status_mapped,
  #   pat_type_unmapped = remap_res$pat_type_unmapped,
  #   memcat_parent_unmapped = remap_res$memcat_parent_unmapped,
  #   memcat_child_unmapped = remap_res$memcat_child_unmapped,
  #   discharge_unmapped = remap_res$discharge_unmapped,
  #   claim_status_unmapped = remap_res$claim_status_unmapped,
  #   discard_rvs_one = c1_discarded_rvs,
  #   discard_rvs_two = c2_discarded_rvs,
  #   empty_replaced_with_na_1 = empty_replaced_with_na_1,
  #   NA_replaced_with_empty_1 = NA_replaced_with_empty_1,
  #   NA_replaced_with_empty_2 = NA_replaced_with_empty_2,
  #   NA_replaced_with_empty_3 = NA_replaced_with_empty_3,
  #   rvss = rvs_mapping_result$rvss,
  #   mappable_rvs = rvs_mapping_result$mappable_rvs,
  #   unmappable_rvs = rvs_mapping_result$unmappable_rvs,
  #   multi_mapped_rvs = rvs_mapping_result$multi_mapped_rvs,
  #   without_drg = rvs_mapping_result$without_drg
  # )
  invisible(gc())
  return(
    # list(
    # return_chunk =
    chunk
    # , return_summary = chunk_summary
    # )
  )
}
