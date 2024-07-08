read_entire_file <- function(drop_cols) {
  #' @description Reads the entire claims data file, dropping specified columns.
  #' @param drop_cols A vector of column names to drop.
  #' @return A data.table containing the claims data.
  dt <- fread(full_claims, na.strings = na_values, drop = drop_cols, colClasses = col_classes)
  return(dt)
}

read_sampled_file <- function() {
  #' @description Reads the sampled claims data file.
  #' @return A data.table containing the sampled claims data.
  dt <- fread(sampled_claims, na.strings = na_values, colClasses = col_classes)
  return(dt)
}

sample_data <- function(dt) {
  #' @description Samples a given data.table.
  #' @param dt A data.table to sample from.
  #' @return A sampled data.table.
  dt <- dt[sample(.N, min(sample_size, .N))]
  return(dt)
}

write_data <- function(dt, path) {
  #' @description Writes a data.table to a specified path.
  #' @param dt A data.table to write.
  #' @param path The file path where the data.table should be written.
  fwrite(dt, path)
}

add_year_column <- function(dt, year_to_load) {
  #' @description Adds a year column to a data.table.
  #' @param dt A data.table to modify.
  #' @param year_to_load The year to add as a new column.
  #' @return The modified data.table with the added year column.
  dt[, SRC_YR := as.integer(year_to_load)]
  return(dt)
}

rename_columns <- function(dt) {
  #' @description Renames columns in a data.table.
  #' @param dt A data.table with columns to rename.
  #' @return The modified data.table with renamed columns.
  setnames(dt, old = old_colnames, new = new_colnames)
  return(dt)
}

clean_columns <- function(dt) {
  #' @description Cleans columns in a data.table by converting to UTF-8, removing spaces, and setting NA values.
  #' @param dt A data.table to clean.
  #' @return The cleaned data.table.
  if (!is.data.table(dt)) {
    dt <- as.data.table(dt)
  }
  dt[] <- lapply(dt, as.character)
  dt[] <- lapply(dt, function(col) {
    col <- iconv(col, to = "UTF-8", sub = "byte")
    col <- toupper(col)
    col <- stri_replace_all_regex(col, "[ \n]", "")
    col <- stri_replace_all_regex(col, "[^\\w\\d\\/\\s]+", "")
    col <- stri_trim_both(col)
    col <- ifelse(col %in% na_like_strings, NA_character_, col)
    return(col)
  })
  return(dt)
}

clean_columns_in_dt <- function(dt, cols_to_clean) {
  #' @description Cleans specified columns in a data.table.
  #' @param dt A data.table to clean.
  #' @param cols_to_clean A vector of column names to clean.
  #' @return The cleaned data.table.
  dt[, (cols_to_clean) := clean_columns(.SD), .SDcols = cols_to_clean]
  return(dt)
}

process_and_collapse_columns <- function(dt, cols_to_process, new_col_name) {
  #' @description Processes and collapses specified columns in a data.table into a new column.
  #' @param dt A data.table to process.
  #' @param cols_to_process A vector of column names to process.
  #' @param new_col_name The name of the new column to create.
  dt[, (cols_to_process) := clean_columns(.SD), .SDcols = cols_to_process]
  dt[, (new_col_name) := do.call(paste, c(.SD, sep = "||")), .SDcols = cols_to_process]
  dt[, (new_col_name) := stri_replace_all_regex(get(new_col_name), "\\|\\|NA", "")]
  dt[, (new_col_name) := stri_replace_all_regex(get(new_col_name), "NA\\|\\|", "")]
  dt[, (new_col_name) := stri_replace_all_regex(get(new_col_name), "\\|\\|$", "")]
  dt[, (new_col_name) := ifelse(get(new_col_name) %in% na_like_strings, NA_character_, get(new_col_name))]
  dt[, (cols_to_process) := NULL]
}

find_lumped_codes <- function(codes) {
  #' @description Identifies lumped codes in a vector of codes.
  #' @param codes A vector of codes to check.
  #' @return A logical vector indicating which codes are lumped.
  sapply(codes, function(code) {
    if (is.na(code)) {
      return(FALSE)
    }
    nchar(code) > 4 &&
      str_count(code, "[A-Za-z]") > 1 &&
      str_count(code, "[0-9]") > 1
  })
}

replace_NA_as_char <- function(result) {
  #' @description Replaces "NA" strings with NA values in a vector.
  #' @param result A vector to process.
  #' @return The modified vector with "NA" strings replaced by NA values.
  result[result == "NA" | result == ""] <- NA_character_
  return(result)
}

remove_lumped_icd_codes <- function(dt, column) {
  #' @description Removes lumped ICD codes from a specified column in a data.table.
  #' @param dt A data.table to process.
  #' @param column The name of the column to process.
  #' @return The modified data.table with lumped ICD codes removed.
  dt[, (column) := gsub("(?<=\\d)(?=[A-Za-z])", "||", get(column), perl = TRUE)]
  return(dt)
}

replace_empty_with_na <- function(dt) {
  #' @description Replaces empty strings with NA values in character and factor columns of a data.table.
  #' @param dt A data.table to process.
  #' @return The modified data.table with empty strings replaced by NA values.
  char_factor_cols <- names(dt)[sapply(dt, function(col) is.character(col) || is.factor(col) || is.list(col))]
  dt[, (char_factor_cols) := lapply(.SD, function(x) {
    x[x == "" | x == "NA"] <- NA_character_
    if (is.factor(x)) {
      levels(x) <- c(levels(x), NA)
    }
    return(x)
  }), .SDcols = char_factor_cols]
  return(dt)
}

split_to_vector <- function(column) {
  #' @description Splits strings in a column by "||" and handles NA values.
  #' @param column A column to split.
  #' @return A list of vectors resulting from the split.
  result <- lapply(column, function(x) {
    if (is.na(x)) {
      return(NA_character_)
    } else {
      return(unlist(strsplit(x, "||", fixed = TRUE)))
    }
  })
  return(result)
}

process_icd10_codes <- function(dt, col) {
  #' @description Processes ICD-10 codes in a specified column of a data.table.
  #' @param dt A data.table to process.
  #' @param col The name of the column to process.
  #' @return The modified data.table with processed ICD-10 codes.
  dt[, clin_icd := lapply(clin_icd, function(x) if (is.null(x)) character() else x)]
  dt[lengths(get(col)) > 1, `:=` (
    clin_icd = mapply(function(icd, c1) c(icd, c1[-1]), clin_icd, get(col), SIMPLIFY = FALSE),
    tmp_col = lapply(get(col), function(x) x[1])
  )]
  if (col == "clin_c1") {
    dt[tmp_col != "NULL", clin_c1 := tmp_col]
  } else if (col == "clin_c2") {
    dt[tmp_col != "NULL", clin_c2 := tmp_col]
  }
  dt[, tmp_col := NULL]
  return(dt)
}

process_patient_type <- function(dt) {
  #' @description Processes patient type in a data.table.
  #' @param dt A data.table to process.
  #' @return The modified data.table with processed patient type.
  dt[, pat_type := fcase(
    pat_type == "MEMBER", "MEM",
    pat_type == "DEPENDENT", "DEP"
  )]
  return(dt)
}

process_memcat_parent_desc <- function(dt) {
  #' @description Processes member category parent description in a data.table.
  #' @param dt A data.table to process.
  #' @return The modified data.table with processed member category parent description.
  dt[, pat_memcat_parent := fcase(
    pat_memcat_parent == "DIRECT CONTRIBUTOR", "DIRECT",
    pat_memcat_parent == "INDIRECT CONTRIBUTOR", "INDIRECT"
  )]
  return(dt)
}

process_memcat_child_desc <- function(dt) {
  #' @description Processes member category child description in a data.table.
  #' @param dt A data.table to process.
  #' @return The modified data.table with processed member category child description.
  dt[, pat_memcat_child := fcase(
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
    pat_memcat_child == "FILIPINOS WITH DUAL CITIZENSHIP / LIVING ABROAD", "INFORMAL",
    pat_memcat_child == "SELF EARNING INDIVIDUAL", "INFORMAL",
    pat_memcat_child == "FAMILY DRIVER", "FORMAL"
  )]
  return(dt)
}

process_disposition <- function(dt) {
  #' @description Processes clinical discharge disposition in a data.table.
  #' @param dt A data.table to process.
  #' @return The modified data.table with processed clinical discharge disposition.
  dt[, clin_discharge := fcase(
    clin_discharge == "IMPROVED", 1L,
    clin_discharge == "RECOVERED", 1L,
    clin_discharge == "HOME/DISCHARGED AGAINST MEDICAL ADVICE", 2L,
    clin_discharge == "ABSCONDED", 3L,
    clin_discharge == "TRANSFERRED/REFERRED", 4L,
    clin_discharge == "EXPIRED", 9L,
    clin_discharge == "UNDEFINED", NA_integer_
  )]
  return(dt)
}

process_rvs_code_mapping <- function(dt, rvs_icd9) {
  #' @description Processes RVS code mappings to ICD-9-CM codes in a data.table.
  #' @param dt A data.table to process.
  #' @param rvs_icd9 A data.table containing RVS to ICD-9-CM code mappings.
  #' @return The modified data.table with processed RVS to ICD-9-CM code mappings.
  with_drg <- rvs_icd9[is_drg == TRUE]
  without_drg <- rvs_icd9[!rvs %in% with_drg$rvs]
  cat(sprintf('There are %d RVS codes without an ICD-9CM equivalent recognized by the TDRG ICD9CM\n', length(unique(without_drg$rvs))))
  rvs_map_list <- list()
  rvs_map_solo <- list()
  with_drg <- with_drg[order(rvs, -is_drg)]
  unique_rvs <- unique(with_drg$rvs)
  rvs_grouped <- split(with_drg, with_drg$rvs)
  for (r in unique_rvs) {
    sub <- rvs_grouped[[r]]
    if (nrow(sub) == 1) {
      rvs_map_solo[[r]] <- sub$icd9cm[1]
    } else {
      rvs_map_list[[r]] <- sub$icd9cm
    }
  }
  rvss <- unique(unlist(dt$clin_rvs))
  rvss <- intersect(rvss, acr_rvs$rvs)
  cat(sprintf('There are %d unique RVS codes that appear in the claims.\n', length(rvss)))
  mappable_rvs <- intersect(rvss, rvs_icd9$rvs)
  cat(sprintf('Of these, %d (%.2f%%) have a mapping to an ICD-9-CM code.\n', length(mappable_rvs), length(mappable_rvs) * 100 / length(rvss)))
  multi_mapped_rvs <- intersect(rvss, names(rvs_map_list))
  cat(sprintf('Of these, there are %d (%.2f%%) with more than one ICD9 equivalent recognized by the Thai ICD9 library.\n', length(multi_mapped_rvs), length(multi_mapped_rvs) * 100 / length(mappable_rvs)))
  unmappable_rvs <- setdiff(rvss, mappable_rvs)
  cat(sprintf('There are %d (%.2f%%) with no ICD-9-CM equivalents.\n', length(unmappable_rvs), length(unmappable_rvs) * 100 / length(rvss)))
  dt2 <- dt[lengths(clin_rvs) > 0]
  dt2_info <- dt2[, .N, by = id_series]
  dt2 <- dt2[, .(clin_rvs), by = .(id_series)]
  rvs_map_solo_env <- as.environment(rvs_map_solo)
  rvs_map_list_env <- as.environment(rvs_map_list)
  dt2[, icd9_list := lapply(clin_rvs, function(x) {
    codes <- unlist(x)
    mappable <- codes[!is.na(mget(codes, envir = rvs_map_solo_env, ifnotfound = NA))]
    if (length(mappable) > 0) {
      unique(unlist(mget(mappable, envir = rvs_map_solo_env)))
    } else {
      NA_character_
    }
  })]
  dt2[, rvs_unmap_list := lapply(clin_rvs, function(x) {
    codes <- unlist(x)
    unmappable <- codes[is.na(mget(codes, envir = rvs_map_solo_env, ifnotfound = NA)) & is.na(mget(codes, envir = rvs_map_list_env, ifnotfound = NA))]
    if (length(unmappable) > 0) {
      unique(unmappable)
    } else {
      NA_character_
    }
  })]
  setkey(dt, id_series)
  setkey(dt2, id_series)
  dt <- merge(dt, dt2[, .(id_series, icd9_list, rvs_unmap_list)], by = "id_series", all.x = TRUE)
  return(dt)
}

format_large_numbers <- function(x) {
  #' @description Formats large numbers with appropriate suffixes (e.g., k, m, b).
  #' @param x A numeric value to format.
  #' @return A formatted string representing the large number.
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

process_icd10_mapping <- function(dt) {
  #' @description Processes ICD-10 mappings in a data.table using the Thai ICD-10 library.
  #' @param dt A data.table to process.
  #' @return The modified data.table with processed ICD-10 mappings.
  icds <- unique(c(unlist(dt$clin_c1), unlist(dt$clin_c2), unlist(dt$clin_icd)))
  icds <- icds[!is.na(icds)]
  thai_icd10 <- unique(tdrg_icd10$CODE)
  thai_icd10_env <- list2env(setNames(as.list(rep(TRUE, length(thai_icd10))), thai_icd10))
  direct_matches <- mget(icds, thai_icd10_env, ifnotfound = as.list(rep(FALSE, length(icds))))
  direct_match_codes <- names(unlist(direct_matches[unlist(direct_matches) == TRUE]))
  cat(sprintf("There are %d unique entries for ICD-10 codes, of which %d (%.2f%%) are directly in the Thai ICD-10 library\n", length(icds), length(direct_match_codes), length(direct_match_codes) * 100 / length(icds)))
  icd_mapping <- list()
  modified_count <- 0
  neoplasms <- unique(tdrg_icd10[grepl("/", tdrg_icd10$CODE), "CODE"])
  neoplasms_env <- list2env(setNames(as.list(rep(TRUE, length(neoplasms))), neoplasms))
  for (d in icds) {
    d <- str_trim(d)
    if (exists(d, thai_icd10_env)) {
      icd_mapping[[d]] <- d
    } else if (!exists(d, neoplasms_env) && grepl("[A-Za-z]", d) && grepl("[0-9]", d)) {
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
  cat(sprintf('The modifications led to a total of %d codes being mapped to an equivalent in the Thai ICD10 library.\n', length(icd_mapping)))
  cat(sprintf('Out of these, %d were modified to match.\n', modified_count))
  unmatched_icds <- setdiff(icds, names(icd_mapping))
  if (length(unmatched_icds) > 0) {
    cat(sprintf('There are %d codes that could not be mapped to the Thai ICD10 library:\n', length(unmatched_icds)))
    unmatched_sources <- data.table(
      code = unmatched_icds,
      source = NA_character_
    )
    for (col in c("clin_c1", "clin_c2", "clin_icd")) {
      unmatched_sources[code %in% unlist(dt[[col]]), source := col]
    }
    print(unmatched_sources)
  }
  icd10_map <- data.table(phl_icd10 = names(icd_mapping), tdrg_icd10 = unlist(icd_mapping))
  fwrite(icd10_map, here(path_to_cache, paste0("icd10_map_file_", year_to_load, ".csv")))
  icd10_env <- list2env(setNames(as.list(icd10_map$tdrg_icd10), icd10_map$phl_icd10))
  map_icd10 <- function(codes) {
    mapped <- mget(codes, icd10_env, ifnotfound = as.list(codes))
    return(unname(unlist(mapped)))
  }
  dt[, clin_c1 := lapply(clin_c1, map_icd10)]
  dt[, clin_c2 := lapply(clin_c2, map_icd10)]
  dt[, clin_icd := lapply(clin_icd, map_icd10)]
  return(dt)
}

append_and_remove_rvs_codes <- function(dt, col) {
  #' @description Appends and removes 5-digit numeric codes from a specified column in a data.table.
  #' @param dt A data.table to process.
  #' @param col The name of the column to process.
  #' @return The modified data.table with 5-digit numeric codes appended and removed.
  dt[, clin_rvs := lapply(clin_rvs, function(x) if (is.null(x)) character() else x)]
  regex_5_digit <- "\\b\\d{5}\\b"
  dt[, `:=` (
    clin_rvs = mapply(function(rvs, col_value) {
      matches <- unlist(regmatches(col_value, gregexpr(regex_5_digit, col_value)))
      if (length(matches) > 0) {
        rvs <- c(rvs, matches)
      }
      return(rvs)
    }, clin_rvs, get(col), SIMPLIFY = FALSE),
    tmp_col = lapply(get(col), function(x) {
      gsub(regex_5_digit, "", x)
    })
  )]
  dt[, (col) := tmp_col]
  dt[, tmp_col := NULL]
  return(dt)
}

deduplicate_columns <- function(dt, columns) {
  #' @description Deduplicates specified columns in a data.table.
  #' @param dt A data.table to process.
  #' @param columns A vector of column names to deduplicate.
  #' @return The modified data.table with deduplicated columns.
  for (col in columns) {
    dt[, (col) := lapply(get(col), unique)]
  }
  return(dt)
}

check_similarity <- function(x, y) {
  #' @description Checks similarity between two strings.
  #' @param x The first string to compare.
  #' @param y The second string to compare.
  #' @return A numeric value representing the similarity score.
  score <- 0
  min_len <- min(nchar(x), nchar(y))
  for (i in seq_len(min_len)) {
    if (substr(x, i, i) == substr(y, i, i)) {
      score <- score + 1
    }
  }
  return(score)
}

find_pdx <- function(clin_c1, clin_c2, clin_icd, acc_pdx) {
  #' @description Finds the primary diagnosis (PDX) in a given set of clinical codes.
  #' @param clin_c1 The first clinical code.
  #' @param clin_c2 The second clinical code.
  #' @param clin_icd A list of clinical ICD codes.
  #' @param acc_pdx A vector of acceptable PDX codes.
  #' @return A list containing the PDX and its code.
  clin_icd <- unlist(clin_icd)
  if (!is.null(clin_c1) && clin_c1 %in% acc_pdx) {
    return(list(pdx = clin_c1, pdx_code = 1))
  }
  if (!is.null(clin_c2) && clin_c2 %in% acc_pdx) {
    return(list(pdx = clin_c2, pdx_code = 2))
  }
  pdxs <- intersect(clin_icd, acc_pdx)
  if (length(pdxs) == 0) {
    return(list(pdx = NA_character_, pdx_code = 99))
  } else if (length(pdxs) == 1) {
    return(list(pdx = pdxs[1], pdx_code = 3))
  }
  for (cr in list(clin_c1, clin_c2)) {
    if (!is.na(cr) && cr != "") {
      starting_letter <- substr(cr, 1, 1)
      starting_codes <- pdxs[substr(pdxs, 1, 1) == starting_letter]
      if (length(starting_codes) == 1) {
        return(list(pdx = starting_codes[1], pdx_code = 4))
      }
      if (length(starting_codes) > 1) {
        similarities <- sapply(starting_codes, check_similarity, y = cr)
        most_similar_pdx <- starting_codes[which.max(similarities)]
        return(list(pdx = most_similar_pdx, pdx_code = 5))
      }
    }
  }
  if (length(pdxs) > 0) {
    return(list(pdx = sample(pdxs, 1), pdx_code = 6))
  }
}

apply_find_pdx <- function(dt, acc_pdx, num_cores = availableCores() - 1, seed = 123) {
  #' @description Applies the find_pdx function to each row of a data.table.
  #' @param dt A data.table to process.
  #' @param acc_pdx A vector of acceptable PDX codes.
  #' @param num_cores The number of cores to use for parallel processing.
  #' @param seed The seed for random number generation.
  #' @return The modified data.table with PDX information added.
  plan(multisession, workers = num_cores)
  result_list <- future_lapply(seq_len(nrow(dt)), function(i) {
    find_pdx(dt$clin_c1[i], dt$clin_c2[i], dt$clin_icd[[i]], acc_pdx)
  }, future.seed = seed)
  dt[, `:=`(pdx = sapply(result_list, `[[`, "pdx"), pdx_code = sapply(result_list, `[[`, "pdx_code"))]
  return(dt)
}

generate_dob_vectorized <- function(bdays, ages, date_adms) {
  #' @description Generates date of birth (DOB) values vectorized from birthdates, ages, and admission dates.
  #' @param bdays A vector of birthdates.
  #' @param ages A vector of ages.
  #' @param date_adms A vector of admission dates.
  #' @return A vector of generated DOB values.
  require(lubridate)
  dob <- rep(NA_character_, length(ages))
  dob[!is.na(bdays) & bdays != ""] <- format(mdy(bdays[!is.na(bdays) & bdays != ""]), "%d/%m/%Y")
  missing_bday_indices <- which(is.na(bdays) | bdays == "")
  ref_dates <- mdy(date_adms[missing_bday_indices])
  zero_age_indices <- which(!is.na(ages[missing_bday_indices]) & ages[missing_bday_indices] == 0)
  dob[missing_bday_indices[zero_age_indices]] <- format(ref_dates[zero_age_indices] - days(sample(1:27, length(zero_age_indices), replace = TRUE)), "%d/%m/%Y")
  positive_age_indices <- which(!is.na(ages[missing_bday_indices]) & ages[missing_bday_indices] > 0)
  truncated_ages <- floor(ages[missing_bday_indices][positive_age_indices])
  dob[missing_bday_indices[positive_age_indices]] <- format(ref_dates[positive_age_indices] - years(truncated_ages) - days(sample(1:170, length(positive_age_indices), replace = TRUE)), "%d/%m/%Y")
  return(dob)
}

export_for_batch_grouper <- function(dt, year_to_load, output_txt_file) {
  #' @description Exports data for batch grouper processing.
  #' @param dt A data.table to export.
  #' @param year_to_load The year to load for the export.
  #' @param output_txt_file The path of the output text file.
  output_dt <- data.table(CASEID = 1:nrow(dt))
  output_dt[, DOB := generate_dob_vectorized(dt$pat_bdate, dt$pat_age, dt$date_adm)]
  output_dt[, Sex := ifelse(dt$pat_sex == "M", 1, 2)]
  output_dt[, DateAdm := format(mdy(dt$date_adm), "%d/%m/%Y")]
  output_dt[, TimeAdm := gsub(":", "", dt$time_adm)]
  output_dt[, DateDsc := format(mdy(dt$date_dis), "%d/%m/%Y")]
  output_dt[, TimeDsc := gsub(":", "", dt$time_dis)]
  output_dt[, DischT := dt$clin_discharge]
  output_dt[, AdmWt := dt$pat_bwt]
  output_dt[, PDx := dt$pdx]
  split_icd_codes <- function(icd_str) {
    codes <- unlist(icd_str)
    length(codes) <- 12
    codes
  }
  icd_codes_list <- lapply(dt$clin_icd, split_icd_codes)
  icd_codes <- do.call(rbind, icd_codes_list)
  icd_codes <- as.data.table(icd_codes)
  icd_cols <- paste0("SDx", 1:12)
  output_dt[, (icd_cols) := icd_codes]
  split_rvs_codes <- function(rvs_str) {
    codes <- unlist(rvs_str)
    length(codes) <- 20
    codes
  }
  rvs_codes_list <- lapply(dt$icd9_list, split_rvs_codes)
  rvs_codes <- do.call(rbind, rvs_codes_list)
  rvs_codes <- as.data.table(rvs_codes)
  proc_cols <- paste0("Proc", 1:20)
  output_dt[, (proc_cols) := rvs_codes]
  output_dt[is.na(output_dt)] <- '--'
  for (col in names(output_dt)) {
    if (is.list(output_dt[[col]])) {
      output_dt[[col]] <- sapply(output_dt[[col]], paste, collapse = ",")
    }
  }
  fwrite(output_dt, output_txt_file, sep = "|", col.names = TRUE)
}