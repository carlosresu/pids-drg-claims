read_entire_file <- function(drop_cols) {
  dt <- fread(full_claims, na.strings = na_values, drop = drop_cols, colClasses = col_classes)
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

write_data <- function(dt, path) {
  fwrite(dt, path)
}

add_year_column <- function(dt, year_to_load) {
  dt[, SRC_YR := as.integer(year_to_load)]
  return(dt)
}

rename_columns <- function(dt) {
  setnames(dt, old = old_colnames, new = new_colnames)
  return(dt)
}

clean_columns <- function(dt) {
  # Ensure dt is a data.table
  if (!is.data.table(dt)) {
    dt <- as.data.table(dt)
  }
  
  # Convert all columns to character
  dt[] <- lapply(dt, as.character)
  
  # Apply transformations in a vectorized manner
  dt[] <- lapply(dt, function(col) {
    col <- iconv(col, to = "UTF-8", sub = "byte")  # Convert to UTF-8
    col <- toupper(col)  # Convert to uppercase
    
    # Combine multiple string replacements into one call
    col <- stri_replace_all_regex(col, "[ \n]", "")  # Remove spaces and newlines
    col <- stri_replace_all_regex(col, "[^\\w\\d\\/\\s]+", "")  # Remove non-alphanumeric characters
    
    # Trim spaces from both sides
    col <- stri_trim_both(col)
    
    # Replace NA-like strings with NA
    col <- ifelse(col %in% na_like_strings, NA_character_, col)
    
    return(col)
  })
  
  return(dt)
}

clean_columns_in_dt <- function(dt, cols_to_clean) {
  dt[, (cols_to_clean) := clean_columns(.SD), .SDcols = cols_to_clean]
  return(dt)
}

process_and_collapse_columns <- function(dt, cols_to_process, new_col_name) {
  dt[, (cols_to_process) := clean_columns(.SD), .SDcols = cols_to_process]
  dt[, (new_col_name) := do.call(paste, c(.SD, sep = "||")), .SDcols = cols_to_process]
  dt[, (new_col_name) := gsub("\\|\\|NA", "", get(new_col_name))]
  dt[, (new_col_name) := gsub("NA\\|\\|", "", get(new_col_name))]
  dt[, (new_col_name) := gsub("\\|\\|$", "", get(new_col_name))]
  dt[, (new_col_name) := ifelse(get(new_col_name) %in% na_like_strings, NA_character_, get(new_col_name))]
  dt[, (cols_to_process) := NULL]
}

find_lumped_codes <- function(codes) {
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
  result[result == "NA" | result == ""] <- NA_character_
  return(result)
}

remove_lumped_icd_codes <- function(dt, column) {
  dt[, (column) := gsub("(?<=\\d)(?=[A-Za-z])", "||", get(column), perl = TRUE)]
  return(dt)
}

replace_empty_with_na <- function(dt) {
  # Identify character and factor columns
  char_factor_cols <- names(dt)[sapply(dt, function(x) is.character(x) || is.factor(x))]
  
  # Replace in character and factor columns
  for (col in char_factor_cols) {
    dt[, (col) := {
      col_data <- as.character(get(col))
      col_data[col_data == "" | col_data == "NA"] <- NA_character_
      col_data
    }]
  }
  
  # Identify list columns
  list_cols <- names(dt)[sapply(dt, is.list)]
  
  # Replace in list columns
  for (col in list_cols) {
    dt[, (col) := lapply(get(col), function(x) {
      if (is.null(x)) return(NA_character_)
      x <- ifelse(x == "" | x == "NA", NA_character_, x)
      x
    })]
  }
  
  return(dt)
}

split_to_vector <- function(column) {
  # Handle NA values
  na_indices <- is.na(column)
  
  # Perform strsplit on non-NA values
  split_result <- strsplit(column[!na_indices], "||", fixed = TRUE)
  
  # Reinsert NA values into the split result
  result <- vector("list", length(column))
  result[!na_indices] <- split_result
  result[na_indices] <- NA_character_
  
  # Convert single-item lists to vectors
  result <- lapply(result, function(x) if (length(x) == 1) x[[1]] else x)
  
  return(result)
}


process_icd10_codes <- function(dt, col) {
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
  dt[, pat_type := fcase(
    pat_type == "MEMBER", "MEM",
    pat_type == "DEPENDENT", "DEP"
  )]
  return(dt)
}

process_memcat_parent_desc <- function(dt) {
  dt[, pat_memcat_parent := fcase(
    pat_memcat_parent == "DIRECT CONTRIBUTOR", "DIRECT",
    pat_memcat_parent == "INDIRECT CONTRIBUTOR", "INDIRECT"
  )]
  return(dt)
}

process_memcat_child_desc <- function(dt) {
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
  # Split the rvs_icd9 into two parts based on whether they have DRG use
  with_drg <- rvs_icd9[is_drg == TRUE]
  without_drg <- rvs_icd9[!rvs %in% with_drg$rvs]
  
  # Create environment for faster lookups
  rvs_map_solo_env <- new.env(hash = TRUE, parent = emptyenv())
  rvs_map_list_env <- new.env(hash = TRUE, parent = emptyenv())
  
  # Fill environments with mappings
  for (r in unique(with_drg$rvs)) {
    sub <- with_drg[rvs == r]
    if (nrow(sub) == 1) {
      assign(r, sub$icd9cm[1], envir = rvs_map_solo_env)
    } else {
      assign(r, sub$icd9cm, envir = rvs_map_list_env)
    }
  }
  
  # Process each row of dt
  dt <- dt[, .(id_series, clin_rvs)]
  dt <- dt[, {
    codes <- unlist(clin_rvs)
    codes <- as.character(codes) # Ensure codes is a character vector
    
    # Map the codes
    mapped_icd9 <- unique(unlist(mget(codes, envir = rvs_map_solo_env, ifnotfound = NA)))
    unmapped_rvs <- codes[is.na(mget(codes, envir = rvs_map_solo_env, ifnotfound = NA)) & is.na(mget(codes, envir = rvs_map_list_env, ifnotfound = NA))]
    
    # Output mapped ICD9 codes and unmapped RVS codes
    .(icd9_list = list(mapped_icd9), rvs_unmap_list = list(unmapped_rvs))
  }, by = id_series]
  
  # Merge the results back to the original dt
  dt <- merge(dt, dt, by = "id_series", all.x = TRUE)
  return(dt)
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

process_icd10_mapping <- function(dt) {
  icds <- unique(c(unlist(dt$clin_c1), unlist(dt$clin_c2), unlist(dt$clin_icd)))
  icds <- icds[!is.na(icds)]
  thai_icd10 <- unique(tdrg_icd10$CODE)
  thai_icd10_env <- list2env(setNames(as.list(rep(TRUE, length(thai_icd10))), thai_icd10))
  direct_matches <- mget(icds, thai_icd10_env, ifnotfound = as.list(rep(FALSE, length(icds))))
  direct_match_codes <- names(unlist(direct_matches[unlist(direct_matches) == TRUE]))
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
  unmatched_icds <- setdiff(icds, names(icd_mapping))
  if (length(unmatched_icds) > 0) {
    unmatched_sources <- data.table(
      code = unmatched_icds,
      source = NA_character_
    )
    for (col in c("clin_c1", "clin_c2", "clin_icd")) {
      unmatched_sources[code %in% unlist(dt[[col]]), source := col]
    }
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
  regex_5_digit <- "\\b\\d{5}\\b"
  
  # Detect and append 5-digit numeric codes
  matches_list <- regmatches(dt[[col]], gregexpr(regex_5_digit, dt[[col]]))
  
  # Append matches to clin_rvs
  dt[, clin_rvs := Map(c, clin_rvs, matches_list)]
  
  # Remove 5-digit numeric codes from the original column
  dt[, (col) := gsub(regex_5_digit, "", dt[[col]])]
  
  return(dt)
}

deduplicate_columns <- function(dt, columns) {
  for (col in columns) {
    dt[, (col) := lapply(.SD[[1]], function(x) {
      if (is.null(x)) return(NA)
      unique(x)
    }), .SDcols = col]
  }
  return(dt)
}


check_similarity <- function(x, y) {
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
  result_list <- future_lapply(seq_len(nrow(dt)), function(i) {
    find_pdx(dt$clin_c1[i], dt$clin_c2[i], dt$clin_icd[[i]], acc_pdx)
  }, future.seed = seed)
  
  dt[, `:=`(pdx = sapply(result_list, `[[`, "pdx"), pdx_code = sapply(result_list, `[[`, "pdx_code"))]
  
  return(dt)
}

generate_dob_vectorized <- function(bdays, ages, date_adms) {
  require(lubridate)
  dob <- rep(NA_character_, length(ages))  # Initialize dob vector
  dob[!is.na(bdays) & bdays != ""] <- format(mdy(bdays[!is.na(bdays) & bdays != ""]), "%d/%m/%Y")  # Use PAT_BDAY where available
  
  # Indices where PAT_BDAY is not available
  missing_bday_indices <- which(is.na(bdays) | bdays == "")
  
  # Use age and date_adm to generate DOB for those with missing PAT_BDAY
  ref_dates <- mdy(date_adms[missing_bday_indices])
  
  # Case 1: age == 0
  zero_age_indices <- which(!is.na(ages[missing_bday_indices]) & ages[missing_bday_indices] == 0)
  dob[missing_bday_indices[zero_age_indices]] <- format(ref_dates[zero_age_indices] - days(sample(1:27, length(zero_age_indices), replace = TRUE)), "%d/%m/%Y")
  
  # Case 2: age > 0
  positive_age_indices <- which(!is.na(ages[missing_bday_indices]) & ages[missing_bday_indices] > 0)
  truncated_ages <- floor(ages[missing_bday_indices][positive_age_indices])
  dob[missing_bday_indices[positive_age_indices]] <- format(ref_dates[positive_age_indices] - years(truncated_ages) - days(sample(1:170, length(positive_age_indices), replace = TRUE)), "%d/%m/%Y")
  
  return(dob)
}

export_for_batch_grouper <- function(dt, year_to_load, output_txt_file) {
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
  
  # Split icd_list_1 into multiple columns, ensuring 12 elements per row
  split_icd_codes <- function(icd_str) {
    codes <- unlist(icd_str)
    length(codes) <- 12  # Ensuring 12 elements, with NA for missing
    codes
  }
  
  icd_codes_list <- lapply(dt$clin_icd, split_icd_codes)
  icd_codes <- do.call(rbind, icd_codes_list)
  icd_codes <- as.data.table(icd_codes)
  icd_cols <- paste0("SDx", 1:12)
  output_dt[, (icd_cols) := icd_codes]
  
  # Split icd9_list into multiple columns, ensuring 20 elements per row
  split_rvs_codes <- function(rvs_str) {
    codes <- unlist(rvs_str)
    length(codes) <- 20  # Ensuring 20 elements, with NA for missing
    codes
  }
  
  rvs_codes_list <- lapply(dt$icd9_list, split_rvs_codes)
  rvs_codes <- do.call(rbind, rvs_codes_list)
  rvs_codes <- as.data.table(rvs_codes)
  proc_cols <- paste0("Proc", 1:20)
  output_dt[, (proc_cols) := rvs_codes]
  
  # Replace NA values with '--'
  output_dt[is.na(output_dt)] <- '--'
  
  # Convert list columns to character if any
  for (col in names(output_dt)) {
    if (is.list(output_dt[[col]])) {
      output_dt[[col]] <- sapply(output_dt[[col]], paste, collapse = ",")
    }
  }
  
  # Write the data to a text file
  fwrite(output_dt, output_txt_file, sep = "|", col.names = TRUE)
}