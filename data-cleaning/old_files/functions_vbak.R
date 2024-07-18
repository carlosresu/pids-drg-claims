read_entire_file <- function(drop_cols) {
  dt <- fread(full_claims_file, na.strings = na_values, drop = drop_cols, colClasses = col_classes)
  return(dt)
}

read_sampled_file <- function() {
  dt <- fread(sampled_claims_file, na.strings = na_values, colClasses = col_classes)
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
    col <- iconv(col, to = "UTF-8", sub = "byte") # Convert to UTF-8
    col <- toupper(col) # Convert to uppercase

    # Combine multiple string replacements into one call
    col <- stri_replace_all_regex(col, "[ \n]", "") # Remove spaces and newlines
    col <- stri_replace_all_regex(col, "[^\\w\\d\\/\\s]+", "") # Remove non-alphanumeric characters

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
  dt[, (new_col_name) := stri_replace_all_regex(get(new_col_name), "\\|\\|NA", "")]
  dt[, (new_col_name) := stri_replace_all_regex(get(new_col_name), "NA\\|\\|", "")]
  dt[, (new_col_name) := stri_replace_all_regex(get(new_col_name), "\\|\\|$", "")]
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
    dt[, (col) := fifelse(as.character(get(col)) %in% na_like_strings, NA_character_, as.character(get(col)))]
  }

  # Identify list columns
  list_cols <- names(dt)[sapply(dt, is.list)]

  # Replace in list columns
  for (col in list_cols) {
    dt[, (col) := lapply(get(col), function(x) ifelse(x %in% na_like_strings, NA, x))]
  }

  return(dt)
}


split_to_vector <- function(column) {
  # Split the column using strsplit and filter out NAs directly
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
  # Ensure clin_icd is initialized if it's empty
  dt[, clin_icd := lapply(clin_icd, function(x) if (is.null(x)) character() else x)]

  # Process each row where the length of col is more than 1
  dt[lengths(get(col)) > 1, `:=`(
    # Append all but the first element of col to clin_icd
    clin_icd = mapply(function(icd, c1) c(icd, c1[-1]), clin_icd, get(col), SIMPLIFY = FALSE),
    # Retain only the first element in col
    tmp_col = lapply(get(col), function(x) x[1])
  )]

  if (col == "clin_c1") {
    dt[tmp_col != "NULL", clin_c1 := tmp_col]
  } else if (col == "clin_c2") {
    dt[tmp_col != "NULL", clin_c2 := tmp_col]
  }

  # Remove the temporary tmp_col column
  dt[, tmp_col := NULL]

  # Return the modified data table
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
  with_drg <- rvs_icd9[is_drg == TRUE]

  without_drg <- rvs_icd9[!rvs %in% with_drg$rvs]
  cat(sprintf("There are %d RVS codes without an ICD-9CM equivalent recognized by the TDRG ICD9CM\n", length(unique(without_drg$rvs))))

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

  cat(sprintf("There are %d unique RVS codes that appear in the claims.\n", length(rvss)))

  mappable_rvs <- intersect(rvss, rvs_icd9$rvs)
  cat(sprintf("Of these, %d (%.2f%%) have a mapping to an ICD-9-CM code.\n", length(mappable_rvs), length(mappable_rvs) * 100 / length(rvss)))

  multi_mapped_rvs <- intersect(rvss, names(rvs_map_list))
  cat(sprintf("Of these, there are %d (%.2f%%) with more than one ICD9 equivalent recognized by the Thai ICD9 library.\n", length(multi_mapped_rvs), length(multi_mapped_rvs) * 100 / length(mappable_rvs)))

  unmappable_rvs <- setdiff(rvss, mappable_rvs)
  cat(sprintf("There are %d (%.2f%%) with no ICD-9-CM equivalents.\n", length(unmappable_rvs), length(unmappable_rvs) * 100 / length(rvss)))

  dt2 <- dt[lengths(clin_rvs) > 0]
  dt2_info <- dt2[, .N, by = id_series]

  dt2 <- dt2[, .(clin_rvs), by = .(id_series)]

  # Convert hash tables to environments for faster lookup with mget
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

  # Set keys for faster merge
  setkey(dt, id_series)
  setkey(dt2, id_series)

  # Perform the merge
  dt <- merge(dt, dt2[, .(id_series, icd9_list, rvs_unmap_list)], by = "id_series", all.x = TRUE)

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
  icds <- unique(c(
    unlist(dt$clin_c1),
    unlist(dt$clin_c2),
    unlist(dt$clin_icd)
  ))

  icds <- icds[!is.na(icds)]

  thai_icd10 <- unique(tdrg_icd10$CODE)

  # Create environment for fast %in% checking
  thai_icd10_env <- list2env(setNames(as.list(rep(TRUE, length(thai_icd10))), thai_icd10))

  direct_matches <- mget(icds, thai_icd10_env, ifnotfound = as.list(rep(FALSE, length(icds))))
  direct_match_codes <- names(unlist(direct_matches[unlist(direct_matches) == TRUE]))

  cat(sprintf(
    "There are %d unique entries for ICD-10 codes, of which %d (%.2f%%) are directly in the Thai ICD-10 library\n",
    length(icds), length(direct_match_codes), length(direct_match_codes) * 100 / length(icds)
  ))

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

  cat(sprintf("The modifications led to a total of %d codes being mapped to an equivalent in the Thai ICD10 library.\n", length(icd_mapping)))
  cat(sprintf("Out of these, %d were modified to match.\n", modified_count))

  # Detailed logging of unmatched codes
  unmatched_icds <- setdiff(icds, names(icd_mapping))
  if (length(unmatched_icds) > 0) {
    cat(sprintf("There are %d codes that could not be mapped to the Thai ICD10 library:\n", length(unmatched_icds)))

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
  # Ensure clin_rvs is initialized if it's empty
  dt[, clin_rvs := lapply(clin_rvs, function(x) if (is.null(x)) character() else x)]

  # Regex to match 5-digit numeric codes
  regex_5_digit <- "\\b\\d{5}\\b"

  # Process each row to detect, append, and delete 5-digit numeric codes
  dt[, `:=`(
    # Append detected 5-digit numeric codes to clin_rvs
    clin_rvs = mapply(function(rvs, col_value) {
      matches <- unlist(regmatches(col_value, gregexpr(regex_5_digit, col_value)))
      if (length(matches) > 0) {
        rvs <- c(rvs, matches)
      }
      return(rvs)
    }, clin_rvs, get(col), SIMPLIFY = FALSE),
    # Remove 5-digit numeric codes from the original column
    tmp_col = lapply(get(col), function(x) {
      gsub(regex_5_digit, "", x)
    })
  )]

  # Copy the modified values back to the original column
  dt[, (col) := tmp_col]

  # Remove the temporary tmp_col column
  dt[, tmp_col := NULL]

  # Return the modified data table
  return(dt)
}

deduplicate_columns <- function(dt, columns) {
  for (col in columns) {
    dt[, (col) := lapply(.SD, function(x) list(unique(unlist(x)))), .SDcols = col]
  }
  return(dt)
}


# find_pdx <- function(clin_c1, clin_c2, clin_icd, acc_pdx) {
#   check_similarity <- function(x, y) {
#     # See how many starting letters the two strings have in common
#     score <- 0
#     min_len <- min(nchar(x), nchar(y))
#     for (i in seq_len(min_len)) {
#       if (substr(x, i, i) == substr(y, i, i)) {
#         score <- score + 1
#       }
#     }
#     return(score)
#   }
#
#   # Ensure that clin_icd is a vector
#   clin_icd <- unlist(clin_icd)
#
#   # First, see if either clin_c1 or clin_c2 is an acceptable pdx
#   if (!is.null(clin_c1) && clin_c1 %in% acc_pdx) {
#     return(list(pdx = clin_c1, pdx_code = 1))
#   }
#   if (!is.null(clin_c2) && clin_c2 %in% acc_pdx) {
#     return(list(pdx = clin_c2, pdx_code = 2))
#   }
#
#   # Check all the other codes listed under clin_icd
#   pdxs <- intersect(clin_icd, acc_pdx)
#
#   # For those with no acceptable pdx or only one acceptable pdx
#   if (length(pdxs) == 0) {
#     return(list(pdx = NA_character_, pdx_code = 99))
#   } else if (length(pdxs) == 1) {
#     return(list(pdx = pdxs[1], pdx_code = 3))
#   }
#
#   # If there are multiple eligible pdx
#   for (cr in list(clin_c1, clin_c2)) {
#     if (!is.na(cr) && cr != "") {
#       starting_letter <- substr(cr, 1, 1)
#       starting_codes <- pdxs[substr(pdxs, 1, 1) == starting_letter]
#
#       if (length(starting_codes) == 1) {
#         return(list(pdx = starting_codes[1], pdx_code = 4))
#       }
#       if (length(starting_codes) > 1) {
#         similarities <- sapply(starting_codes, check_similarity, y = cr)
#         most_similar_pdx <- starting_codes[which.max(similarities)]
#         return(list(pdx = most_similar_pdx, pdx_code = 5))
#       }
#     }
#   }
#
#   # If there is no related starting letter, choose randomly
#   if (length(pdxs) > 0) {
#     return(list(pdx = sample(pdxs, 1), pdx_code = 6))
#   }
# }
#
# apply_find_pdx <- function(dt, acc_pdx) {
#   results <- dt[, {
#     result <- find_pdx(.SD$clin_c1[[1]], .SD$clin_c2[[1]], .SD$clin_icd[[1]], acc_pdx)
#     .(pdx = result$pdx, pdx_code = result$pdx_code)
#   }, by = 1:nrow(dt)]
#
#   dt[, `:=`(pdx = results$pdx, pdx_code = results$pdx_code)]
#   return(dt)
# }

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
  plan(multisession, workers = num_cores)

  result_list <- future_lapply(seq_len(nrow(dt)), function(i) {
    find_pdx(dt$clin_c1[i], dt$clin_c2[i], dt$clin_icd[[i]], acc_pdx)
  }, future.seed = seed)

  dt[, `:=`(pdx = sapply(result_list, `[[`, "pdx"), pdx_code = sapply(result_list, `[[`, "pdx_code"))]

  return(dt)
}

generate_dob_vectorized <- function(bdays, ages, date_adms) {
  require(lubridate)
  dob <- rep(NA_character_, length(ages)) # Initialize dob vector
  dob[!is.na(bdays) & bdays != ""] <- format(mdy(bdays[!is.na(bdays) & bdays != ""]), "%d/%m/%Y") # Use PAT_BDAY where available

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
    length(codes) <- 12 # Ensuring 12 elements, with NA for missing
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
    length(codes) <- 20 # Ensuring 20 elements, with NA for missing
    codes
  }

  rvs_codes_list <- lapply(dt$icd9_list, split_rvs_codes)
  rvs_codes <- do.call(rbind, rvs_codes_list)
  rvs_codes <- as.data.table(rvs_codes)
  proc_cols <- paste0("Proc", 1:20)
  output_dt[, (proc_cols) := rvs_codes]

  # Replace NA values with '--'
  output_dt[is.na(output_dt)] <- "--"

  # Convert list columns to character if any
  for (col in names(output_dt)) {
    if (is.list(output_dt[[col]])) {
      output_dt[[col]] <- sapply(output_dt[[col]], paste, collapse = ",")
    }
  }

  # Write the data to a text file
  fwrite(output_dt, output_txt_file, sep = "|", col.names = TRUE)
}
