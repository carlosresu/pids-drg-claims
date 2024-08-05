# Function to remove lumped ICD codes
remove_lumped_icd_codes <- function(column) {
  #' @title Remove Lumped ICD Codes
  #'
  #' @description This function removes lumped ICD codes by adding a separator
  #' between numeric and alphabetic characters.
  #'
  #' @param column character. The column to be processed.
  #'
  #' @return character. The modified column with lumped ICD codes separated.

  modified_column <- gsub("(?<=\\d)(?=[A-Za-z])", "||", column, perl = TRUE)
  return(modified_column)
}

# Function to transfer extra ICD-10 codes to clinical ICD
transfer_extra_icd10s_to_clin_icd <- function(clin_icd, col) {
  #' @title Transfer Extra ICD-10 Codes to Clinical ICD
  #'
  #' @description This function transfers extra ICD-10 codes from
  #' a column to the clinical ICD.
  #'
  #' @param clin_icd list. The clinical ICD codes.
  #' @param col list. The column containing extra ICD-10 codes.
  #'
  #' @return list. A list containing updated clinical ICD and the
  #' first code of the column.

  clin_icd <- lapply(clin_icd, function(x) if (is.null(x)) character() else x)
  col_first <- lapply(col, function(x) x[1])

  clin_icd <- mapply(function(icd, c1) {
    c(icd, c1[-1])
  }, clin_icd, col, SIMPLIFY = FALSE)

  return(list(clin_icd = clin_icd, col_first = col_first))
}

# Function to get unique ICD codes
get_unique_icd_codes <- function(clin_c1, clin_c2, clin_icd) {
  #' @title Get Unique ICD Codes
  #'
  #' @description This function retrieves unique ICD codes from
  #' the given columns.
  #'
  #' @param clin_c1 list. The clinical column 1 ICD codes.
  #' @param clin_c2 list. The clinical column 2 ICD codes.
  #' @param clin_icd list. The clinical ICD codes.
  #'
  #' @return character. The unique ICD codes.

  icds <- unique(c(unlist(clin_c1), unlist(clin_c2), unlist(clin_icd)))
  icds <- icds[!is.na(icds)]
  return(icds)
}

# Function to create a Thai ICD-10 environment
create_thai_icd10_environment <- function(thai_icd10_codes) {
  #' @title Create Thai ICD-10 Environment
  #'
  #' @description This function creates an environment for Thai ICD-10 codes.
  #'
  #' @param thai_icd10_codes character. The Thai ICD-10 codes.
  #'
  #' @return environment. The environment with Thai ICD-10 codes.

  thai_icd10_env <- list2env(
    setNames(as.list(rep(TRUE, length(thai_icd10_codes))), thai_icd10_codes)
  )
  return(thai_icd10_env)
}

# Function to find direct ICD matches
find_direct_icd_matches <- function(icds, thai_icd10_env) {
  #' @title Find Direct ICD Matches
  #'
  #' @description This function finds direct matches for ICD codes
  #' in the Thai ICD-10 environment.
  #'
  #' @param icds character. The ICD codes to be matched.
  #' @param thai_icd10_env environment. The environment with Thai ICD-10 codes.
  #'
  #' @return character. The ICD codes that have direct matches.

  direct_matches <- mget(
    icds, thai_icd10_env,
    ifnotfound = as.list(rep(FALSE, length(icds)))
  )
  direct_match_codes <- names(
    unlist(direct_matches[unlist(direct_matches) == TRUE])
  )
  return(direct_match_codes)
}

# Function to generate ICD-10 mapping
generate_icd10_mapping <- function(icds, thai_icd10_env, neoplasms_env) {
  #' @title Generate ICD-10 Mapping
  #'
  #' @description This function generates a mapping of ICD-10 codes
  #' based on the Thai ICD-10 environment.
  #'
  #' @param icds character. The ICD codes to be mapped.
  #' @param thai_icd10_env environment. The environment with Thai ICD-10 codes.
  #' @param neoplasms_env environment. The environment with neoplasm ICD codes.
  #'
  #' @return list. A list containing the ICD mapping and the count of
  #' modified codes.

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
  #' @title Apply ICD-10 Mapping to Columns
  #'
  #' @description This function maps ICD-10 codes to the given columns
  #' using the provided environment.
  #'
  #' @param clin_c1 list. The clinical column 1 ICD codes.
  #' @param clin_c2 list. The clinical column 2 ICD codes.
  #' @param clin_icd list. The clinical ICD codes.
  #' @param icd10_env environment. The environment with ICD-10 codes.
  #'
  #' @return list. A list containing the mapped clinical columns.

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

# Function to implement ICD-10 mapping
implement_icd10_mapping <- function(clin_c1, clin_c2, clin_icd, tdrg_icd10) {
  #' @title Implement ICD-10 Mapping
  #'
  #' @description This function implements the ICD-10 mapping
  #' for the given clinical columns.
  #'
  #' @param clin_c1 list. The clinical column 1 ICD codes.
  #' @param clin_c2 list. The clinical column 2 ICD codes.
  #' @param clin_icd list. The clinical ICD codes.
  #' @param tdrg_icd10 data.table. The table with Thai ICD-10 codes.
  #'
  #' @return list. A list containing the mapped clinical columns
  #' and related information.

  icds <- get_unique_icd_codes(clin_c1, clin_c2, clin_icd)

  thai_icd10_env <- create_thai_icd10_environment(
    unique(tdrg_icd10$CODE)
  )
  neoplasms_env <- create_thai_icd10_environment(
    unique(tdrg_icd10[grepl("/", tdrg_icd10$CODE), "CODE"])
  )

  direct_match_codes <- find_direct_icd_matches(
    icds, thai_icd10_env
  )

  icd_mapping_info <- generate_icd10_mapping(
    icds, thai_icd10_env, neoplasms_env
  )
  icd_mapping <- icd_mapping_info$icd_mapping
  modified_count <- icd_mapping_info$modified_count

  unmatched_icds <- setdiff(icds, names(icd_mapping))

  if (length(unmatched_icds) > 0) {
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
  if (to_debug) fwrite(icd10_map, paste0("cache/icd10_map_file_", year_to_load, ".csv"))
  icd10_env <- list2env(
    setNames(as.list(icd10_map$tdrg_icd10), icd10_map$phl_icd10)
  )

  mapped_columns <- apply_icd10_mapping_to_columns(
    clin_c1, clin_c2, clin_icd, icd10_env
  )

  return(list(
    clin_c1 = mapped_columns$clin_c1,
    clin_c2 = mapped_columns$clin_c2,
    clin_icd = mapped_columns$clin_icd,
    icd10_map_dt = icd10_map,
    unique_icds = icds,
    direct_matches = direct_match_codes,
    unmatched = unmatched_icds,
    unmatched_sources = unmatched_sources
  ))
}

# Function to ensure unique ICD codes
ensure_unique_icd_codes <- function(clin_c1, clin_c2, clin_icd) {
  #' @title Ensure Unique ICD Codes
  #'
  #' @description This function ensures that ICD codes are unique
  #' within and across clinical columns.
  #'
  #' @param clin_c1 list. The clinical column 1 ICD codes.
  #' @param clin_c2 list. The clinical column 2 ICD codes.
  #' @param clin_icd list. The clinical ICD codes.
  #'
  #' @return list. A list containing the deduplicated clinical columns.

  # Convert lists to data.table for efficient processing
  datatable <- data.table(
    clin_c1 = clin_c1,
    clin_c2 = clin_c2,
    clin_icd = clin_icd
  )

  # Deduplicate each column
  datatable[, clin_c1 := lapply(clin_c1, unique)]
  datatable[, clin_c2 := lapply(clin_c2, unique)]
  datatable[, clin_icd := lapply(clin_icd, unique)]

  # Remove entries in clin_icd that are in clin_c1 or clin_c2
  datatable[, clin_icd := Map(function(c1, c2, icd) {
    setdiff(icd, union(c1, c2))
  }, clin_c1, clin_c2, clin_icd)]

  # Remove entries in clin_c1 that are in clin_c2
  datatable[, clin_c1 := Map(function(c1, c2) {
    setdiff(c1, c2)
  }, clin_c1, clin_c2)]

  # Remove entries in clin_c2 that are in clin_c1
  datatable[, clin_c2 := Map(function(c1, c2) {
    setdiff(c2, c1)
  }, clin_c1, clin_c2)]

  return(
    list(
      clin_c1 = datatable$clin_c1,
      clin_c2 = datatable$clin_c2,
      clin_icd = datatable$clin_icd
    )
  )
}

# Function to split RVS codes
split_rvs_codes <- function(rvs_icd9) {
  with_drg <- rvs_icd9[is_drg == TRUE]
  without_drg <- rvs_icd9[!rvs %in% with_drg$rvs]
  return(list(with_drg = with_drg, without_drg = without_drg))
}

# Function to create RVS map lists
create_rvs_map_lists <- function(with_drg) {
  setorder(with_drg, rvs, -is_drg)
  unique_rvs <- with_drg[, .(icd9cm_list = list(icd9cm)), by = rvs]
  solo <- unique_rvs[lengths(icd9cm_list) == 1]
  list_mapped <- unique_rvs[lengths(icd9cm_list) > 1]

  rvs_map_solo <- setNames(solo$icd9cm_list, solo$rvs)
  rvs_map_list <- setNames(list_mapped$icd9cm_list, list_mapped$rvs)

  return(list(rvs_map_list = rvs_map_list, rvs_map_solo = rvs_map_solo))
}

# Function to get ICD-9 codes from clinical RVS
get_icd9_codes <- function(clin_rvs, rvs_map_solo_env) {
  lapply(clin_rvs, function(x) {
    codes <- unlist(x)
    mappable <- codes[
      !is.na(mget(codes, envir = rvs_map_solo_env, ifnotfound = NA))
    ]
    if (length(mappable) > 0) {
      unique(unlist(mget(mappable, envir = rvs_map_solo_env)))
    } else {
      NA_character_
    }
  })
}

# Function to map RVS to ICD-9
map_rvs_icd9 <- function(clin_rvs, rvs_icd9) {
  split_codes <- split_rvs_codes(rvs_icd9)
  rvs_maps <- create_rvs_map_lists(split_codes$with_drg)
  rvs_map_list <- rvs_maps$rvs_map_list

  rvs_map_solo_env <- as.environment(rvs_maps$rvs_map_solo)
  icd9_list <- get_icd9_codes(clin_rvs, rvs_map_solo_env)

  rvss <- unique(unlist(clin_rvs))
  mappable_rvs <- intersect(rvss, rvs_icd9$rvs)
  unmappable_rvs <- setdiff(rvss, rvs_icd9$rvs)
  multi_mapped_rvs <- intersect(rvss, names(rvs_map_list))
  without_drg <- unique(rvs_icd9[!rvs %in% names(rvs_map_list)]$rvs)

  return_list <- list(
    icd9_list = icd9_list,
    rvs_map_list = rvs_maps$rvs_map_list,
    rvss = rvss,
    mappable_rvs = mappable_rvs,
    unmappable_rvs = unmappable_rvs,
    multi_mapped_rvs = multi_mapped_rvs,
    without_drg = without_drg
  )

  return(return_list)
}

# Function to find and append valid RVS codes
find_and_append_valid_rvs <- function(datatable, valid_rvs_codes) {
  regex_5_digit <- "\\b\\d{5}\\b"
  valid_rvs_env <- new.env(hash = TRUE, parent = emptyenv())
  for (code in valid_rvs_codes) {
    assign(code, TRUE, envir = valid_rvs_env)
  }

  datatable[, matches := regmatches(col, gregexpr(regex_5_digit, col))]
  datatable[, valid_matches := lapply(
    matches,
    function(x) x[x %in% valid_rvs_codes]
  )]
  datatable[, clin_rvs := mapply(
    function(rvs, matches) unique(c(rvs, matches)),
    clin_rvs, valid_matches,
    SIMPLIFY = FALSE
  )]
}

# Function to remove 5-digit codes
remove_5_digit_codes <- function(col) {
  regex_5_digit <- "\\b\\d{5}\\b"
  gsub(regex_5_digit, "", col)
}

# Function to warn about invalid RVS codes
warn_invalid_rvs <- function(matches, valid_rvs_codes) {
  valid_rvs_env <- new.env(hash = TRUE, parent = emptyenv())
  for (code in valid_rvs_codes) {
    assign(code, TRUE, envir = valid_rvs_env)
  }

  invalid_matches <- lapply(
    matches,
    function(x) x[!vapply(x, exists, logical(1), envir = valid_rvs_env)]
  )
  discarded_codes <- unlist(invalid_matches)
  if (length(discarded_codes) > 0) {
    discarded_table <- data.table(
      CODE = discarded_codes
    )[, .N, by = CODE][order(-N)]
    setnames(discarded_table, c("CODE", "count"))
  } else {
    discarded_table <- data.table()
  }
  return(discarded_table)
}

# Function to append and remove RVS codes
append_and_remove_rvs <- function(clin_rvs, col, rvs_icd9) {
  datatable <- data.table(clin_rvs = clin_rvs, col = col)
  valid_rvs_codes <- rvs_icd9$rvs

  find_and_append_valid_rvs(datatable, valid_rvs_codes)
  datatable[, col := remove_5_digit_codes(col)]
  discarded_rvs <- warn_invalid_rvs(datatable$matches, valid_rvs_codes)

  return(
    list(
      clin_rvs = datatable$clin_rvs,
      col = datatable$col,
      discarded_rvs = discarded_rvs
    )
  )
}

# Function to find the primary diagnosis (PDX) based on the provided logic
find_pdx <- function(clin_c1, clin_c2, clin_icd, acc_pdx_env) {
  #' @title Find primary diagnosis (PDX)
  #'
  #' @description This function finds the primary diagnosis (PDX) based
  #' on the provided clinical codes and acceptable PDX environment.
  #'
  #' @param clin_c1 character The first clinical code.
  #' @param clin_c2 character The second clinical code.
  #' @param clin_icd list The list of clinical ICD codes.
  #' @param acc_pdx_env environment The environment containing
  #' acceptable PDX codes.
  #'
  #' @return list A list containing the PDX and PDX code.

  check_similarity <- function(x, y) {
    score <- 0
    min_len <- min(nchar(x), nchar(y))
    for (i in 1:min_len) {
      if (substr(x, i, i) == substr(y, i, i)) {
        score <- score + 1
      }
    }
    return(score)
  }

  clin_icd <- unlist(clin_icd)

  # Get a list of all SDx that may be chosen as PDx
  pdxs <- unique(clin_icd)
  pdxs <- pdxs[sapply(pdxs, function(x) exists(x, acc_pdx_env))]

  # For those with no acceptable PDx or only 1 acceptable PDx
  if (length(pdxs) == 0) {
    return(list(pdx = NA_character_, pdx_code = 99))
  } else if (length(pdxs) == 1) {
    return(list(pdx = pdxs[1], pdx_code = 3))
  }

  # If there are multiple eligible PDx,
  # see if any are related to the starting letters
  for (cr in c(clin_c1, clin_c2)) {
    if (!is.na(cr)) {
      if (exists(cr, acc_pdx_env)) { # If clin_c* is a valid ICD-10
        # Get starting letter of clin_c*
        starting_letter <- substr(cr, 1, 1)
        # List all valid ICD-10 codes with same starting letter
        starting_codes <- pdxs[substr(pdxs, 1, 1) == starting_letter]
        # If there's only one similar eligible PDx, choose that
        if (length(starting_codes) == 1) {
          return(list(pdx = starting_codes[1], pdx_code = 4))
        }
        # If there are multiple similar eligible PDx
        if (length(starting_codes) > 1) {
          # Obtain the one that most resembles the case rate
          starting_codes <- starting_codes[
            order(sapply(
              starting_codes,
              function(x) check_similarity(cr, x)
            ), decreasing = TRUE)
          ]
          return(list(pdx = starting_codes[1], pdx_code = 5))
        }
      }
    }
  }

  # If there is no related starting letter, choose randomly
  if (length(pdxs) > 0) {
    return(list(pdx = sample(pdxs, 1), pdx_code = 6))
  }

  return(list(pdx = NA_character_, pdx_code = 99))
}

apply_find_pdx <- function(clin_c1, clin_c2, clin_icd, acc_pdx) {
  #' @title Apply find PDX
  #'
  #' @description This function applies the find PDX logic
  #' to a set of clinical codes and acceptable PDX codes.
  #'
  #' @param clin_c1 list The list of first clinical codes.
  #' @param clin_c2 list The list of second clinical codes.
  #' @param clin_icd list The list of clinical ICD codes.
  #' @param acc_pdx character The list of acceptable PDX codes.
  #'
  #' @return list A list containing the PDX and PDX codes
  #' for the input clinical codes.

  acc_pdx_env <- new.env(hash = TRUE, parent = emptyenv())
  for (code in acc_pdx) {
    assign(code, TRUE, envir = acc_pdx_env)
  }

  datatable <- data.table(
    clin_c1 = clin_c1,
    clin_c2 = clin_c2,
    clin_icd = clin_icd
  )

  # Vectorized application of find_pdx function
  find_pdx_vectorized <- function(clin_c1, clin_c2, clin_icd) {
    # Convert lists to characters for easy handling
    clin_c1_char <- sapply(
      clin_c1, function(x) if (is.null(x)) NA_character_ else x
    )
    clin_c2_char <- sapply(
      clin_c2, function(x) if (is.null(x)) NA_character_ else x
    )
    clin_icd_char <- sapply(
      clin_icd, function(x) paste(x, collapse = ",")
    )

    # Initialize result vectors
    pdx <- rep(NA_character_, length(clin_c1))
    pdx_code <- rep(NA_integer_, length(clin_c1))

    # Batch check clin_c1 and clin_c2
    clin_c1_check <- sapply(clin_c1_char, function(x) exists(x, acc_pdx_env))
    clin_c2_check <- sapply(clin_c2_char, function(x) exists(x, acc_pdx_env))

    pdx[clin_c1_check] <- clin_c1_char[clin_c1_check]
    pdx_code[clin_c1_check] <- 1

    clin_c2_only_check <- !clin_c1_check & clin_c2_check
    pdx[clin_c2_only_check] <- clin_c2_char[clin_c2_only_check]
    pdx_code[clin_c2_only_check] <- 2

    # Apply find_pdx function to remaining rows
    remaining_indices <- which(is.na(pdx))
    for (i in remaining_indices) {
      result <- find_pdx(
        clin_c1_char[i], clin_c2_char[i], clin_icd_char[i], acc_pdx_env
      )
      pdx[i] <- result$pdx
      pdx_code[i] <- result$pdx_code
    }

    return(list(pdx = pdx, pdx_code = pdx_code))
  }

  pdx_results <- find_pdx_vectorized(
    datatable$clin_c1,
    datatable$clin_c2,
    datatable$clin_icd
  )
  datatable[, pdx := pdx_results$pdx]
  datatable[, pdx_code := pdx_results$pdx_code]

  return(list(pdx = datatable$pdx, pdx_code = datatable$pdx_code))
}

# Function to generate date of birth (DOB) vectorized
generate_dob <- function(bdays, ages, date_adms) {
  #' @title Generate Date of Birth Vectorized
  #'
  #' @description This function generates a vector of dates of birth
  #' (DOB) based on birthdates, ages, and admission dates.
  #'
  #' @param bdays character. A vector of birthdates in string format.
  #' @param ages numeric. A vector of ages.
  #' @param date_adms character. A vector of admission dates in string format.
  #'
  #' @return character. A vector of dates of birth in "dd/mm/yyyy" format.

  require(lubridate)

  # Ensure ages are numeric
  ages <- as.numeric(ages)

  dob <- rep(NA_character_, length(ages))

  # Use provided birthdates where available
  valid_bdays_indices <- !is.na(bdays) & bdays != ""
  dob[valid_bdays_indices] <- format(
    mdy(bdays[valid_bdays_indices]),
    "%d/%m/%Y"
  )

  # Identify indices where birthdates are missing
  missing_bday_indices <- which(is.na(bdays) | bdays == "")
  ref_dates <- mdy(date_adms[missing_bday_indices])

  # Handle cases where ages are zero:
  # For age 0, generate a random date within the past 27 days
  # from the admission date.
  zero_age_indices <- which(
    !is.na(ages[missing_bday_indices]) & ages[missing_bday_indices] == 0
  )
  dob[missing_bday_indices[zero_age_indices]] <- format(
    ref_dates[zero_age_indices] - days(
      sample(
        1:27, length(zero_age_indices),
        replace = TRUE
      )
    ), "%d/%m/%Y"
  )

  # Handle cases where ages are positive:
  # For positive ages, subtract the truncated age in years and a random
  # number of days (up to 170) from the admission date.
  positive_age_indices <- which(
    !is.na(ages[missing_bday_indices]) & ages[missing_bday_indices] > 0
  )
  truncated_ages <- floor(
    ages[missing_bday_indices][positive_age_indices]
  )
  dob[missing_bday_indices[positive_age_indices]] <- format(
    ref_dates[positive_age_indices] - years(truncated_ages) - days(
      sample(1:170, length(positive_age_indices), replace = TRUE)
    ), "%d/%m/%Y"
  )

  # # Check that all years for dates are above 1900
  # years <- year(mdy(dob))
  # if (any(years < 1900)) {
  #   stop("Generated dates have years below 1900")
  # }

  return(dob)
}

# Function to prepare and write output
prepare_and_write_output <- function(output_dt, output_txt_file) {
  #' @title Prepare and Write Output
  #'
  #' @description This function prepares and writes a data table to a file,
  #' converting list columns to comma-separated strings.
  #'
  #' @param output_dt data.table. The output data table.
  #' @param output_txt_file character. The path to the output text file.
  #'
  #' @return NULL.

  # Replace NA values with '--'
  output_dt[is.na(output_dt)] <- "--"
  # Convert list columns to comma-separated strings
  for (col in names(output_dt)) {
    if (is.list(output_dt[[col]])) {
      output_dt[[col]] <- sapply(output_dt[[col]], paste, collapse = ",")
    }
  }
  # Write the data.table to a file
  fwrite(output_dt, output_txt_file, sep = "|", col.names = TRUE)
}

# Function to export data for batch grouper
export_for_grouper <- function(dt, year_to_load, output_txt_file) {
  #' @title Export Data for Batch Grouper
  #'
  #' @description This function exports data for batch grouper,
  #' generating necessary columns and formatting them accordingly.
  #'
  #' @param dt data.table. The input data table.
  #' @param year_to_load integer. The year to load.
  #' @param output_txt_file character. The path to the output text file.
  #'
  #' @return NULL.

  output_dt <- data.table(CASEID = 1:nrow(dt))
  output_dt[, DOB := generate_dob(dt$pat_bdate, dt$pat_age, dt$date_adm)]
  output_dt[, Sex := ifelse(dt$pat_sex == "M", 1, 2)]
  output_dt[, DateAdm := format(mdy(dt$date_adm), "%d/%m/%Y")]
  output_dt[, TimeAdm := gsub(":", "", dt$time_adm)]
  output_dt[, DateDsc := format(mdy(dt$date_dis), "%d/%m/%Y")]
  output_dt[, TimeDsc := gsub(":", "", dt$time_dis)]
  output_dt[, DischT := dt$clin_discharge]
  output_dt[, AdmWt := dt$pat_bwt]
  output_dt[, PDx := dt$pdx]

  icd_codes_list <- lapply(dt$clin_icd, function(icd_str) {
    codes <- unlist(icd_str)
    length(codes) <- 12
    codes
  })
  icd_codes <- as.data.table(do.call(rbind, icd_codes_list))
  icd_cols <- paste0("SDx", 1:12)
  output_dt[, (icd_cols) := icd_codes]

  rvs_codes_list <- lapply(dt$icd9_list, function(rvs_str) {
    codes <- unlist(rvs_str)
    length(codes) <- 20
    codes
  })
  rvs_codes <- as.data.table(do.call(rbind, rvs_codes_list))
  proc_cols <- paste0("Proc", 1:20)
  output_dt[, (proc_cols) := rvs_codes]

  prepare_and_write_output(output_dt, output_txt_file)

  if (to_dec_mem_usage) rm(output_dt) # debug
  if (to_dec_mem_usage) gc() # debug
  if (to_debug) {
    return(NULL)
  } # debug
}

group_data <- function(to_group, part, dt) {
  #' @title Group data for batch processing
  #'
  #' @description This function groups the data for batch processing
  #' and exports it for the batch grouper.
  #'
  #' @param part integer. The part number of the data being processed.
  #' @param dt data.table. The data table to be grouped.
  #'
  #' @return NULL. The function is used for its side effect of
  #' grouping and exporting the data.

  if (to_group) {
    export_for_grouper(
      dt, year_to_load,
      output_txt_file(part)
    )
    if (to_dec_mem_usage) rm(dt) # debug
    if (to_dec_mem_usage) gc() # debug
    for_batch_grouping <- fread(
      output_txt_file(part),
      sep = "|", na.strings = "--"
    )
    if (file.exists(grouper_result_file(part))) {
      batch_grouping_result <- fread(
        grouper_result_file(part),
        sep = "|", na.strings = "--"
      )
    }
  }
}
