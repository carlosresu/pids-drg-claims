# Function to split RVS codes
split_rvs_codes <- function(rvs_icd9) {
  #' @title Split RVS Codes
  #'
  #' @description This function splits RVS codes into those with and without DRG.
  #'
  #' @param rvs_icd9 data.table. The RVS ICD-9 codes.
  #'
  #' @return list. A list containing RVS codes with DRG and without DRG.

  with_drg <- rvs_icd9[is_drg == TRUE]
  without_drg <- rvs_icd9[!rvs %in% with_drg$rvs]
  return(list(with_drg = with_drg, without_drg = without_drg))
}

# Function to create RVS map lists
create_rvs_map_lists <- function(with_drg) {
  #' @title Create RVS Map Lists
  #'
  #' @description This function creates lists for RVS mapping with DRG.
  #'
  #' @param with_drg data.table. The RVS codes with DRG.
  #'
  #' @return list. A list containing RVS map list and RVS map solo.

  # Order the data by rvs and -is_drg
  setorder(with_drg, rvs, -is_drg)

  # Create a list of unique rvs
  unique_rvs <- with_drg[, .(icd9cm_list = list(icd9cm)), by = rvs]

  # Split into solo and list mappings
  solo <- unique_rvs[lengths(icd9cm_list) == 1]
  list_mapped <- unique_rvs[lengths(icd9cm_list) > 1]

  rvs_map_solo <- setNames(solo$icd9cm_list, solo$rvs)
  rvs_map_list <- setNames(list_mapped$icd9cm_list, list_mapped$rvs)

  return(list(rvs_map_list = rvs_map_list, rvs_map_solo = rvs_map_solo))
}

# Function to get ICD-9 codes from clinical RVS
get_icd9_codes <- function(clin_rvs, rvs_map_solo_env) {
  #' @title Get ICD-9 Codes from Clinical RVS
  #'
  #' @description This function retrieves ICD-9 codes for clinical RVS.
  #'
  #' @param clin_rvs list. The clinical RVS codes.
  #' @param rvs_map_solo_env environment. The environment with RVS map solo codes.
  #'
  #' @return list. The ICD-9 codes for clinical RVS.

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
  #' @title Map RVS to ICD-9
  #'
  #' @description This function maps RVS codes to ICD-9 codes.
  #'
  #' @param clin_rvs list. The clinical RVS codes.
  #' @param rvs_icd9 data.table. The RVS ICD-9 codes.
  #'
  #' @return list. A list containing the
  #' mapped ICD-9 codes and related information.

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

find_and_append_valid_rvs <- function(dt, valid_rvs_codes) {
  regex_5_digit <- "\\b\\d{5}\\b"
  valid_rvs_set <- unique(valid_rvs_codes)
  
  dt[, matches := lapply(regmatches(col, gregexpr(regex_5_digit, col)), function(x) x[x %in% valid_rvs_set])]
  dt[, clin_rvs := mapply(function(rvs, matches) unique(c(rvs, matches)), clin_rvs, matches, SIMPLIFY = FALSE)]
}

remove_5_digit_codes <- function(col) {
  regex_5_digit <- "\\b\\d{5}\\b"
  lapply(col, function(x) gsub(regex_5_digit, "", x))
}

warn_invalid_rvs <- function(matches, valid_rvs_codes) {
  valid_rvs_set <- unique(valid_rvs_codes)
  
  invalid_matches <- lapply(matches, function(x) setdiff(x, valid_rvs_set))
  discarded_codes <- unlist(invalid_matches)
  if (length(discarded_codes) > 0) {
    discarded_table <- data.table(CODE = discarded_codes)[, .N, by = CODE][order(-N)]
    setnames(discarded_table, c("CODE", "count"))
  } else {
    discarded_table <- data.table()
  }
  return(discarded_table)
}

# Function to append and remove RVS codes
append_and_remove_rvs <- function(clin_rvs, col, rvs_icd9) {
  #' @title Append and Remove RVS Codes
  #'
  #' @description This function appends valid RVS codes and removes invalid RVS codes.
  #'
  #' @param clin_rvs list. The clinical RVS codes.
  #' @param col character. The column with RVS codes.
  #' @param rvs_icd9 data.table. The RVS ICD-9 codes.
  #'
  #' @return list. A list containing the updated clinical RVS, column, and discarded RVS codes.

  dt <- data.table(clin_rvs = clin_rvs, col = col)
  valid_rvs_codes <- rvs_icd9$rvs

  find_and_append_valid_rvs(dt, valid_rvs_codes)
  dt[, col := remove_5_digit_codes(col)]
  discarded_rvs <- warn_invalid_rvs(dt$matches, valid_rvs_codes)

  return(
    list(
      clin_rvs = dt$clin_rvs,
      col = dt$col,
      discarded_rvs = discarded_rvs
    )
  )
}
