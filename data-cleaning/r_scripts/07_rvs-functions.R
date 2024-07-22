# Function to split RVS codes
split_rvs_codes <- function(rvs_icd9) {
  #' @title Split RVS codes
  #'
  #' @description This function splits RVS codes into
  #' those with DRG and those without DRG.
  #'
  #' @param rvs_icd9 data.frame The input data containing RVS to ICD-9 mappings.
  #'
  #' @return list A list containing two data.frames:
  #' one with DRG and one without DRG.

  with_drg <- rvs_icd9[is_drg == TRUE]
  without_drg <- rvs_icd9[!rvs %in% with_drg$rvs]
  return(list(with_drg = with_drg, without_drg = without_drg))
}

# Function to create RVS map lists
create_rvs_map_lists <- function(with_drg) {
  #' @title Create RVS map lists
  #'
  #' @description This function creates mapping lists for RVS codes,
  #' separating them into solo and list-mapped categories.
  #'
  #' @param with_drg data.frame The input data containing RVS codes with DRG.
  #'
  #' @return list A list containing RVS map lists for solo and
  #' list-mapped categories.

  setorder(with_drg, rvs, -is_drg)
  unique_rvs <- with_drg[, .(icd9cm_list = list(icd9cm)), by = rvs]
  solo <- unique_rvs[lengths(icd9cm_list) == 1]
  list_mapped <- unique_rvs[lengths(icd9cm_list) > 1]
  rvs_map_solo <- setNames(solo$icd9cm_list, solo$rvs)
  rvs_map_list <- setNames(list_mapped$icd9cm_list, list_mapped$rvs)
  return(list(rvs_map_list = rvs_map_list, rvs_map_solo = rvs_map_solo))
}

# Function to get ICD-9 codes from clinical RVS
get_icd9_codes <- function(clin_rvs, rvs_map_solo) {
  #' @title Get ICD-9 codes from clinical RVS
  #'
  #' @description This function retrieves ICD-9 codes from
  #' clinical RVS codes using the provided RVS map.
  #'
  #' @param clin_rvs list The input list of clinical RVS codes.
  #' @param rvs_map_solo list The mapping of RVS codes to ICD-9 codes.
  #'
  #' @return list A list of ICD-9 codes corresponding to the clinical RVS codes.

  lapply(clin_rvs, function(x) {
    codes <- unlist(x)
    mappable <- codes[codes %in% names(rvs_map_solo)]
    if (length(mappable) > 0) {
      unique(unlist(rvs_map_solo[mappable]))
    } else {
      NA_character_
    }
  })
}

# Function to map RVS to ICD-9
map_rvs_icd9 <- function(clin_rvs, rvs_icd9) {
  #' @title Map RVS to ICD-9
  #'
  #' @description This function maps RVS codes to ICD-9 codes
  #' using the provided mappings.
  #'
  #' @param clin_rvs list The input list of clinical RVS codes.
  #' @param rvs_icd9 data.frame The input data containing RVS to ICD-9 mappings.
  #'
  #' @return list A list containing the ICD-9 list, RVS map lists,
  #' and details about the mapping process.

  split_codes <- split_rvs_codes(rvs_icd9)
  rvs_maps <- create_rvs_map_lists(split_codes$with_drg)
  rvs_map_list <- rvs_maps$rvs_map_list
  rvs_map_solo <- rvs_maps$rvs_map_solo
  icd9_list <- get_icd9_codes(clin_rvs, rvs_map_solo)
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
find_and_append_valid_rvs <- function(dt, valid_rvs_codes) {
  #' @title Find and append valid RVS codes
  #'
  #' @description This function finds and appends valid
  #' RVS codes to the provided data table.
  #'
  #' @param dt data.table The input data table.
  #' @param valid_rvs_codes character The list of valid RVS codes.

  regex_5_digit <- "\\b\\d{5}\\b"
  valid_rvs_set <- unique(valid_rvs_codes)
  dt[
    ,
    matches := lapply(
      regmatches(col, gregexpr(regex_5_digit, col)),
      function(x) x[x %in% valid_rvs_set]
    )
  ]
  dt[
    ,
    clin_rvs := mapply(
      function(rvs, matches) unique(c(rvs, matches)),
      clin_rvs, matches,
      SIMPLIFY = FALSE
    )
  ]
}

# Function to remove 5-digit codes
remove_5_digit_codes <- function(col) {
  #' @title Remove 5-digit codes
  #'
  #' @description This function removes 5-digit codes from the provided column.
  #'
  #' @param col character The input column from which to remove 5-digit codes.
  #'
  #' @return list A list with 5-digit codes removed.

  regex_5_digit <- "\\b\\d{5}\\b"
  lapply(col, function(x) gsub(regex_5_digit, "", x))
}

# Function to warn about invalid RVS codes
warn_invalid_rvs <- function(matches, valid_rvs_codes) {
  #' @title Warn about invalid RVS codes
  #'
  #' @description This function warns about invalid RVS codes
  #' and returns a table of discarded codes.
  #'
  #' @param matches list The list of matches to check for invalid RVS codes.
  #' @param valid_rvs_codes character The list of valid RVS codes.
  #'
  #' @return data.table A table of discarded invalid RVS codes.

  valid_rvs_set <- unique(valid_rvs_codes)
  invalid_matches <- lapply(matches, function(x) setdiff(x, valid_rvs_set))
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
  #' @title Append and remove RVS codes
  #'
  #' @description This function appends valid RVS codes and
  #' removes invalid 5-digit codes from the provided columns.
  #'
  #' @param clin_rvs list The list of clinical RVS codes.
  #' @param col character The input column containing codes.
  #' @param rvs_icd9 data.frame The input data containing RVS to ICD-9 mappings.
  #'
  #' @return list A list containing updated clinical RVS codes,
  #' the modified column, and a table of discarded invalid RVS codes.

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
