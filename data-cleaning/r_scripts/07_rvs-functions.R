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
