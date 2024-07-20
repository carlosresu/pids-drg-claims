split_rvs_codes <- function(rvs_icd9) {
  with_drg <- rvs_icd9[is_drg == TRUE]
  without_drg <- rvs_icd9[!rvs %in% with_drg$rvs]
  return(list(with_drg = with_drg, without_drg = without_drg))
}

create_rvs_map_lists <- function(with_drg) {
  with_drg <- with_drg[order(rvs, -is_drg)]
  unique_rvs <- unique(with_drg$rvs)
  rvs_grouped <- split(with_drg, with_drg$rvs)

  rvs_map_list <- list()
  rvs_map_solo <- list()

  for (r in unique_rvs) {
    sub <- rvs_grouped[[r]]
    if (nrow(sub) == 1) {
      rvs_map_solo[[r]] <- sub$icd9cm[1]
    } else {
      rvs_map_list[[r]] <- sub$icd9cm
    }
  }

  return(list(rvs_map_list = rvs_map_list, rvs_map_solo = rvs_map_solo))
}

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

map_rvs_icd9 <- function(clin_rvs, rvs_icd9) {
  split_codes <- split_rvs_codes(rvs_icd9)
  rvs_maps <- create_rvs_map_lists(split_codes$with_drg)

  rvs_map_solo_env <- as.environment(rvs_maps$rvs_map_solo)
  icd9_list <- get_icd9_codes(clin_rvs, rvs_map_solo_env)

  return(list(icd9_list = icd9_list, rvs_map_list = rvs_maps$rvs_map_list))
}

find_and_append_valid_rvs <- function(dt, valid_rvs_codes) {
  regex_5_digit <- "\\b\\d{5}\\b"
  dt[, matches := regmatches(col, gregexpr(regex_5_digit, col))]
  dt[, valid_matches := lapply(matches, function(x) x[x %in% valid_rvs_codes])]
  dt[, clin_rvs := lapply(
    seq_along(clin_rvs),
    function(i) unique(c(clin_rvs[[i]], dt$valid_matches[[i]]))
  )]
}

remove_5_digit_codes <- function(col) {
  regex_5_digit <- "\\b\\d{5}\\b"
  lapply(col, function(x) gsub(regex_5_digit, "", x))
}

warn_invalid_rvs <- function(matches, valid_rvs_codes) {
  invalid_matches <- lapply(matches, function(x) x[!x %in% valid_rvs_codes])
  discarded_codes <- unlist(invalid_matches)
  if (length(discarded_codes) > 0) {
    discarded_table <- data.table(
      CODE = discarded_codes
    )[, .N, by = CODE][order(-N)]
    # Change column names here
    names(discarded_table) <- c("CODE", "count")
  } else {
    discarded_table <- data.table()
  }
  return(discarded_table)
}

append_and_remove_rvs <- function(clin_rvs, col, rvs_icd9) {
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


