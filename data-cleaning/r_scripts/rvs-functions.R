source(here("data-cleaning", "r_scripts", "libraries.R"))

# Helper function to split RVS codes into with and without DRG
split_rvs_codes <- function(rvs_icd9) {
  #' @title Split RVS Codes
  #' @description Splits RVS codes into those with and without DRG.
  #' @param rvs_icd9 A data.table containing RVS to ICD-9-CM code mappings.
  #' @return A list containing two data.tables: with_drg and without_drg.
  with_drg <- rvs_icd9[is_drg == TRUE]
  without_drg <- rvs_icd9[!rvs %in% with_drg$rvs]
  return(list(with_drg = with_drg, without_drg = without_drg))
}

# Helper function to create RVS to ICD-9-CM mapping lists
create_rvs_map_lists <- function(with_drg) {
  #' @title Create RVS Map Lists
  #' @description Creates mapping lists for RVS to ICD-9-CM codes.
  #' @param with_drg A data.table containing RVS codes with DRG.
  #' @return A list containing two lists: rvs_map_list and rvs_map_solo.
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

# Helper function to print summary statistics
print_summary_statistics <- function(rvss, rvs_icd9, rvs_map_list) {
  #' @title Print Summary Statistics
  #' @description Prints summary statistics for RVS to ICD-9-CM mappings.
  #' @param rvss A vector of unique RVS codes that appear in the claims.
  #' @param rvs_icd9 A data.table containing RVS to ICD-9-CM code mappings.
  #' @param rvs_map_list A list of RVS codes with multiple ICD-9-CM mappings.
  without_drg <- rvs_icd9[!rvs %in% names(rvs_map_list)]
  cat(
    sprintf(
      "There are %d",
      length(unique(without_drg$rvs))
    ),
    "RVS codes without an ICD-9CM equivalent recognized by the TDRG ICD9CM\n"
  )

  mappable_rvs <- intersect(rvss, rvs_icd9$rvs)
  cat(
    sprintf(
      "Of these, %d (%.2f%%) have a mapping to an ICD-9-CM code.\n",
      length(mappable_rvs), length(mappable_rvs) * 100 / length(rvss)
    )
  )

  multi_mapped_rvs <- intersect(rvss, names(rvs_map_list))
  cat(
    sprintf(
      "Of these, there are %d (%.2f%%)",
      length(multi_mapped_rvs),
      length(multi_mapped_rvs) * 100 / length(mappable_rvs)
    ),
    "with more than one ICD9 equivalent recognized by the Thai ICD9 library.\n"
  )

  unmappable_rvs <- setdiff(rvss, mappable_rvs)
  cat(
    sprintf(
      "There are %d (%.2f%%) with no ICD-9-CM equivalents.\n",
      length(unmappable_rvs), length(unmappable_rvs) * 100 / length(rvss)
    )
  )
}

# Main function to process RVS code mappings
map_rvs_icd9 <- function(clin_rvs, rvs_icd9) {
  #' @title Process RVS Code Mappings
  #' @description Processes RVS code mappings to ICD-9-CM codes.
  #' @param clin_rvs A list of character vectors representing
  #' clinical RVS codes.
  #' @param rvs_icd9 A data.table containing RVS to ICD-9-CM code mappings.
  #' @return A list of ICD-9-CM code mappings for each RVS code.

  # Split RVS codes into with and without DRG
  split_codes <- split_rvs_codes(rvs_icd9)
  with_drg <- split_codes$with_drg
  without_drg <- split_codes$without_drg

  # Create RVS to ICD-9-CM mapping lists
  rvs_maps <- create_rvs_map_lists(with_drg)
  rvs_map_list <- rvs_maps$rvs_map_list
  rvs_map_solo <- rvs_maps$rvs_map_solo

  # Get unique RVS codes from claims
  rvss <- unique(unlist(clin_rvs))
  rvss <- intersect(rvss, rvs_icd9$rvs)

  # Print summary statistics
  print_summary_statistics(rvss, rvs_icd9, rvs_map_list)

  # Process claims data
  rvs_map_solo_env <- as.environment(rvs_map_solo)
  rvs_map_list_env <- as.environment(rvs_map_list)

  icd9_list <- lapply(clin_rvs, function(x) {
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

  return(icd9_list)
}

append_and_remove_rvs <- function(clin_rvs, col, rvs_icd9) {
  #' @title Append and Remove 5-Digit Codes
  #' @description Appends and removes 5-digit numeric codes (i.e.
  #' 5-digit RVS procedure codes) from a specified column.
  #' @param clin_rvs A list of character vectors representing the clin_
  #' rvs column.
  #' @param col A character vector representing the column to process.
  #' @param rvs_icd9 A data.table containing valid RVS codes in the
  #' column 'rvs'.
  #' @return A list containing the modified clin_rvs and the modified col.

  # Create a data.table for efficient processing
  dt <- data.table(clin_rvs = clin_rvs, col = col)

  # Extract valid RVS codes and define the regex pattern for 5-digit codes
  valid_rvs_codes <- rvs_icd9$rvs
  regex_5_digit <- "\\b\\d{5}\\b"

  # Find and append valid 5-digit codes
  dt[, matches := regmatches(col, gregexpr(regex_5_digit, col))]
  dt[, valid_matches := lapply(matches, function(x) x[x %in% valid_rvs_codes])]
  dt[, clin_rvs := lapply(seq_along(clin_rvs), function(i) unique(c(clin_rvs[[i]], dt$valid_matches[[i]])))]

  # Remove 5-digit codes from the specified column
  dt[, col := lapply(col, function(x) gsub(regex_5_digit, "", x))]

  # Find invalid RVS codes and display a warning if any are found
  dt[, invalid_matches := lapply(matches, function(x) x[!x %in% valid_rvs_codes])]
  discarded_codes <- unique(unlist(dt$invalid_matches))
  if (length(discarded_codes) > 0) {
    warning("Discarded RVS codes: ", paste(discarded_codes, collapse = ", "))
  } else {
    print("No RVS codes discarded")
  }

  # Return the modified clin_rvs and col
  return(list(clin_rvs = dt$clin_rvs, col = dt$col))
}
