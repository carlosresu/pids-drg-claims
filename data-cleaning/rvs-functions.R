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
  cat(sprintf('There are %d RVS codes without an ICD-9CM equivalent recognized by the TDRG ICD9CM\n', length(unique(without_drg$rvs))))
  
  mappable_rvs <- intersect(rvss, rvs_icd9$rvs)
  cat(sprintf('Of these, %d (%.2f%%) have a mapping to an ICD-9-CM code.\n', length(mappable_rvs), length(mappable_rvs) * 100 / length(rvss)))
  
  multi_mapped_rvs <- intersect(rvss, names(rvs_map_list))
  cat(sprintf('Of these, there are %d (%.2f%%) with more than one ICD9 equivalent recognized by the Thai ICD9 library.\n', length(multi_mapped_rvs), length(multi_mapped_rvs) * 100 / length(mappable_rvs)))
  
  unmappable_rvs <- setdiff(rvss, mappable_rvs)
  cat(sprintf('There are %d (%.2f%%) with no ICD-9-CM equivalents.\n', length(unmappable_rvs), length(unmappable_rvs) * 100 / length(rvss)))
}

# Main function to process RVS code mappings
process_rvs_code_mapping <- function(dt, rvs_icd9) {
  #' @title Process RVS Code Mappings
  #' @description Processes RVS code mappings to ICD-9-CM codes in a data.table.
  #' @param dt A data.table to process.
  #' @param rvs_icd9 A data.table containing RVS to ICD-9-CM code mappings.
  #' @return The modified data.table with processed RVS to ICD-9-CM code mappings.
  
  # Split RVS codes into with and without DRG
  split_codes <- split_rvs_codes(rvs_icd9)
  with_drg <- split_codes$with_drg
  without_drg <- split_codes$without_drg
  
  # Create RVS to ICD-9-CM mapping lists
  rvs_maps <- create_rvs_map_lists(with_drg)
  rvs_map_list <- rvs_maps$rvs_map_list
  rvs_map_solo <- rvs_maps$rvs_map_solo
  
  # Get unique RVS codes from claims
  rvss <- unique(unlist(dt$clin_rvs))
  rvss <- intersect(rvss, rvs_icd9$rvs)
  
  # Print summary statistics
  print_summary_statistics(rvss, rvs_icd9, rvs_map_list)
  
  # Process claims data
  dt2 <- dt[lengths(clin_rvs) > 0]
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
  
  # Merge results back into the original data.table
  setkey(dt, id_series)
  setkey(dt2, id_series)
  dt <- merge(dt, dt2[, .(id_series, icd9_list, rvs_unmap_list)], by = "id_series", all.x = TRUE)
  
  return(dt)
}

# Helper function to find and append 5-digit codes
find_and_append_codes <- function(clin_rvs, col, regex_5_digit) {
  codes_to_append <- regmatches(col, gregexpr(regex_5_digit, col))[[1]]
  clin_rvs <- c(clin_rvs, codes_to_append)
  return(clin_rvs)
}

# Helper function to remove 5-digit codes
remove_5_digit_codes <- function(col, regex_5_digit) {
  col <- gsub(regex_5_digit, "", col)
  return(col)
}

append_and_remove_rvs <- function(clin_rvs, col) {
  #' @title Append and Remove 5-Digit Codes
  #' @description Appends and removes 5-digit numeric codes (i.e. 5-digit RVS procedure codes) from a specified column.
  #' @param clin_rvs A list of character vectors representing the clin_rvs column.
  #' @param col A character vector representing the column to process.
  #' @return A list containing the modified clin_rvs and the modified col.
  #'
  #' @details
  #' This function performs the following operations:
  #' - Ensures the clin_rvs column is a list of characters.
  #' - Finds and appends 5-digit numeric codes from the specified column to the clin_rvs column.
  #' - Removes 5-digit numeric codes from the specified column.
  #'
  #' @examples
  #' clin_rvs <- list(c("A", "B"), c("C", "D"))
  #' col <- c("12345 E", "67890 F")
  #' result <- append_and_remove_rvs(clin_rvs, col)
  #' print(result$clin_rvs)  # Should print modified clin_rvs with 5-digit codes appended
  #' print(result$col)  # Should print the modified col with 5-digit codes removed
  
  # Ensure the clin_rvs column is a list of characters
  clin_rvs <- lapply(clin_rvs, function(x) if (is.null(x)) character() else x)
  
  # Regular expression to match 5-digit numeric codes
  regex_5_digit <- "\\b\\d{5}\\b" 
  
  # Find and append 5-digit codes, and remove them from the specified column
  modified_clin_rvs <- mapply(find_and_append_codes, clin_rvs, col, MoreArgs = list(regex_5_digit = regex_5_digit), SIMPLIFY = FALSE)
  modified_col <- lapply(col, remove_5_digit_codes, regex_5_digit = regex_5_digit)
  
  return(list(clin_rvs = modified_clin_rvs, col = unlist(modified_col)))
}
