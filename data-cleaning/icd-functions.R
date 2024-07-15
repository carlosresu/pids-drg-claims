remove_lumped_icd_codes <- function(column) {
  #' @title Remove Lumped ICD Codes
  #' @description Removes lumped ICD codes from a specified column.
  #' @param column A character vector representing the column to process.
  #' @return The modified column with lumped ICD codes removed.
  #'
  #' @details
  #' This function processes the specified column to remove lumped ICD codes by adding "||" between numeric and alphabetic characters.
  
  modified_column <- gsub("(?<=\\d)(?=[A-Za-z])", "||", column, perl = TRUE)
  return(modified_column)
}

transfer_extra_icd10s_to_clin_icd <- function(clin_icd, col) {
  #' @title Transfer Extra ICD-10 Codes to clin_icd
  #' @description Transfers Extra ICD-10 codes in a specified column of a data.table to the appropriate list column.
  #' @param clin_icd A list of character vectors representing the clin_icd column.
  #' @param col A list of character vectors representing the column to process.
  #' @return A list containing the modified clin_icd and the first elements of col.
  #'
  #' @details
  #' This function processes the specified column by performing the following operations:
  #' - Ensures the column is a list of characters.
  #' - For rows with more than one element, splits and assigns the first element to the specified column.
  #' - Returns the modified clin_icd and the first elements of col.
  
  clin_icd <- lapply(clin_icd, function(x) if (is.null(x)) character() else x)
  col_first <- lapply(col, function(x) x[1])
  
  clin_icd <- mapply(function(icd, c1) c(icd, c1[-1]), clin_icd, col, SIMPLIFY = FALSE)
  
  return(list(clin_icd = clin_icd, col_first = col_first))
}

# Helper function to extract unique ICD codes from a data.table
get_unique_icd_codes <- function(dt) {
  #' @title Get Unique ICD Codes
  #' @description Extracts unique ICD codes from a data.table.
  #' @param dt A data.table containing ICD codes.
  #' @return A unique vector of ICD codes.
  icds <- unique(c(unlist(dt$clin_c1), unlist(dt$clin_c2), unlist(dt$clin_icd)))
  icds <- icds[!is.na(icds)]
  return(icds)
}

# Helper function to create an environment for Thai ICD-10 codes
create_thai_icd10_environment <- function(thai_icd10_codes) {
  #' @title Create Thai ICD-10 Environment
  #' @description Creates an environment for Thai ICD-10 codes.
  #' @param thai_icd10_codes A vector of Thai ICD-10 codes.
  #' @return An environment with Thai ICD-10 codes as keys.
  thai_icd10_env <- list2env(setNames(as.list(rep(TRUE, length(thai_icd10_codes))), thai_icd10_codes))
  return(thai_icd10_env)
}

# Helper function to identify direct matches of ICD codes in the Thai ICD-10 library
find_direct_icd_matches <- function(icds, thai_icd10_env) {
  #' @title Find Direct ICD Matches
  #' @description Identifies direct matches of ICD codes in the Thai ICD-10 library.
  #' @param icds A vector of ICD codes.
  #' @param thai_icd10_env An environment with Thai ICD-10 codes.
  #' @return A vector of directly matched ICD codes.
  direct_matches <- mget(icds, thai_icd10_env, ifnotfound = as.list(rep(FALSE, length(icds))))
  direct_match_codes <- names(unlist(direct_matches[unlist(direct_matches) == TRUE]))
  return(direct_match_codes)
}

# Helper function to create ICD-10 mapping
generate_icd10_mapping <- function(icds, thai_icd10_env, neoplasms_env) {
  #' @title Generate ICD-10 Mapping
  #' @description Creates a mapping of ICD-10 codes to Thai ICD-10 equivalents.
  #' @param icds A vector of ICD codes.
  #' @param thai_icd10_env An environment with Thai ICD-10 codes.
  #' @param neoplasms_env An environment with neoplasm codes.
  #' @return A list containing the ICD-10 mapping and the count of modified codes.
  icd_mapping <- list()
  modified_count <- 0
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
  return(list(icd_mapping = icd_mapping, modified_count = modified_count))
}

# Helper function to map ICD-10 codes in a data.table
apply_icd10_mapping_to_dt <- function(dt, icd10_env) {
  #' @title Apply ICD-10 Mapping to Data Table
  #' @description Maps ICD-10 codes in a data.table.
  #' @param dt A data.table containing ICD codes to map.
  #' @param icd10_env An environment with ICD-10 mappings.
  map_icd10_helper <- function(codes) {
    mapped <- mget(codes, icd10_env, ifnotfound = as.list(codes))
    return(unname(unlist(mapped)))
  }
  
  dt[, clin_c1 := lapply(clin_c1, map_icd10_helper)]
  dt[, clin_c2 := lapply(clin_c2, map_icd10_helper)]
  dt[, clin_icd := lapply(clin_icd, map_icd10_helper)]
  
  return(dt)
}

# Main function to process ICD-10 mappings
implement_icd10_mapping <- function(dt) {
  #' @title Process ICD-10 Mappings
  #' @description Applies ICD-10 mappings in a data.table using the Thai ICD-10 library.
  #' @param dt A data.table to process.
  #' @return The modified data.table with applied ICD-10 mappings.
  
  # Extract unique ICD codes
  icds <- get_unique_icd_codes(dt)
  thai_icd10 <- unique(tdrg_icd10$CODE)
  
  # Create environments for Thai ICD-10 codes and neoplasms
  thai_icd10_env <- create_thai_icd10_environment(thai_icd10)
  neoplasms <- unique(tdrg_icd10[grepl("/", tdrg_icd10$CODE), "CODE"])
  neoplasms_env <- create_thai_icd10_environment(neoplasms)
  
  # Identify direct matches
  direct_match_codes <- find_direct_icd_matches(icds, thai_icd10_env)
  cat(sprintf("There are %d unique entries for ICD-10 codes, of which %d (%.2f%%) are directly in the Thai ICD-10 library\n", length(icds), length(direct_match_codes), length(direct_match_codes) * 100 / length(icds)))
  
  # Create ICD-10 mapping
  icd_mapping_info <- generate_icd10_mapping(icds, thai_icd10_env, neoplasms_env)
  icd_mapping <- icd_mapping_info$icd_mapping
  modified_count <- icd_mapping_info$modified_count
  cat(sprintf('The modifications led to a total of %d codes being mapped to an equivalent in the Thai ICD10 library.\n', length(icd_mapping)))
  cat(sprintf('Out of these, %d were modified to match.\n', modified_count))
  
  # Identify unmatched ICD codes
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
  
  # Create ICD-10 mapping data.table and environment
  icd10_map <- data.table(phl_icd10 = names(icd_mapping), tdrg_icd10 = unlist(icd_mapping))
  fwrite(icd10_map, here(path_to_cache, paste0("icd10_map_file_", year_to_load, ".csv")))
  icd10_env <- list2env(setNames(as.list(icd10_map$tdrg_icd10), icd10_map$phl_icd10))
  
  # Map ICD-10 codes in the data.table
  dt <- apply_icd10_mapping_to_dt(dt, icd10_env)
  return(dt)
}

deduplicate_and_ensure_unique_icd_codes <- function(clin_c1, clin_c2, clin_icd) {
  #' @title Deduplicate and Ensure Unique Entries Across Columns
  #' @description Deduplicates and ensures unique entries across clin_c1, clin_c2, and clin_icd columns within each row.
  #' @param clin_c1 A list of vectors representing the clin_c1 column.
  #' @param clin_c2 A list of vectors representing the clin_c2 column.
  #' @param clin_icd A list of vectors representing the clin_icd column.
  #' @return A list containing the modified clin_c1, clin_c2, and clin_icd columns.
  #'
  #' @details
  #' This function performs the following operations:
  #' - Deduplicates each specified column within each row.
  #' - Ensures that entries in clin_c1 are not found in clin_c2 or clin_icd within each row.
  #' - Ensures that entries in clin_c2 are not found in clin_c1 or clin_icd within each row.
  #' - Ensures that entries in clin_icd are not found in clin_c1 or clin_c2 within each row.
  
  # Deduplicate each column within each row
  clin_c1 <- lapply(clin_c1, unique)
  clin_c2 <- lapply(clin_c2, unique)
  clin_icd <- lapply(clin_icd, unique)
  
  # Ensure unique entries across columns within each row
  unique_clin_icd <- mapply(function(c1, c2, icd) setdiff(icd, union(c1, c2)), clin_c1, clin_c2, clin_icd, SIMPLIFY = FALSE)
  
  # Ensure that clin_c1 and clin_c2 are unique within their columns
  unique_clin_c1 <- mapply(function(c1, c2, icd) setdiff(c1, c2), clin_c1, clin_c2, SIMPLIFY = FALSE)
  unique_clin_c2 <- mapply(function(c1, c2, icd) setdiff(c2, c1), clin_c1, clin_c2, SIMPLIFY = FALSE)
  
  return(list(clin_c1 = unique_clin_c1, clin_c2 = unique_clin_c2, clin_icd = unique_clin_icd))
}