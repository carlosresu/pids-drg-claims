source(here("data-cleaning", "r_scripts", "libraries.R"))

# Helper function to remove lumped ICD codes
remove_lumped_icd_codes <- function(column) {
  #' @title Remove Lumped ICD Codes
  #' @description Removes lumped ICD codes from a specified column.
  #' @param column A character vector representing the column to process.
  #' @return The modified column with lumped ICD codes removed.

  modified_column <- gsub("(?<=\\d)(?=[A-Za-z])", "||", column, perl = TRUE)
  return(modified_column)
}

# Helper function to transfer extra ICD-10 codes to clin_icd
transfer_extra_icd10s_to_clin_icd <- function(clin_icd, col) {
  #' @title Transfer Extra ICD-10 Codes to clin_icd
  #' @description Transfers extra ICD-10 codes in a specified column of a data.table to the appropriate list column.
  #' @param clin_icd A list of character vectors representing the clin_icd column.
  #' @param col A list of character vectors representing the column to process.
  #' @return A list containing the modified clin_icd and the first elements of col.

  clin_icd <- lapply(clin_icd, function(x) if (is.null(x)) character() else x)
  col_first <- lapply(col, function(x) x[1])

  clin_icd <- mapply(function(icd, c1) {
    c(icd, c1[-1])
  }, clin_icd, col, SIMPLIFY = FALSE)

  return(list(clin_icd = clin_icd, col_first = col_first))
}

# Helper function to get unique ICD codes
get_unique_icd_codes <- function(clin_c1, clin_c2, clin_icd) {
  #' @title Get Unique ICD Codes
  #' @description Extracts unique ICD codes from specific columns.
  #' @param clin_c1 A list of character vectors containing ICD codes.
  #' @param clin_c2 A list of character vectors containing ICD codes.
  #' @param clin_icd A list of character vectors containing ICD codes.
  #' @return A unique vector of ICD codes.

  icds <- unique(c(unlist(clin_c1), unlist(clin_c2), unlist(clin_icd)))
  icds <- icds[!is.na(icds)]
  return(icds)
}

# Helper function to create Thai ICD-10 environment
create_thai_icd10_environment <- function(thai_icd10_codes) {
  #' @title Create Thai ICD-10 Environment
  #' @description Creates an environment for Thai ICD-10 codes.
  #' @param thai_icd10_codes A vector of Thai ICD-10 codes.
  #' @return An environment with Thai ICD-10 codes as keys.

  thai_icd10_env <- list2env(
    setNames(as.list(rep(TRUE, length(thai_icd10_codes))), thai_icd10_codes)
  )
  return(thai_icd10_env)
}

# Helper function to find direct ICD matches
find_direct_icd_matches <- function(icds, thai_icd10_env) {
  #' @title Find Direct ICD Matches
  #' @description Identifies direct matches of ICD codes in the Thai ICD-10 library.
  #' @param icds A vector of ICD codes.
  #' @param thai_icd10_env An environment with Thai ICD-10 codes.
  #' @return A vector of directly matched ICD codes.

  direct_matches <- mget(
    icds, thai_icd10_env,
    ifnotfound = as.list(rep(FALSE, length(icds)))
  )
  direct_match_codes <- names(
    unlist(direct_matches[unlist(direct_matches) == TRUE])
  )
  return(direct_match_codes)
}

# Helper function to generate ICD-10 mapping
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
apply_icd10_mapping_to_columns <- function(clin_c1, clin_c2, clin_icd, icd10_env) {
  #' @title Apply ICD-10 Mapping to Columns
  #' @description Maps ICD-10 codes in specific columns.
  #' @param clin_c1 A list of character vectors containing ICD codes.
  #' @param clin_c2 A list of character vectors containing ICD codes.
  #' @param clin_icd A list of character vectors containing ICD codes.
  #' @param icd10_env An environment with ICD-10 mappings.
  #' @return A list of modified columns with applied ICD-10 mappings.

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

# Main function to process ICD-10 mappings
implement_icd10_mapping <- function(clin_c1, clin_c2, clin_icd, tdrg_icd10, rows_to_show = Inf) {
  #' @title Process ICD-10 Mappings
  #' @description Applies ICD-10 mappings to specific columns using the Thai ICD-10 library.
  #' @param clin_c1 A list of character vectors containing ICD codes.
  #' @param clin_c2 A list of character vectors containing ICD codes.
  #' @param clin_icd A list of character vectors containing ICD codes.
  #' @param tdrg_icd10 A data.table containing Thai ICD-10 codes.
  #' @param rows_to_show The number of lines to display for unmatched ICD codes.
  #' @return A list of modified columns with applied ICD-10 mappings.

  # Extract unique ICD codes
  icds <- get_unique_icd_codes(clin_c1, clin_c2, clin_icd)

  # Create environments for Thai ICD-10 codes and neoplasms
  thai_icd10_env <- create_thai_icd10_environment(unique(tdrg_icd10$CODE))
  neoplasms_env <- create_thai_icd10_environment(unique(tdrg_icd10[grepl("/", tdrg_icd10$CODE), "CODE"]))

  # Identify direct matches
  direct_match_codes <- find_direct_icd_matches(icds, thai_icd10_env)
  cat(
    sprintf(
      "\n\nThere are %d unique entries for ICD-10 codes, of which %d (%.2f%%)",
      length(icds), length(direct_match_codes),
      length(direct_match_codes) * 100 / length(icds)
    ),
    " are directly in the Thai ICD-10 library\n"
  )

  # Create ICD-10 mapping
  icd_mapping_info <- generate_icd10_mapping(icds, thai_icd10_env, neoplasms_env)
  icd_mapping <- icd_mapping_info$icd_mapping
  modified_count <- icd_mapping_info$modified_count
  cat(sprintf(
    "The modifications led to a total of %d codes being mapped to an equivalent in the Thai ICD10 library.\n",
    length(icd_mapping)
  ))
  cat(sprintf("Out of these, %d were modified to match.\n", modified_count))

  # Identify unmatched ICD codes
  unmatched_icds <- setdiff(icds, names(icd_mapping))
  if (length(unmatched_icds) > 0) {
    cat(sprintf("There are %d codes that could not be mapped to the Thai ICD10 library:\n", length(unmatched_icds)))
    unmatched_sources <- data.table(code = unmatched_icds, source = NA_character_, count = 0)

    for (col_name in c("clin_c1", "clin_c2", "clin_icd")) {
      col_values <- get(col_name)
      unmatched_sources[code %in% unlist(col_values), source := col_name]
      unmatched_sources[code %in% unlist(col_values), count := count + table(unlist(col_values))[code]]
    }

    unmatched_sources <- unmatched_sources[order(-count)]
    print(kable(head(unmatched_sources, rows_to_show), format = "markdown", caption = "Unmapped ICD Codes"))
  }

  # Create ICD-10 mapping data.table and environment
  icd10_map <- data.table(phl_icd10 = names(icd_mapping), tdrg_icd10 = unlist(icd_mapping))
  fwrite(icd10_map, paste0("cache/icd10_map_file_", year_to_load, ".csv"))
  icd10_env <- list2env(setNames(as.list(icd10_map$tdrg_icd10), icd10_map$phl_icd10))

  # Map ICD-10 codes in the specific columns
  return(apply_icd10_mapping_to_columns(clin_c1, clin_c2, clin_icd, icd10_env))
}

# Helper function to ensure unique ICD codes across columns
ensure_unique_icd_codes <- function(clin_c1, clin_c2, clin_icd) {
  #' @title Ensure Unique ICD Codes
  #' @description Deduplicates and ensures unique entries across clin_c1, clin_c2, and clin_icd columns within each row.
  #' @param clin_c1 A list of vectors representing the clin_c1 column.
  #' @param clin_c2 A list of vectors representing the clin_c2 column.
  #' @param clin_icd A list of vectors representing the clin_icd column.
  #' @return A list containing the modified clin_c1, clin_c2, and clin_icd columns.

  # Convert lists to data.table for efficient processing
  dt <- data.table(clin_c1 = clin_c1, clin_c2 = clin_c2, clin_icd = clin_icd)

  # Deduplicate each column
  dt[, clin_c1 := lapply(clin_c1, unique)]
  dt[, clin_c2 := lapply(clin_c2, unique)]
  dt[, clin_icd := lapply(clin_icd, unique)]

  # Remove entries in clin_icd that are in clin_c1 or clin_c2
  dt[, clin_icd := Map(function(c1, c2, icd) setdiff(icd, union(c1, c2)), clin_c1, clin_c2, clin_icd)]

  # Remove entries in clin_c1 that are in clin_c2
  dt[, clin_c1 := Map(function(c1, c2) setdiff(c1, c2), clin_c1, clin_c2)]

  # Remove entries in clin_c2 that are in clin_c1
  dt[, clin_c2 := Map(function(c1, c2) setdiff(c2, c1), clin_c1, clin_c2)]

  return(list(clin_c1 = dt$clin_c1, clin_c2 = dt$clin_c2, clin_icd = dt$clin_icd))
}

# Helper function to generate a comparison table
generate_comparison_table <- function(original, modified) {
  #' @title Generate Comparison Table
  #' @description Generates a comparison table of original and modified ICD codes.
  #' @param original A list of original ICD codes.
  #' @param modified A list of modified ICD codes.
  #' @return A data.table with the comparison of original and modified ICD codes.

  original_unlisted <- unlist(original, use.names = FALSE)
  modified_unlisted <- unlist(modified, use.names = FALSE)

  comparison <- data.table(old_code = original_unlisted, new_code = modified_unlisted)

  comparison <- comparison[old_code != new_code, .(count = .N), by = .(old_code, new_code)]

  return(comparison)
}

# Helper function to pad list elements
pad_list_elements <- function(list1, list2) {
  #' @title Pad List Elements
  #' @description Pads list elements to ensure they have the same length.
  #' @param list1 A list of vectors.
  #' @param list2 A list of vectors.
  #' @return A list of padded lists.

  max_length <- max(lengths(list1), lengths(list2))

  pad_with_na <- function(lst, max_length) {
    lapply(lst, function(x) {
      if (length(x) < max_length) {
        x <- c(x, rep(NA, max_length - length(x)))
      }
      return(x)
    })
  }

  list1 <- pad_with_na(list1, max_length)
  list2 <- pad_with_na(list2, max_length)

  return(list(list1, list2))
}

# Main function to map then compare ICD mappings
map_then_compare_icd_mappings <- function(tdrg_icd10, rows_to_show = Inf, invalid_rows_to_show = Inf) {
  #' @title Map Then Compare ICD Mappings
  #' @description Processes and compares ICD-10 mappings.
  #' @param tdrg_icd10 A data.table containing Thai ICD-10 codes.
  #' @param rows_to_show The number of lines to display for unmatched ICD codes.
  #' @param invalid_rows_to_show The number of lines to display for invalid ICD codes.
  #' @return A list of modified columns with applied ICD-10 mappings.

  # Ensure the dt variable is in the global environment
  if (!exists("dt", envir = .GlobalEnv)) {
    stop("The global variable 'dt' does not exist.")
  }

  # Store the original data for comparison
  original_dt <- data.table::copy(dt)

  # Process ICD-10 mappings
  mapped_columns <- implement_icd10_mapping(
    original_dt$clin_c1, original_dt$clin_c2,
    original_dt$clin_icd, tdrg_icd10,
    rows_to_show = rows_to_show
  )

  # Update the global dt with mapped columns
  dt$clin_c1 <- mapped_columns$clin_c1
  dt$clin_c2 <- mapped_columns$clin_c2
  dt$clin_icd <- mapped_columns$clin_icd

  # Ensure unique ICD codes
  unique_icd_codes <- ensure_unique_icd_codes(
    dt$clin_c1, dt$clin_c2, dt$clin_icd
  )
  dt$clin_c1 <- unique_icd_codes$clin_c1
  dt$clin_c2 <- unique_icd_codes$clin_c2
  dt$clin_icd <- unique_icd_codes$clin_icd

  # Pad lists to ensure they have the same length
  padded_c1 <- pad_list_elements(original_dt$clin_c1, dt$clin_c1)
  original_dt$clin_c1 <- padded_c1[[1]]
  dt$clin_c1 <- padded_c1[[2]]

  padded_c2 <- pad_list_elements(original_dt$clin_c2, dt$clin_c2)
  original_dt$clin_c2 <- padded_c2[[1]]
  dt$clin_c2 <- padded_c2[[2]]

  padded_icd <- pad_list_elements(original_dt$clin_icd, dt$clin_icd)
  original_dt$clin_icd <- padded_icd[[1]]
  dt$clin_icd <- padded_icd[[2]]

  # Generate comparison table
  comparison_table <- rbind(
    generate_comparison_table(original_dt$clin_c1, dt$clin_c1),
    generate_comparison_table(original_dt$clin_c2, dt$clin_c2),
    generate_comparison_table(original_dt$clin_icd, dt$clin_icd)
  )

  # Sort the comparison table by count in descending order
  comparison_table <- comparison_table[order(-count)]

  # Print the kable output with a specified number of rows
  print(kable(head(comparison_table, rows_to_show),
    format = "markdown",
    caption = "Comparison of ICD Codes Before and After Mapping"
  ))

  # Check if all resulting ICD codes are in either the Thai library or the PhilHealth library
  all_icds <- unique(
    c(unlist(dt$clin_c1), unlist(dt$clin_c2), unlist(dt$clin_icd))
  )
  valid_icds <- unique(c(tdrg_icd10$CODE, rvs_icd9$icd9cm))
  invalid_icds <- setdiff(all_icds, valid_icds)
  invalid_icds <- invalid_icds[!is.na(invalid_icds) & invalid_icds != "NA"]

  if (length(invalid_icds) > 0) {
    invalid_icds_table <- data.table(
      code = invalid_icds,
      count = sapply(
        invalid_icds,
        function(icd) {
          sum(c(
            unlist(dt$clin_c1),
            unlist(dt$clin_c2),
            unlist(dt$clin_icd)
          ) == icd, na.rm = TRUE)
        }
      )
    )

    invalid_icds_table <- invalid_icds_table[!is.na(code) & code != ""]
    invalid_icds_table <- invalid_icds_table[order(-count)]

    print(kable(head(invalid_icds_table, invalid_rows_to_show),
      format = "markdown",
      caption = "Invalid ICD Codes Not Found in Thai or PhilHealth Libraries"
    ))
  } else {
    cat("All resulting ICD codes are valid and present in the libraries.\n")
  }
}
