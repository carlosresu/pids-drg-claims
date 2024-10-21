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
