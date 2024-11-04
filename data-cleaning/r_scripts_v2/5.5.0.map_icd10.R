map_icd10 <- function(c1, c2, clin_icd,
                      thai_icd10 = tdrg_icd10,
                      covidrvs = covid_rvs,
                      neoplasmsdtactual = neoplasms_dt_actual,
                      acrrvs = acr_rvs) {
  # Helper: Trim numeric suffixes (e.g., J1892 -> J189)
  trim_code <- function(code) {
    sub("(\\D+\\d{3})(\\d*)$", "\\1", code)
  }

  # Helper: Check if a code exists in a valid set
  code_exists <- function(code, valid_set) {
    code %in% valid_set
  }

  # Prepare sets of valid codes from datasets
  valid_codes <- unique(trimws(thai_icd10$CODE))
  neoplasm_codes <- unique(neoplasmsdtactual$icd10)
  rvs_codes <- unique(acrrvs$rvs)

  generate_icd10_mapping <- function(filtered_icds) {
    icd_mapping <- list()
    direct_matches <- character() # Store all directly matched codes
    modifiedmatches <- list(modified_matches = character(), modified_match = character()) # Store original-modified pairs

    for (code in filtered_icds) {
      code <- trimws(code)

      # 1. **Exact match check**
      if (code_exists(code, valid_codes)) {
        icd_mapping[[code]] <- list(match_type = "Exact", original = code, mapped = code)
        direct_matches <- c(direct_matches, code)
        next
      }

      # 2. **Attempt adding '9' for 3-character codes**
      if (nchar(code) == 3) {
        modified_code <- paste0(code, "9")
        if (code_exists(modified_code, valid_codes)) {
          icd_mapping[[code]] <- list(match_type = "Modified (Added 9)", original = code, mapped = modified_code)
          modifiedmatches$modified_matches <- c(modifiedmatches$modified_matches, code)
          modifiedmatches$modified_match <- c(modifiedmatches$modified_match, modified_code)
          next
        }
      }

      # 3. **Trim and progressively shorten the code**
      trimmed_code <- trim_code(code)
      match_found <- FALSE

      for (i in 0:(nchar(trimmed_code) - 3)) {
        partial_code <- substr(trimmed_code, 1, nchar(trimmed_code) - i)
        if (nchar(partial_code) >= 3 && code_exists(partial_code, valid_codes)) {
          icd_mapping[[code]] <- list(match_type = "Modified (Trimmed)", original = code, mapped = partial_code)
          modifiedmatches$modified_matches <- c(modifiedmatches$modified_matches, code)
          modifiedmatches$modified_match <- c(modifiedmatches$modified_match, partial_code)
          match_found <- TRUE
          break
        }
      }

      # 4. **Mark as unmatched if no match found**
      if (!match_found) {
        icd_mapping[[code]] <- list(match_type = "Unmatched", original = code, mapped = character(0))
      }
    }

    list(mapping = icd_mapping, modified_matches = modifiedmatches, direct_matches = direct_matches)
  }

  # Collect and pre-filter unique ICD codes, excluding COVID-related ones and applying all filtering criteria
  icds <- unique(c(unlist(c1), unlist(c2), unlist(clin_icd)))
  filtered_icds <- icds[!is.na(icds) &
    !grepl("^[0-9]", icds) &
    !grepl("^[A-Z]{2}", icds) &
    !grepl("/", icds) &
    !(icds %in% neoplasm_codes) &
    !(icds %in% rvs_codes) &
    !(icds %in% covidrvs)]

  # Generate the ICD-10 mapping
  mapping_info <- generate_icd10_mapping(filtered_icds)
  icd_mapping <- mapping_info$mapping

  # Identify unmatched codes
  unmatched_codes <- names(Filter(function(x) x$match_type == "Unmatched", icd_mapping))

  # Create a data.table of unmatched codes by source
  unmatchedsources <- rbindlist(
    lapply(c("c1", "c2", "clin_icd"), function(col_name) {
      col_values <- get(col_name)

      # Flatten the list, filter out NAs or NULLs, and convert to character
      flattened_values <- unlist(col_values)
      valid_codes <- flattened_values[!is.na(flattened_values) & flattened_values != ""]

      if (to_debug) cat(sprintf("\nProcessing column: %s\n", col_name))
      if (to_debug) print(valid_codes)

      if (length(valid_codes) > 0) {
        data.table(code = valid_codes, source = col_name)[, .(count = .N), by = .(code, source)]
      } else {
        data.table(code = character(), source = character(), count = integer())
      }
    }),
    fill = TRUE
  )

  # Filter unmatched sources based on unmatched codes
  unmatchedsources <- unmatchedsources[code %in% unmatched_codes]

  # Create ICD-10 mapping data.table
  icd10_map <- data.table(
    phl_icd10 = names(icd_mapping),
    thai_icd10 = sapply(icd_mapping, `[[`, "mapped"),
    match_type = sapply(icd_mapping, `[[`, "match_type")
  )

  # Apply the mapping to input columns
  apply_icd10_mapping <- function(codes) {
    sapply(codes, function(code) {
      if (!is.null(icd_mapping[[code]]) && !is.null(icd_mapping[[code]]$mapped)) {
        icd_mapping[[code]]$mapped
      } else {
        code
      }
    })
  }

  # Apply mappings
  c1_mapped <- lapply(c1, apply_icd10_mapping)
  c2_mapped <- lapply(c2, apply_icd10_mapping)
  clin_icd_mapped <- lapply(clin_icd, apply_icd10_mapping)
  # str(mapping_info$modified_matches)
  # Return the results
  list(
    c1 = c1_mapped,
    c2 = c2_mapped,
    clin_icd = clin_icd_mapped,
    icd10_map_dt = icd10_map,
    unique_icds = icds,
    unmatched_codes = unmatched_codes,
    unmatched_sources = unmatchedsources,
    icd_mapping_res = icd_mapping,
    modified_matches = mapping_info$modified_matches,
    direct_matches = mapping_info$direct_matches,
    valid_codes = valid_codes,
    rvs_codes = rvs_codes,
    neoplasm_codes = neoplasm_codes,
    covid_rvs = covidrvs,
    thai_icd10 = thai_icd10
  )
}
