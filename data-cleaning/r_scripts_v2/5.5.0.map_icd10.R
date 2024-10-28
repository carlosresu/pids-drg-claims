map_icd10 <- function(c1, c2, clin_icd, thai_icd10 = tdrg_icd10, covidrvs = covid_rvs) {
  # Step 1: Get all unique ICD codes from the provided columns (c1, c2, clin_icd)
  icds <- unique(c(unlist(c1), unlist(c2), unlist(clin_icd)))
  icds <- icds[!is.na(icds)] # Remove NA values

  # Step 2: Create environments for fast lookups
  thai_icd10_env <- list2env(setNames(as.list(rep(TRUE, length(unique(thai_icd10$CODE)))), unique(thai_icd10$CODE)))
  neoplasms_env <- list2env(setNames(
    as.list(rep(TRUE, length(unique(thai_icd10[grepl("/", thai_icd10$CODE), "CODE"])))),
    unique(thai_icd10[grepl("/", thai_icd10$CODE), "CODE"])
  ))

  # Step 3: Identify direct matches in the Thai ICD-10 environment
  direct_matches <- mget(icds, thai_icd10_env, ifnotfound = as.list(rep(FALSE, length(icds))))
  direct_match_codes <- names(unlist(direct_matches[unlist(direct_matches) == TRUE]))

  # Step 4: Generate ICD-10 mappings
  icd_mapping <- list() # Store mappings
  modified_count <- 0 # Counter for modified mappings

  for (d in icds) {
    d <- str_trim(d) # Remove whitespace

    if (d %in% covidrvs) next # Skip COVID-related codes

    if (exists(d, thai_icd10_env)) {
      icd_mapping[[d]] <- d # Direct match
    } else if (!exists(d, neoplasms_env) && grepl("[A-Za-z]", d) && grepl("[0-9]", d)) {
      # Try modifying the code if it isn't a neoplasm
      if (nchar(d) == 3 && exists(paste0(d, "9"), thai_icd10_env)) {
        icd_mapping[[d]] <- paste0(d, "9") # Append "9" for 3-character codes
        modified_count <- modified_count + 1
      } else if (nchar(d) >= 4) {
        # Trim the code from the end to search for a match
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

  unmatched_icds <- setdiff(icds, names(icd_mapping)) # Identify unmatched codes

  # Step 5: Gather source information for unmatched ICDs
  if (length(unmatched_icds) > 0) {
    unmatched_sources <- data.table(code = unmatched_icds, source = NA_character_, count = 0)
    for (col_name in c("c1", "c2", "clin_icd")) {
      col_values <- get(col_name)
      unmatched_sources[code %in% unlist(col_values), source := col_name]
      unmatched_sources[code %in% unlist(col_values), count := count + table(unlist(col_values))[code]]
    }
    unmatched_sources <- unmatched_sources[order(-count)] # Sort by count
  } else {
    unmatched_sources <- data.table() # Empty data.table if no unmatched codes
  }

  # Step 6: Create a data.table for PHL and Thai ICD-10 mappings
  icd10_map <- data.table(phl_icd10 = names(icd_mapping), thai_icd10 = unlist(icd_mapping))

  # Step 7: Create an environment for fast lookup of mapped ICD-10 codes
  icd10_env <- list2env(setNames(as.list(icd10_map$thai_icd10), icd10_map$phl_icd10))

  # Step 8: Apply ICD-10 mapping to the columns c1, c2, and clin_icd
  map_icd10_helper <- function(codes) {
    mapped <- mget(codes, icd10_env, ifnotfound = as.list(codes))
    return(unname(unlist(mapped)))
  }

  c1_mapped <- lapply(c1, map_icd10_helper)
  c2_mapped <- lapply(c2, map_icd10_helper)
  clin_icd_mapped <- lapply(clin_icd, map_icd10_helper)

  # Step 9: Return the mapped columns and relevant diagnostics
  return(
    list(
      c1 = c1_mapped,
      c2 = c2_mapped,
      clin_icd = clin_icd_mapped,
      icd10_map_dt = icd10_map,
      unique_icds = icds,
      direct_matches = direct_match_codes,
      unmatched = unmatched_icds,
      unmatched_sources = unmatched_sources,
      icd_mapping_res = icd_mapping
    )
  )
}
