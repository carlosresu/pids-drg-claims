generate_icd10_mapping <- function(icds, thai_icd10_env, neoplasms_env, covid_rvs) {
  ## Map ICD-10 codes to their closest equivalents in the Thai ICD-10 library

  icd_mapping <- list() # Initialize an empty list to store mappings
  modified_count <- 0 # Initialize counter for modified codes

  # Loop through each ICD-10 code to generate mappings
  for (d in icds) {
    d <- str_trim(d) # Trim whitespace from the code

    # Skip COVID-related codes
    if (d %in% covid_rvs) {
      next # Move to the next code, no further processing for COVID codes
    }

    # If the code has an exact match, map it directly
    if (exists(d, thai_icd10_env)) {
      icd_mapping[[d]] <- d
    } else if (!exists(d, neoplasms_env) && grepl("[A-Za-z]", d) && grepl("[0-9]", d)) {
      # If it's not a neoplasm and contains both letters and numbers, modify it
      if (nchar(d) == 3 && exists(paste0(d, "9"), thai_icd10_env)) {
        # If the code is 3 characters long, try appending "9"
        icd_mapping[[d]] <- paste0(d, "9")
        modified_count <- modified_count + 1
      } else if (nchar(d) >= 4) {
        # Try trimming digits from the end to find a match
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

  # Return the mapping and count of modified codes
  return(list(icd_mapping_res = icd_mapping, modified_count = modified_count))
}