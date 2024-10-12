### Helper functions for remapping/reformatting key columns in the claims
remap_columns <- function(dt, column_name, to_view_checks = TRUE, known_values, remap_logic = remapped_column) {
  ## Function to remap different categorical columns in the claims dataset
  # dt: data.table
  # column_name: name of the column to remap
  # to_view_checks: boolean flag to enable check and capture of unmapped values
  # known_values: a list of known values for the specific column
  # remap_logic: quoted fcase logic passed as an argument

  # First, initialize the column with the original values to ensure no row count mismatch
  original_values <- dt[[column_name]]

  # Use `set` to modify the data.table by reference to avoid copying
  set(dt, j = column_name, value = eval(remap_logic))

  # Check for unmapped entries
  unknown_values <- setdiff(original_values[!is.na(original_values)], known_values[[column_name]])

  if (length(unknown_values) > 0 && to_view_checks) {
    warning(sprintf(
      "Unmapped values in column '%s': %s",
      column_name, paste(unknown_values, collapse = ", ")
    ))
  }

  return(
    list(
      data = dt,
      remapped = dt[[column_name]],
      unmapped = unknown_values
    )
  )
}

remap_patient_data <- function(dt, to_view_checks = TRUE) {
  ## Remap the categorical columns in the inpatient data

  # Initialize unmapped variables and mapped data tables
  pat_unmap <- parent_unmap <- child_unmap <- discharge_unmap <- claim_status_unmap <- NULL
  pat_mapped <- parent_mapped <- child_mapped <- discharge_mapped <- claim_status_mapped <- NULL

  # Define the columns that need remapping
  columns_to_remap <- list(
    pat_type = "pat_type",
    pat_memcat_parent = "pat_memcat_parent",
    pat_memcat_child = "pat_memcat_child",
    clin_discharge = "clin_discharge",
    claim_status = "claim_status"
  )

  # Apply remapping for each column
  for (col_name in names(columns_to_remap)) {
    result <- remap_columns(dt, columns_to_remap[[col_name]], to_view_checks, known_values, remapped_column)

    # Update the original column using `set`
    set(dt, j = columns_to_remap[[col_name]], value = result$remapped)

    # Capture mapped and unmapped values
    if (col_name == "pat_type") {
      pat_mapped <- unique(data.table(Original = result$remapped, Mapped = result$remapped))
      if (length(result$unmapped) > 0 && to_view_checks) pat_unmap <- result$unmapped
    } else if (col_name == "pat_memcat_parent") {
      parent_mapped <- unique(data.table(Original = result$remapped, Mapped = result$remapped))
      if (length(result$unmapped) > 0 && to_view_checks) parent_unmap <- result$unmapped
    } else if (col_name == "pat_memcat_child") {
      child_mapped <- unique(data.table(Original = result$remapped, Mapped = result$remapped))
      if (length(result$unmapped) > 0 && to_view_checks) child_unmap <- result$unmapped
    } else if (col_name == "clin_discharge") {
      discharge_mapped <- unique(data.table(Original = result$remapped, Mapped = result$remapped))
      if (length(result$unmapped) > 0 && to_view_checks) discharge_unmap <- result$unmapped
    } else if (col_name == "claim_status") {
      claim_status_mapped <- unique(data.table(Original = result$remapped, Mapped = result$remapped))
      if (length(result$unmapped) > 0 && to_view_checks) claim_status_unmap <- result$unmapped
    }
  }

  # Return the remapped data and all mapping/unmapped data
  return(
    list(
      data = dt,
      pat_type_mapped = pat_mapped,
      pat_memcat_parent_mapped = parent_mapped,
      pat_memcat_child_mapped = child_mapped,
      clin_discharge_mapped = discharge_mapped,
      claim_status_mapped = claim_status_mapped,
      pat_type_unmapped = pat_unmap,
      memcat_parent_unmapped = parent_unmap,
      memcat_child_unmapped = child_unmap,
      discharge_unmapped = discharge_unmap,
      claim_status_unmapped = claim_status_unmap
    )
  )
}

remove_lumped_icd_codes <- function(column) {
  ## Takes a column and separates out ICD-10 codes using "||"
  ## been lumped into a single string

  # Use regex to add "||" between letters and digits in the ICD codes (e.g., A123B456 -> A123||B456)
  modified_column <- stri_replace_all_regex(
    column, "(?<=\\d)(?=[A-Za-z])", "||",
    opts_regex = stri_opts_regex() # Specify regex options for the replacement
  )

  # Return the modified column with ICD codes split
  return(modified_column)
}

remove_lumped_rvs_codes <- function(column) {
  ## Separates out lumped RVS codes by splitting into chunks of 5 chars each

  # Define a helper function to split each code into 5-character chunks
  split_rvs_codes_helper <- function(code) {
    if (is.na(code) || code == "" || is.null(code)) {
      return(NA_character_) # If the input code is NA, empty, or NULL, return NA
    }

    # Remove all non-alphanumeric characters and clean the code
    code_clean <- gsub("\\|", "", code) # Remove all "|" characters
    code_clean <- gsub("[^A-Z0-9]", "", code_clean) # Remove non-alphanumeric characters

    # If the cleaned code length is 0, return NA
    if (nchar(code_clean) == 0) {
      return(NA_character_)
    } else if (nchar(code_clean) %% 5 != 0) {
      # If the length is not a multiple of 5, log a message and return NA
      message(paste0("Total length of concatenated RVS codes is not a multiple of 5 characters: ", code_clean))
      return(NA_character_)
    } else {
      # Insert "||" every 5 characters to split the code
      modified_code <- gsub("(.{5})", "\\1||", code_clean)

      # Remove trailing "||" if present
      modified_code <- gsub("\\|\\|$", "", modified_code)

      return(modified_code)
    }
  }

  # Apply the helper function to each element of the input column
  modified_column <- sapply(as.character(column), split_rvs_codes_helper, USE.NAMES = FALSE)

  return(modified_column) # Return the modified column with split RVS codes
}

remove_lumped_icd9_codes <- function(column) {
  ## Separates out lumped ICD9 codes by splitting into chunks of 4 chars each

  # Define a helper function to process each code
  split_rvs_codes_helper_icd9 <- function(code) {
    if (is.na(code) || code == "" || is.null(code)) {
      return(NA_character_) # Return NA if input is NA, empty, or NULL
    }

    # Remove all '|' characters and ensure only alphanumeric characters are kept
    code_clean <- gsub("\\|", "", code) # Remove all "|" characters
    code_clean <- gsub("[^A-Z0-9]", "", code_clean) # Remove non-alphanumeric characters

    # If the cleaned code length is 0, return NA
    if (nchar(code_clean) == 0) {
      return(NA_character_)
    } else if (nchar(code_clean) %% 4 != 0) {
      # If the length is not a multiple of 4, log a message and return NA
      message(paste0("Total length of concatenated ICD9 codes is not a multiple of 4 characters: ", code_clean))
      return(NA_character_)
    } else {
      # Insert "||" every 4 characters to split the code
      modified_code <- gsub("(.{4})", "\\1||", code_clean)

      # Remove trailing "||" if present
      modified_code <- gsub("\\|\\|$", "", modified_code)

      return(modified_code)
    }
  }

  # Apply the helper function to each element in the input column
  modified_column <- sapply(as.character(column), split_rvs_codes_helper_icd9, USE.NAMES = FALSE)

  return(modified_column) # Return the modified column with split ICD9 codes
}


transfer_extra_icd10s_to_clin_icd <- function(clin_icd, col) {
  ## Transfer extra ICD-10 codes from a column to the compilation of ICD-10 codes for the case
  # clin_icd: column with all the ICD-10 codes
  # col: column with the possible extra ICD-10 codes

  # If clin_icd is null, initialize it as an empty vector
  clin_icd <- lapply(clin_icd, function(x) if (is.null(x)) character() else x)

  # Extract the first element of col (typically case rate c1 or c2) to keep separately
  col_first <- lapply(col, function(x) x[1])

  # Append the remaining elements of col to clin_icd for each row, only if not already in clin_icd
  clin_icd <- mapply(function(icd, c1) {
    extra_icds <- c1[-1] # Take all elements from c1 except the first
    # Only append elements that are not already in clin_icd
    new_icds <- extra_icds[!extra_icds %in% icd]
    c(icd, new_icds) # Concatenate clin_icd with the filtered new elements
  }, clin_icd, col, SIMPLIFY = FALSE)

  # Return the updated clin_icd and the first element of col
  return(list(clin_icd = clin_icd, col_first = col_first))
}


get_unique_icd_codes <- function(c1, c2, clin_icd) {
  ## Obtains list of all unique ICD-10 codes across all cases and columns

  # Concatenate all elements from c1, c2, and clin_icd and remove duplicates using unique
  icds <- unique(c(unlist(c1), unlist(c2), unlist(clin_icd)))

  # Remove any NA values from the list of ICD codes
  icds <- icds[!is.na(icds)]

  # Return the unique list of ICD codes
  return(icds)
}


create_thai_icd10_environment <- function(thai_icd10_codes) {
  ## Create environment for Thai ICD-10 codes

  # Create a new environment where the Thai ICD-10 codes are set to TRUE
  thai_icd10_env <- list2env(
    setNames(as.list(rep(TRUE, length(thai_icd10_codes))), thai_icd10_codes)
  )

  # Return the created environment
  return(thai_icd10_env)
}


find_direct_icd_matches <- function(icds, thai_icd10_env) {
  ## Identify ICD-10 codes with exact matches in the Thai ICD-10 library
  # icds: list of ICD-10 codes to be cross-checked
  # thai_icd10_env: environment of Thai ICD-10 codes

  # Retrieve the values of each ICD code from the thai_icd10_env environment.
  # If a code is not found, it returns FALSE (using ifnotfound argument).
  direct_matches <- mget(icds, thai_icd10_env, ifnotfound = as.list(rep(FALSE, length(icds))))

  # Extract the names of ICD codes that matched (i.e., returned TRUE from the environment)
  direct_match_codes <- names(unlist(direct_matches[unlist(direct_matches) == TRUE]))

  # Return the matched ICD codes
  return(direct_match_codes)
}

generate_icd10_mapping <- function(icds, thai_icd10_env, neoplasms_env) {
  ## Map ICD-10 codes to their closest equivalents in the Thai ICD-10 library

  icd_mapping <- list() # Initialize an empty list to store mappings
  modified_count <- 0 # Initialize counter for modified codes

  # Loop through each ICD-10 code to generate mappings
  for (d in icds) {
    d <- str_trim(d) # Trim whitespace from the code

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
  return(list(icd_mapping = icd_mapping, modified_count = modified_count))
}


apply_icd10_mapping_to_columns <- function(c1, c2, clin_icd, icd10_env) {
  ## Maps ICD-10 codes to the given columns using the provided environment

  # Helper function to map ICD-10 codes using the provided environment
  map_icd10_helper <- function(codes) {
    # Use mget to map each code to its equivalent in icd10_env or return the original if no match is found
    mapped <- mget(codes, icd10_env, ifnotfound = as.list(codes))
    return(unname(unlist(mapped))) # Return the mapped codes as an unnamed vector
  }

  # Apply the mapping function to each of the columns (c1, c2, and clin_icd)
  # Cel: change this to a for-loop
  # Carlos: lapply is faster because lapply is optimized for iteration in R’s internal C/C++ code,
  # Carlos: whereas for loops have more overhead due to their explicit nature in R.
  c1_mapped <- lapply(c1, map_icd10_helper)
  c2_mapped <- lapply(c2, map_icd10_helper)
  clin_icd_mapped <- lapply(clin_icd, map_icd10_helper)

  # Return the mapped values for c1, c2, and clin_icd
  return(list(c1 = c1_mapped, c2 = c2_mapped, clin_icd = clin_icd_mapped))
}

add_c1_c2_to_clin_icd <- function(c1, c2, clin_icd) {
  ## Adds c1 and c2 ICD codes to clin_icd, allowing duplicates

  # Create a data.table to handle the merging of codes efficiently
  datatable <- data.table(c1 = c1, c2 = c2, clin_icd = clin_icd)

  # Map function to concatenate clin_icd with c1 and c2, allowing duplicates
  # TODO: Don't duplicate it if it's already there
  datatable[, clin_icd := Map(function(c1, c2, icd) {
    c(icd, c1, c2) # Concatenate clin_icd with c1 and c2
  }, c1, c2, clin_icd)]

  # Return the updated clin_icd column
  return(list(clin_icd = datatable$clin_icd))
}

split_rvs_codes <- function(rvs_icd9) {
  ## Split the RVS codes into those with DRG and those without

  # Filter rows where the RVS code has an associated DRG
  with_drg <- rvs_icd9[is_drg == TRUE]

  # Filter rows where the RVS code does not have a DRG
  without_drg <- rvs_icd9[!rvs %in% with_drg$rvs]

  # Return the lists of RVS codes with and without DRG
  return(list(with_drg = with_drg, without_drg = without_drg))
}


create_rvs_map_lists <- function(with_drg) {
  ## Create two lists for mapping RVS codes to ICD-9-CM codes

  # Order the table by RVS code and whether it's associated with a DRG
  setorder(with_drg, rvs, -is_drg)

  # Group by RVS code and create a list of associated ICD-9-CM codes for each RVS
  unique_rvs <- with_drg[, .(icd9cm_list = list(icd9cm)), by = rvs]

  # Separate RVS codes that map to a single ICD-9-CM code from those with multiple mappings
  solo <- unique_rvs[lengths(icd9cm_list) == 1]
  list_mapped <- unique_rvs[lengths(icd9cm_list) > 1]

  # Create named lists for solo and multi-mapped RVS codes
  rvs_map_solo <- setNames(solo$icd9cm_list, solo$rvs)
  rvs_map_list <- setNames(list_mapped$icd9cm_list, list_mapped$rvs)

  # Return the solo and multi-mapped lists
  return(list(rvs_map_list = rvs_map_list, rvs_map_solo = rvs_map_solo))
}


get_icd9_codes <- function(clin_rvs, rvs_map_solo_env) {
  ## Maps a column containing RVS codes to ICD-9-CM

  # Use lapply to loop through each row of clin_rvs
  lapply(clin_rvs, function(x) {
    codes <- unlist(x) # Unlist the RVS codes in each row

    # Map all codes to ICD-9-CM equivalents using the rvs_map_solo_env environment
    mappable <- codes[!is.na(mget(codes, envir = rvs_map_solo_env, ifnotfound = NA_character_))]

    if (length(mappable) > 0) {
      # Return the unique set of mapped ICD-9 codes
      unique(unlist(mget(mappable, envir = rvs_map_solo_env)))
    } else {
      NA_character_ # Return NA if no mappable codes are found
    }
  })
}

find_and_append_valid_rvs <- function(datatable, valid_rvs_codes) {
  datatable[, matches := lapply(col, function(x) {
    # Find valid RVS codes within each vector of 'col'
    valid_codes <- x[x %in% valid_rvs_codes]
    return(unique(valid_codes))
  })]

  # Append valid matches to the existing 'clin_rvs' vector
  datatable[, clin_rvs := mapply(function(rvs, matches) unique(c(rvs, matches)), clin_rvs, matches, SIMPLIFY = FALSE)]
}

remove_5_digit_codes <- function(col) {
  # Handle the column as a list of vectors
  return(lapply(col, function(x) {
    if (is.na(x)) {
      return(NA_character_)
    } else {
      return(x)
    }
    # stri_replace_all_regex(x, "\\b\\d{5}\\b", "") # Don't delete rvs codes from c1 and c2
  }))
}

warn_invalid_rvs <- function(matches, valid_rvs_codes) {
  ## Triggers warnings for invalid RVS codes

  # Create a new environment for valid RVS codes
  valid_rvs_env <- new.env(hash = TRUE, parent = emptyenv())

  # Populate the environment with valid RVS codes
  for (code in valid_rvs_codes) {
    assign(code, TRUE, envir = valid_rvs_env)
  }

  # Identify invalid RVS codes by checking if they exist in the valid_rvs_env environment
  invalid_matches <- lapply(matches, function(x) x[!vapply(x, exists, logical(1), envir = valid_rvs_env)])

  # Flatten the list of invalid matches into a single vector
  discarded_codes <- unlist(invalid_matches)

  # If invalid codes exist, create a summary table of their counts
  if (length(discarded_codes) > 0) {
    discarded_table <- data.table(CODE = discarded_codes)[, .N, by = CODE][order(-N)]
    setnames(discarded_table, c("CODE", "count"))
  } else {
    discarded_table <- data.table()
  }

  # Return the table of invalid codes and their counts
  return(discarded_table)
}


append_and_remove_rvs <- function(clin_rvs, col, rvs_icd9) {
  ## Ensure both clin_rvs and col are lists of vectors
  datatable <- data.table(clin_rvs = clin_rvs, col = col)

  valid_rvs_codes <- rvs_icd9$rvs

  # Append valid RVS codes to clin_rvs (handling each element of the vectors)
  find_and_append_valid_rvs(datatable, valid_rvs_codes)

  # Modify the column by removing 5-digit codes from each vector and recursively unlisting
  datatable[, col := lapply(col, function(x) {
    # Optimize by checking if x is already a character vector
    cleaned_col <- if (is.character(x)) {
      remove_5_digit_codes(x) # Directly modify the character vector
    } else {
      # Recursively unlist and clean the elements
      remove_5_digit_codes(as.character(x))
    }
    return(unlist(cleaned_col))
  })]

  # Trigger warnings for invalid RVS codes
  discarded_rvs <- warn_invalid_rvs(datatable$matches, valid_rvs_codes)

  # Return updated clin_rvs, cleaned col, and invalid codes
  return(list(clin_rvs = datatable$clin_rvs, col = datatable$col, discarded_rvs = discarded_rvs))
}

apply_find_pdx <- function(c1, c2, clin_icd, acc_pdx) {
  ## Function to apply the PDX finding logic in a vectorized manner

  # Step 1: Create a new environment for accepted PDX codes
  acc_pdx_env <- new.env(hash = TRUE, parent = emptyenv())

  # Step 2: Populate the environment with accepted PDX codes
  for (code in acc_pdx) {
    assign(code, TRUE, envir = acc_pdx_env)
  }

  # Step 3: Define helper function to find PDX for each row
  find_pdx_for_row <- function(c1, c2, clin_icd) {
    # Function to check similarity between two strings
    check_similarity <- function(x, y) {
      score <- 0
      min_len <- min(nchar(x), nchar(y))
      for (i in 1:min_len) {
        if (substr(x, i, i) == substr(y, i, i)) {
          score <- score + 1
        }
      }
      return(score)
    }

    # Split c1 and c2 by '|' if necessary
    c1 <- unlist(strsplit(c1, "\\|"))
    c2 <- unlist(strsplit(c2, "\\|"))

    # Step 1: Check if any element in c1 or c2 is an acceptable PDx
    for (cr_list in list(c1, c2)) {
      for (cr in cr_list) {
        if (!is.na(cr) && exists(cr, envir = acc_pdx_env)) {
          return(list(pdx = cr, pdx_code = ifelse(cr %in% c1, 1, 2)))
        }
      }
    }

    # Step 2: Unlist clin_icd by splitting if necessary
    clin_icd <- unlist(strsplit(clin_icd, "\\|"))

    # Step 3: Get a list of acceptable PDx from clin_icd
    pdxs <- unique(clin_icd)
    pdxs <- pdxs[sapply(pdxs, function(x) exists(x, envir = acc_pdx_env))]

    # Step 4: Handle cases with no or only one acceptable PDx
    if (length(pdxs) == 0) {
      return(list(pdx = NA_character_, pdx_code = 99))
    } else if (length(pdxs) == 1) {
      return(list(pdx = pdxs[1], pdx_code = 3))
    }

    # Step 5: Check c1 and c2 for matching starting letters
    for (cr_list in list(c1, c2)) {
      for (cr in cr_list) {
        if (!is.na(cr)) {
          starting_letter <- substr(cr, 1, 1)
          starting_codes <- pdxs[substr(pdxs, 1, 1) == starting_letter]

          if (length(starting_codes) == 1) {
            return(list(pdx = starting_codes[1], pdx_code = 4))
          } else if (length(starting_codes) > 1) {
            starting_codes <- starting_codes[
              order(sapply(starting_codes, function(x) check_similarity(cr, x)), decreasing = TRUE)
            ]
            return(list(pdx = starting_codes[1], pdx_code = 5))
          }
        }
      }
    }

    # Step 6: If no matching starting letter, pick a random PDx
    if (length(pdxs) > 0) {
      return(list(pdx = sample(pdxs, 1), pdx_code = 6))
    }

    # Step 7: Return NA and code 99 if no PDx is found
    return(list(pdx = NA_character_, pdx_code = 99))
  }

  # Step 4: Apply find_pdx_for_row function to all rows
  result <- mapply(find_pdx_for_row, c1, c2, clin_icd, SIMPLIFY = FALSE)

  # Step 5: Extract PDX and PDX codes into vectors
  pdx <- sapply(result, function(x) x$pdx)
  pdx_code <- sapply(result, function(x) x$pdx_code)

  # Return the PDX values and codes
  return(list(pdx = pdx, pdx_code = pdx_code))
}

generate_dob <- function(bdays, ages, date_adms) {
  ## Function to generate dates of birth (DOB) based on birthdates, ages, and admission dates

  set.seed(global_seed) # Ensure reproducibility by setting a global seed

  require(lubridate) # Load lubridate for date manipulation

  # Convert ages to numeric
  ages <- as.numeric(ages)

  # Initialize DOB vector with NA values
  dob <- rep(NA_character_, length(ages))

  ## Step 1: Use provided birthdates where available
  valid_bdays_indices <- !is.na(bdays) & bdays != "" # Find valid birthdate indices

  # Convert valid birthdates to desired format
  dob[valid_bdays_indices] <- format(ymd(bdays[valid_bdays_indices]), "%d/%m/%Y")

  ## Step 2: Handle missing birthdates
  missing_bday_indices <- which(is.na(bdays) | bdays == "") # Find indices with missing birthdates
  ref_dates <- ymd(date_adms[missing_bday_indices]) # Get reference dates (admission dates)

  ## Step 3: Handle age == 0
  zero_age_indices <- which(!is.na(ages[missing_bday_indices]) & ages[missing_bday_indices] == 0)

  # Generate random days for age 0 cases
  if (length(zero_age_indices) > 0) {
    dob[missing_bday_indices[zero_age_indices]] <- format(
      ref_dates[zero_age_indices] - days(sample(1:27, length(zero_age_indices), replace = TRUE)), "%d/%m/%Y"
    )
  }
  # TODO for where birthdate exists impute it as the difference between date admission and birthdate
  # Otherwise just "3"

  ## Step 4: Handle positive ages
  positive_age_indices <- which(!is.na(ages[missing_bday_indices]) & ages[missing_bday_indices] > 0)

  # Subtract exact age in years from the reference date
  if (length(positive_age_indices) > 0) {
    truncated_ages <- floor(ages[missing_bday_indices][positive_age_indices])
    dob[missing_bday_indices[positive_age_indices]] <- format(
      ref_dates[positive_age_indices] - years(truncated_ages), "%d/%m/%Y"
    )
  }

  # Return the vector of generated DOBs
  return(dob)
}
