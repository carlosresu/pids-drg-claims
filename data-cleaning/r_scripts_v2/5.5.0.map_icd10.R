############################# OVERALL TODO #############################
# TO-DO's:
# 0. Make a new branch
# 1. Separate all nested functions
#    (including those nested within nested functions)
# 2. Mapping code should process one column at a time, not all columns at once.
# 3. Separate out checks from functions and return statements
############################# OVERALL TODO #############################

############################# SPECIFIC TODO #############################
# 1. document!!!!!!
# 2. rearrange
# 3. test
############################# SPECIFIC TODO #############################

############################# WHAT IS A DOCSTRING? #############################
# Docstring that covers the bigger picture of the function
# Such as why it exists, what it intends to do, and why it was written
############################# WHAT IS A DOCSTRING? #############################

# REFACTORED VERSION WITH DOCSTRINGS/COMMENTAS
code_exists <- function(code, env) {
  # Returns TRUE if code (key: a string) exists (value: a boolean) in an env
  exists(x = code, envir = env, inherits = FALSE)
}

generate_icd10_mapping <- function(filtered_icds) {
  # Initialize things
  icd_mapping <- list() # Mapping to store results

  # Loop through every element of ICDs that are filtered
  for (code in filtered_icds) {
    # 0. **Trim whitespace for ALL codes**
    code <- trimws(code)

    # 1. **Exact match check**
    # If code exists directly/exactly,
    if (code_exists(code, icd_codes_env)) {
      # Add that code to the mapping
      icd_mapping[[code]] <- code

      # Skip further processing for this code
      next
    }

    # 2. **Attempt adding '9' for 3-character codes, then check for existence**
    # If code is 3 characters long, e.g., J18
    if (nchar(code) == 3) {
      # First, add '9' to it
      modified_code <- paste0(code, "9")

      # If the modified code exists in the environment
      if (code_exists(modified_code, icd_codes_env)) {
        # Add the modified code to the generated mapping
        icd_mapping[[code]] <- modified_code

        # Skip further processing for this code
        next
      }
    }

    # 3. **Trimming codes longer than or equal to 4 characters**
    trimmed_code <-
      if (nchar(code) > 4) {
        # For codes longer than 4 characters, trim them
        code <- sub("(\\D+\\d{3})(\\d*)$", "\\1", code)
      } else if (nchar(code) == 4) {
        # For codes exactly 4 characters, keep them as is
        code
      } else {
        # For codes shorter than 4 characters, set to NULL to skip trimming
        NULL
      }

    # For non-null trimmed codes
    if (!is.null(trimmed_code)) {
      # Check if the trimmed 4-character code exists
      if (code_exists(trimmed_code, icd_codes_env)) {
        icd_mapping[[code]] <- trimmed_code

        # Skip further processing for this code
        next
      }

      # If the 4-character code doesn't exist, trim further to 3 characters
      trimmed_to_3 <- substr(trimmed_code, 1, 3)
      if (code_exists(trimmed_to_3, icd_codes_env)) {
        icd_mapping[[code]] <- trimmed_to_3

        # Skip further processing for this code
        next
      }
    }

    # 4. **Mark as unmatched once all else fails**
    icd_mapping[[code]] <- NA_character_
  }

  # Return the final results
  return(icd_mapping)
}


# Updated map_icd10 function using covid_rvs_neoplasm_env
map_icd10 <- function(col) {
  if (!exists("icd_codes_env") || !is.environment(icd_codes_env)) {
    stop("Error: 'icd_codes_env' is not initialized.")
  }

  if (!exists("covid_rvs_neoplasm_env") ||
    !is.environment(covid_rvs_neoplasm_env)) {
    stop("Error: 'covid_rvs_neoplasm_env' is not initialized.")
  }

  # Collect and pre-filter unique ICD codes,
  # excluding those in covid_rvs_neoplasm_env
  icds <- unique(unlist(col))
  filtered_icds <- icds[!is.na(icds) &
    !grepl("^[0-9]", icds) &
    !grepl("^[A-Z]{2}", icds) &
    !grepl("/", icds) &
    !sapply(icds, function(code) code_exists(code, covid_rvs_neoplasm_env))]

  # Generate the ICD-10 mapping
  mapping_info <- generate_icd10_mapping(filtered_icds)

  # Apply mappings
  col_mapped <- lapply(col, function(filtered_icds, mapping_info) {
    unname(sapply(filtered_icds, function(filtered_icd) {
      if (!is.null(mapping_info[[filtered_icd]])) {
        mapping_info[[filtered_icd]]
      } else {
        filtered_icd
      }
    }))
  })

  # Return the results
  return(col_mapped)
}

# # OLD VERSION FOR REFERENCE
# # [[1]] is necessary because mget always returns a list, AND
# # because is.null expects a NULL value directly,
# # not a list containing NULL
# code_exists <- function(code, env) {
#   # Returns TRUE if code exists in/is a valid thai code(s))
#   !is.null(mget(x = code, envir = env, ifnotfound = list(NULL))[[1]])
# }

# # OLD VERSION FOR REFERENCE
# generate_icd10_mapping <- function(filtered_icds) {
#   # Initialize things
#   icd_mapping <- list() # Mapping to store results
#   generated_direct_matches <- character() # Store exact matches
#   generated_modified_matches <- list(
#     # A two-element list of the raw match and its modified counterpart
#     raw_match = character(),
#     modified_match = character()
#   )

#   # Loop through every element of ICDs that are filtered
#   for (code in filtered_icds) {
#     # 0. **Trim whitespace for ALL codes**
#     code <- trimws(code)

#     # 1. **Exact match check**
#     # If code exists directly/exactly,
#     if (code_exists(code, icd_codes_env)) {
#       icd_mapping[[code]] <-
#         # Add that code to generated mapping
#         list(
#           match_type = "Exact",
#           original = code,
#           mapped = code
#         )

#       # Also, add output to checks
#       generated_direct_matches <- c(generated_direct_matches, code)

#       # Skip further processing for this code
#       next
#     }

#     # 2. **Attempt adding '9' for 3-character codes, then check for existence**
#     # If code is 3 characters long, e.g., J18
#     if (nchar(code) == 3) {
#       # First, add '9' to it
#       modified_code <- paste0(code, "9")

#       # If the modified code exists in the environment
#       if (code_exists(modified_code, icd_codes_env)) {
#         # Add the modified code to the generated mapping
#         icd_mapping[[code]] <-
#           list(
#             match_type = "Modified (Added 9)",
#             original = code,
#             mapped = modified_code
#           )

#         # Update generated modified matches
#         generated_modified_matches <- list(
#           raw_match = c(generated_modified_matches$raw_match, code),
#           modified_match = c(generated_modified_matches$modified_match, modified_code)
#         )

#         # Skip further processing for this code
#         next
#       }
#     }

#     # 3. **Trimming codes longer than or equal to 4 characters**
#     trimmed_code <-
#       if (nchar(code) > 4) {
#         # For codes longer than 4 characters, trim them
#         trim_code(code)
#       } else if (nchar(code) == 4) {
#         # For codes exactly 4 characters, keep them as is
#         code
#       } else {
#         # For codes shorter than 4 characters, set to NULL to skip trimming
#         NULL
#       }

#     # For non-null trimmed codes
#     if (!is.null(trimmed_code)) {
#       # Check if the trimmed 4-character code exists
#       if (code_exists(trimmed_code, icd_codes_env)) {
#         icd_mapping[[code]] <- list(
#           match_type = "Modified (Trimmed)",
#           original = code,
#           mapped = trimmed_code
#         )

#         # Update generated modified matches
#         generated_modified_matches <- list(
#           raw_match = c(generated_modified_matches$raw_match, code),
#           modified_match = c(generated_modified_matches$modified_match, trimmed_code)
#         )

#         # Skip further processing for this code
#         next
#       }

#       # If the 4-character code doesn't exist, trim further to 3 characters
#       trimmed_to_3 <- substr(trimmed_code, 1, 3)
#       if (code_exists(trimmed_to_3, icd_codes_env)) {
#         icd_mapping[[code]] <- list(
#           match_type = "Modified (Trimmed)",
#           original = code,
#           mapped = trimmed_to_3
#         )

#         # Update generated modified matches
#         generated_modified_matches <- list(
#           raw_match = c(generated_modified_matches$raw_match, code),
#           modified_match = c(generated_modified_matches$modified_match, trimmed_to_3)
#         )

#         # Skip further processing for this code
#         next
#       }
#     }

#     # 4. **Mark as unmatched once all else fails**
#     icd_mapping[[code]] <- list(
#       match_type = "Unmatched",
#       original = code,
#       mapped = NA_character_
#     )
#   }

#   # Return the final results
#   list(
#     mapping = icd_mapping, # Complete ICD mapping
#     returned_modified_matches = generated_modified_matches, # Modified matches
#     returned_direct_matches = generated_direct_matches # Direct matches
#   )
# }


# # OLD VERSION FOR REFERENCE
# # Apply the mapping to input columns
# apply_icd10_mapping <- function(codes, generated_icd10_mapping) {
#   unname(sapply(codes, function(code) {
#     if (!is.null(icd_mapping[[code]])) {
#       icd_mapping[[code]]
#     } else {
#       code
#     }
#   }))
# }

# # Updated map_icd10 function using covid_rvs_neoplasm_env
# map_icd10 <- function(c1, c2, clin_icd) {
#   # Collect and pre-filter unique ICD codes, excluding those in covid_rvs_neoplasm_env
#   icds <- unique(c(unlist(c1), unlist(c2), unlist(clin_icd)))
#   filtered_icds <- icds[!is.na(icds) &
#     !grepl("^[0-9]", icds) &
#     !grepl("^[A-Z]{2}", icds) &
#     !grepl("/", icds) &
#     !sapply(icds, function(code) code_exists(code, covid_rvs_neoplasm_env))]

#   # Generate the ICD-10 mapping
#   mapping_info <- generate_icd10_mapping(filtered_icds)
#   icd_mapping <- mapping_info$mapping

#   # Identify unmatched codes
#   unmatched_codes <- names(Filter(function(x) x$match_type == "Unmatched", icd_mapping))

#   # Create a data.table of unmatched codes by source
#   unmatchedsources <- rbindlist(
#     lapply(
#       c("c1", "c2", "clin_icd"), # hardcoded names of columns to process
#       function(col_name) {
#         col_values <- get(col_name)

#         # Flatten the list, filter out NAs or NULLs, and convert to character
#         flattened_values <- unlist(col_values)
#         valid_codes <- flattened_values[
#           !is.na(
#             flattened_values
#           ) & flattened_values != ""
#         ]

#         if (length(valid_codes) > 0) {
#           data.table(
#             code = valid_codes,
#             source = col_name
#           )[,
#             .(count = .N),
#             by = .(
#               code,
#               source
#             )
#           ]
#         } else {
#           data.table(
#             code = character(),
#             source = character(),
#             count = integer()
#           )
#         }
#       }
#     ),
#     fill = TRUE
#   )

#   # Filter unmatched sources based on unmatched codes
#   # %chin% is a faster version of %in% for character vectors
#   unmatchedsources <- unmatchedsources[code %chin% unmatched_codes]

#   # Create ICD-10 mapping data.table
#   icd10_map <- data.table(
#     phl_icd10 = names(icd_mapping),
#     # STUDY: what does `[[` do
#     thai_icd10 = sapply(icd_mapping, `[[`, "mapped"),
#     match_type = sapply(icd_mapping, `[[`, "match_type")
#   )

#   # Apply mappings
#   c1_mapped <- lapply(
#     c1,
#     apply_icd10_mapping
#   )
#   c2_mapped <- lapply(
#     c2,
#     apply_icd10_mapping
#   )
#   clin_icd_mapped <- lapply(
#     clin_icd,
#     apply_icd10_mapping
#   )

#   # Return the results
#   return(
#     list(
#       c1 = c1_mapped,
#       c2 = c2_mapped,
#       clin_icd = clin_icd_mapped,
#       icd10_map_dt_for_checks = icd10_map,
#       unique_icds_for_checks = icds,
#       unmatched_codes_for_checks = unmatched_codes,
#       unmatched_sources_for_checks = unmatchedsources,
#       icd_mapping_for_checks = icd_mapping,
#       modified_matches_for_checks = mapping_info$returned_modified_matches,
#       direct_matches_for_checks = mapping_info$returned_direct_matches
#     )
#   )
# }
