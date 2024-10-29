map_icd10 <- function(c1, c2, clin_icd, thai_icd10 = tdrg_icd10, covidrvs = covid_rvs) {
  # Helper: Create named lists for environments
  create_named_list <- function(codes) {
    if (length(codes) > 0) {
      setNames(as.list(rep(TRUE, length(codes))), codes)
    } else {
      list()
    }
  }

  # Step 1: Create environments for Thai codes and neoplasm codes
  thai_env <- new.env(hash = TRUE)
  neoplasm_env <- new.env(hash = TRUE)

  # Step 2: Populate environments with named lists
  list2env(create_named_list(thai_icd10$CODE), envir = thai_env)
  neoplasm_codes <- thai_icd10[grepl("/", thai_icd10$CODE), "CODE"]
  list2env(create_named_list(neoplasm_codes), envir = neoplasm_env)

  # Step 3: Helper to get unique ICD codes
  get_unique_icd_codes <- function(...) {
    unique(unlist(list(...), use.names = FALSE))
  }

  # Step 4: Generate ICD10 mapping using environment lookups with batch processing
  generate_icd10_mapping <- function(icds, covidrvs) {
    icd_mapping <- new.env(hash = TRUE)
    modified_count <- 0

    # Filter out invalid and COVID-related codes
    valid_icds <- setdiff(icds[!is.na(icds) & icds != ""], covidrvs)

    # Batch lookup using mget for faster performance
    thai_matches <- mget(valid_icds, envir = thai_env, ifnotfound = list(NULL))
    neoplasm_matches <- mget(valid_icds, envir = neoplasm_env, ifnotfound = list(NULL))

    for (d in valid_icds) {
      d <- str_trim(d)
      if (!is.null(thai_matches[[d]])) {
        icd_mapping[[d]] <- d
      } else if (is.null(neoplasm_matches[[d]]) && grepl("[A-Za-z]", d) && grepl("[0-9]", d)) {
        if (nchar(d) == 3 && !is.null(thai_matches[[paste0(d, "9")]])) {
          icd_mapping[[d]] <- paste0(d, "9")
          modified_count <- modified_count + 1
        } else if (nchar(d) >= 4) {
          # Substring search for longer codes
          for (i in seq_len(nchar(d) - 3)) {
            new_d <- substr(d, 1, nchar(d) - i)
            if (!is.null(thai_matches[[new_d]])) {
              icd_mapping[[d]] <- new_d
              modified_count <- modified_count + 1
              break
            }
          }
        }
      }
    }
    list(icd_mapping_res = as.list(icd_mapping), modified_count = modified_count)
  }

  # Step 5: Apply mappings using vectorization
  apply_icd10_mapping <- function(codes, icd_mapping) {
    vapply(codes, function(code) icd_mapping[[code]] %||% code, character(1))
  }

  # Step 6: Get unique ICD codes
  icds <- get_unique_icd_codes(c1, c2, clin_icd)

  # Step 7: Generate ICD10 mapping
  icd_mapping_info <- generate_icd10_mapping(icds, covidrvs)
  icd_mapping <- icd_mapping_info$icd_mapping_res

  # Step 8: Identify unmatched ICD codes
  unmatched_icds <- setdiff(icds, names(icd_mapping))

  # Step 9: Create data.table for unmatched codes
  unmatched_sources <- rbindlist(lapply(c("c1", "c2", "clin_icd"), function(col_name) {
    col_values <- get(col_name)
    data.table(code = unlist(col_values), source = col_name)[, .(count = .N), by = .(code, source)]
  }))
  unmatched_sources <- unmatched_sources[code %in% unmatched_icds, ][order(-count)]

  str(icd_mapping)

  # Step 10: Create data.table for ICD10 mapping
  icd10_map <- data.table(phl_icd10 = names(icd_mapping), thai_icd10 = unlist(icd_mapping))

  # Step 11: Apply ICD10 mapping to input columns
  c1_mapped <- lapply(c1, apply_icd10_mapping, icd_mapping)
  c2_mapped <- lapply(c2, apply_icd10_mapping, icd_mapping)
  clin_icd_mapped <- lapply(clin_icd, apply_icd10_mapping, icd_mapping)

  # Step 12: Return results
  list(
    c1 = c1_mapped,
    c2 = c2_mapped,
    clin_icd = clin_icd_mapped,
    icd10_map_dt = icd10_map,
    unique_icds = icds,
    direct_matches = names(icd_mapping),
    unmatched = unmatched_icds,
    unmatched_sources = unmatched_sources,
    icd_mapping_res = icd_mapping
  )
}

# get_unique_icd_codes <- function(c1, c2, clin_icd) {
#   ## Obtains list of all unique ICD-10 codes across all cases and columns

#   # Concatenate all elements from c1, c2, and clin_icd and remove duplicates using unique
#   icds <- unique(c(unlist(c1), unlist(c2), unlist(clin_icd)))
#   # icds <- c(unlist(c1), unlist(c2), unlist(clin_icd))

#   # Remove any NA values from the list of ICD codes
#   icds <- icds[!is.na(icds)]

#   # Return the unique list of ICD codes
#   return(icds)
# }
# find_direct_icd_matches <- function(icds, thai_icd10_env) {
#   ## Identify ICD-10 codes with exact matches in the Thai ICD-10 library
#   # icds: list of ICD-10 codes to be cross-checked
#   # thai_icd10_env: environment of Thai ICD-10 codes

#   # Retrieve the values of each ICD code from the thai_icd10_env environment.
#   # If a code is not found, it returns FALSE (using ifnotfound argument).
#   direct_matches <- mget(icds, thai_icd10_env, ifnotfound = as.list(rep(FALSE, length(icds))))

#   # Extract the names of ICD codes that matched (i.e., returned TRUE from the environment)
#   direct_match_codes <- names(unlist(direct_matches[unlist(direct_matches) == TRUE]))

#   # Return the matched ICD codes
#   return(direct_match_codes)
# }
# generate_icd10_mapping <- function(icds, thai_icd10_env, neoplasms_env, covid_rvs) {
#   ## Map ICD-10 codes to their closest equivalents in the Thai ICD-10 library

#   icd_mapping <- list() # Initialize an empty list to store mappings
#   modified_count <- 0 # Initialize counter for modified codes

#   # Loop through each ICD-10 code to generate mappings
#   for (d in icds) {
#     d <- str_trim(d) # Trim whitespace from the code

#     # Skip COVID-related codes
#     if (d %in% covid_rvs) {
#       next # Move to the next code, no further processing for COVID codes
#     }

#     # If the code has an exact match, map it directly
#     if (exists(d, thai_icd10_env)) {
#       icd_mapping[[d]] <- d
#     } else if (!exists(d, neoplasms_env) && grepl("[A-Za-z]", d) && grepl("[0-9]", d)) {
#       # If it's not a neoplasm and contains both letters and numbers, modify it
#       if (nchar(d) == 3 && exists(paste0(d, "9"), thai_icd10_env)) {
#         # If the code is 3 characters long, try appending "9"
#         icd_mapping[[d]] <- paste0(d, "9")
#         modified_count <- modified_count + 1
#       } else if (nchar(d) >= 4) {
#         # Try trimming digits from the end to find a match
#         for (i in seq_len(nchar(d) - 3)) {
#           new_d <- substr(d, 1, nchar(d) - i)
#           if (exists(new_d, thai_icd10_env)) {
#             icd_mapping[[d]] <- new_d
#             modified_count <- modified_count + 1
#             break
#           }
#         }
#       }
#     }
#   }

#   # Return the mapping and count of modified codes
#   return(list(icd_mapping_res = icd_mapping, modified_count = modified_count))
# }
# apply_icd10_mapping_to_columns <- function(c1, c2, clin_icd, icd10_env) {
#   ## Maps ICD-10 codes to the given columns using the provided environment

#   # Helper function to map ICD-10 codes using the provided environment
#   map_icd10_helper <- function(codes) {
#     codes <- codes[!is.na(codes) & codes != ""] # Filter out invalid values

#     # Use make.names to ensure codes are valid in environment lookups
#     sanitized_codes <- make.names(codes, unique = TRUE)

#     # Use mget to map each code to its equivalent in icd10_env or return the original if no match is found
#     mapped <- mget(sanitized_codes, icd10_env, ifnotfound = as.list(codes))
#     return(unname(unlist(mapped))) # Return the mapped codes as an unnamed vector
#   }

#   # Apply the mapping function to each of the columns (c1, c2, and clin_icd)
#   c1_mapped <- lapply(c1, map_icd10_helper)
#   c2_mapped <- lapply(c2, map_icd10_helper)
#   clin_icd_mapped <- lapply(clin_icd, map_icd10_helper)

#   # Return the mapped values for c1, c2, and clin_icd
#   return(list(c1 = c1_mapped, c2 = c2_mapped, clin_icd = clin_icd_mapped))
# }
# create_thai_icd10_environment <- function(thai_icd10_codes) {
#   ## Create environment for Thai ICD-10 codes

#   # Use make.names to sanitize codes for environment keys
#   sanitized_codes <- make.names(thai_icd10_codes, unique = TRUE)

#   # Create a new environment where the Thai ICD-10 codes are set to TRUE
#   thai_icd10_env <- list2env(
#     setNames(as.list(rep(TRUE, length(sanitized_codes))), sanitized_codes)
#   )

#   # Return the created environment
#   return(thai_icd10_env)
# }
# map_icd10 <- function(c1, c2, clin_icd, thai_icd10 = tdrg_icd10) {
#   # Step 1: Get all unique ICD codes from the provided
#   # columns (c1, c2, and clin_icd)
#   icds <- get_unique_icd_codes(c1, c2, clin_icd)
#   # str(icds)
#   icds <- icds[!is.na(icds) & icds != ""]
#   # str(icds)

#   # Step 2: Create an environment for Thai ICD10 codes for faster lookup
#   # This uses the unique set of ICD10 codes in the thai_icd10 table.
#   thai_icd10_env <- create_thai_icd10_environment(
#     unique(thai_icd10$CODE)
#   )

#   # Step 3: Create a second environment for Thai ICD10
#   # neoplasm codes (those with slashes '/')
#   neoplasms_env <- create_thai_icd10_environment(
#     unique(thai_icd10[grepl("/", thai_icd10$CODE), "CODE"])
#   )

#   # Step 4: Find direct matches between the provided ICD
#   # codes (icds) and the Thai ICD10 environment
#   direct_match_codes <- find_direct_icd_matches(
#     icds, thai_icd10_env
#   )

#   # Step 5: Generate the full ICD10 mapping for the ICD codes,
#   # considering both Thai ICD10 environment and neoplasms environment.
#   icd_mapping_info <- generate_icd10_mapping(
#     icds, thai_icd10_env, neoplasms_env, covid_rvs
#   )
#   # Extract the mapping and the count of modified mappings
#   icd_mapping <- icd_mapping_info$icd_mapping_res

#   modified_count <- icd_mapping_info$modified_count

#   # Step 6: Identify ICD codes that were not successfully mapped.
#   unmatched_icds <- setdiff(icds, names(icd_mapping))

#   # Step 7: If there are unmatched ICD codes, gather
#   # their source information (c1, c2, clin_icd)
#   # and the count of occurrences in each column.
#   if (length(unmatched_icds) > 0) {
#     unmatched_sources <- data.table(
#       code = unmatched_icds, source = NA_character_, count = 0
#     )
#     # Loop over the columns (c1, c2, clin_icd) to fill
#     # in source and count details for unmatched codes.
#     for (col_name in c("c1", "c2", "clin_icd")) {
#       col_values <- get(col_name)
#       unmatched_sources[
#         code %in% unlist(col_values),
#         source := col_name
#       ]
#       unmatched_sources[
#         code %in% unlist(col_values),
#         count := count + table(unlist(col_values))[code]
#       ]
#     }
#     # Order unmatched codes by their occurrence count in descending order
#     unmatched_sources <- unmatched_sources[order(-count)]
#   } else {
#     # If there are no unmatched codes, return an empty data.table.
#     unmatched_sources <- data.table()
#   }

#   # Step 8: Create a data.table containing the
#   # mapping between PHL (input) ICD10 codes
#   # and Thai DRG ICD10 codes.
#   icd10_map <- data.table(
#     phl_icd10 = names(icd_mapping),
#     thai_icd10 = unlist(icd_mapping)
#   )

#   # Step 9: DEPRECATED

#   # Step 10: Create an environment from the ICD10
#   # mapping for fast lookup during column mapping.
#   icd10_env <- list2env(
#     setNames(as.list(icd10_map$thai_icd10), icd10_map$phl_icd10)
#   )

#   # Step 11: Apply the ICD10 mapping to the columns c1, c2, and clin_icd
#   # This updates these columns based on the generated ICD10 environment.
#   mapped_columns <- apply_icd10_mapping_to_columns(
#     c1, c2, clin_icd, icd10_env
#   )

#   # Step 12: Return a list containing the mapped columns
#   # and other information for further checks and outputs:
#   # - The updated columns (c1, c2, clin_icd)
#   # - The full ICD10 map (icd10_map_dt)
#   # - The unique ICD codes
#   # - Direct matches found
#   # - Unmatched ICDs and their source information
#   return(
#     list(
#       c1 = mapped_columns$c1,
#       c2 = mapped_columns$c2,
#       clin_icd = mapped_columns$clin_icd,
#       icd10_map_dt = icd10_map,
#       unique_icds = icds,
#       direct_matches = direct_match_codes,
#       unmatched = unmatched_icds,
#       unmatched_sources = unmatched_sources,
#       icd_mapping_res = icd_mapping
#     )
#   )
# }
