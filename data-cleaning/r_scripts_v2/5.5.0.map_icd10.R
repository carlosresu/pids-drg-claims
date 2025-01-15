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
# 2. did you document it?
# 3. no, really, did you?
# 4. ok go rearrange
# 5. test
############################# SPECIFIC TODO #############################

############################# WHAT IS A DOCSTRING? #############################
# Docstring that covers the bigger picture of the function
# Such as why it exists, what it intends to do, and why it was written
############################# WHAT IS A DOCSTRING? #############################

# NTS: TODO: This can probably be de-functionized and put in the script
#      everywhere it's needed
trim_code <- function(code) {
  # Trims ICD-10 codes with 4+ digits, into one with exactly has 3 digits
  # e.g. J1892 and J18923 both become J189
  # NOTE: if regex is not met, e.g. J18, it will be returned unchanged
  sub("(\\D+\\d{3})(\\d*)$", "\\1", code)
}

# OLD VERSION FOR REFERENCE
# [[1]] is necessary because mget always returns a list, AND
# because is.null expects a NULL value directly,
# not a list containing NULL
code_exists <- function(code, env) {
  # Returns TRUE if code exists in/is a valid thai code(s))
  !is.null(mget(x = code, envir = env, ifnotfound = list(NULL))[[1]])
}

# REFACTORED VERSION WITH DOCSTRINGS/COMMENTAS
code_exists <- function(code, env) {
  # Returns TRUE if code (key: a string) exists (value: a boolean) in an env
  exists(x = code, envir = env, inherits = FALSE)
}

generate_icd10_mapping <- function(filtered_icds) {
  # Initialize things
  icd_mapping <- list()
  generated_direct_matches <- character()
  generated_modified_matches <-
    # a two-element list of the raw match and its modified counterpart
    list(raw_match = character(), modified_match = character())

  # loop through every element of icds that are filtered
  for (code in filtered_icds) {
    # 0. ** trim whitespace for ALL codes
    code <- trimws(code)

    # 1. **Exact match check**
    # If code exists directly/exactly,
    if (code_exists(code, icd_codes_env)) {
      icd_mapping[[code]] <-
        # add that code to generated mapping
        list(
          match_type = "Exact",
          original = code,
          mapped = code
        )

      # also, add output to checks
      generated_direct_matches <- c(generated_direct_matches, code)

      # then, continue the loop and bypass code below
      next # Skip further processing for this code
    }

    # 2. **Attempt adding '9' for 3-character codes, then check for existence**
    # If code is 3 characters long, e.g. J18
    if (nchar(code) == 3) {
      # first, add 9 to it
      modified_code <- paste0(code, "9")

      # If once with a 9 code exists in env,
      if (code_exists(modified_code, icd_codes_env)) {
        # add said code to generated mapping
        icd_mapping[[code]] <-
          list(
            match_type = "Modified (Added 9)",
            original = code,
            mapped = modified_code
          )

        # also, add raw version of code to checks
        generated_modified_matches$raw_match <-
          c(generated_modified_matches$raw_match, code)

        # also, add said code to checks
        generated_modified_matches$modified_match <-
          c(generated_modified_matches$modified_match, modified_code)

        # then, continue the loop and bypass code below
        next # Skip further processing for this code
      }
    }

    # 3. **Trimming codes longer than or equal to 4 char**
    trimmed_code <- if (nchar(code) > 4) trim_code(code) else if (nchar(code) == 4) code else NULL

    # For non-null 4 char codes,
    if (!is.null(trimmed_code)) {
      # Check if the 4-character trimmed code exists
      if (code_exists(trimmed_code, icd_codes_env)) {
        icd_mapping[[code]] <- list(
          match_type = "Modified (Trimmed)",
          original = code,
          mapped = trimmed_code
        )

        # also, add raw to checks
        generated_modified_matches$raw_match <- c(generated_modified_matches$raw_match, code)

        # also, add trimmed to checks
        generated_modified_matches$modified_match <- c(generated_modified_matches$modified_match, trimmed_code)

        # Skip further processing for this code
        next
      }

      # If 4 char code doesn't exist, trim again, to 3 char
      trimmed_to_3 <- substr(trimmed_code, 1, 3)
      if (code_exists(trimmed_to_3, icd_codes_env)) {
        icd_mapping[[code]] <- list(
          match_type = "Modified (Trimmed)",
          original = code,
          mapped = trimmed_to_3
        )

        # also, add raw to checks
        generated_modified_matches$raw_match <- c(generated_modified_matches$raw_match, code)

        # also, add trimmed to checks
        generated_modified_matches$modified_match <- c(generated_modified_matches$modified_match, trimmed_to_3)

        # Skip further processing for this code
        next
      }
    }

    # 4. **Mark as unmatched once all else fails**
    icd_mapping[[code]] <- list(
      match_type = "Unmatched",
      original = code,
      mapped = NA_character_
    )
  }

  list(
    mapping = icd_mapping,
    returned_modified_matches = generated_modified_matches,
    returned_direct_matches = generated_direct_matches
  )
}

# Apply the mapping to input columns
apply_icd10_mapping <- function(codes) {
  unname(sapply(codes, function(code) {
    if (!is.null(icd_mapping[[code]]) && !is.null(icd_mapping[[code]]$mapped)) {
      icd_mapping[[code]]$mapped
    } else {
      code
    }
  }))
}

# Updated map_icd10 function using covid_rvs_neoplasm_env
map_icd10 <- function(c1, c2, clin_icd) {
  # Collect and pre-filter unique ICD codes, excluding those in covid_rvs_neoplasm_env
  icds <- unique(c(unlist(c1), unlist(c2), unlist(clin_icd)))
  filtered_icds <- icds[!is.na(icds) &
    !grepl("^[0-9]", icds) &
    !grepl("^[A-Z]{2}", icds) &
    !grepl("/", icds) &
    !sapply(icds, function(code) code_exists(code, covid_rvs_neoplasm_env))]

  # Generate the ICD-10 mapping
  mapping_info <- generate_icd10_mapping(filtered_icds)
  icd_mapping <- mapping_info$mapping

  # Identify unmatched codes
  unmatched_codes <- names(Filter(function(x) x$match_type == "Unmatched", icd_mapping))

  # Create a data.table of unmatched codes by source
  unmatchedsources <- rbindlist(
    lapply(
      c("c1", "c2", "clin_icd"), # hardcoded names of columns to process
      function(col_name) {
        col_values <- get(col_name)

        # Flatten the list, filter out NAs or NULLs, and convert to character
        flattened_values <- unlist(col_values)
        valid_codes <- flattened_values[
          !is.na(
            flattened_values
          ) & flattened_values != ""
        ]

        if (length(valid_codes) > 0) {
          data.table(
            code = valid_codes,
            source = col_name
          )[,
            .(count = .N),
            by = .(
              code,
              source
            )
          ]
        } else {
          data.table(
            code = character(),
            source = character(),
            count = integer()
          )
        }
      }
    ),
    fill = TRUE
  )

  # Filter unmatched sources based on unmatched codes
  # %chin% is a faster version of %in% for character vectors
  unmatchedsources <- unmatchedsources[code %chin% unmatched_codes]

  # Create ICD-10 mapping data.table
  icd10_map <- data.table(
    phl_icd10 = names(icd_mapping),
    # STUDY: what does `[[` do
    thai_icd10 = sapply(icd_mapping, `[[`, "mapped"),
    match_type = sapply(icd_mapping, `[[`, "match_type")
  )

  # Apply mappings
  c1_mapped <- lapply(
    c1,
    apply_icd10_mapping
  )
  c2_mapped <- lapply(
    c2,
    apply_icd10_mapping
  )
  clin_icd_mapped <- lapply(
    clin_icd,
    apply_icd10_mapping
  )

  # Return the results
  return(
    list(
      c1 = c1_mapped,
      c2 = c2_mapped,
      clin_icd = clin_icd_mapped,
      icd10_map_dt_for_checks = icd10_map,
      unique_icds_for_checks = icds,
      unmatched_codes_for_checks = unmatched_codes,
      unmatched_sources_for_checks = unmatchedsources,
      icd_mapping_for_checks = icd_mapping,
      modified_matches_for_checks = mapping_info$returned_modified_matches,
      direct_matches_for_checks = mapping_info$returned_direct_matches
    )
  )
}
