# Pre-split any input that already contains '||'
pre_split_on_double_pipe <- function(col) {
  lapply(col, function(vec) {
    # For each element in the input vector
    # (which is expected to be a list of strings),
    # split each string by the "||" delimiter
    final_result <- unlist(lapply(vec, function(elem) {
      strsplit(elem, "\\|\\|", perl = TRUE)[[1]]
    }))

    # Remove any empty strings resulting from the split
    clean_result <- final_result[final_result != ""]
  })
}

# Pre-split any input that already contains '|'
pre_split_on_single_pipe <- function(col) {
  lapply(col, function(vec) {
    # For each element in the input vector (expected to be a list of strings),
    # split each string by the single pipe "|" delimiter
    final_result <- unlist(lapply(vec, function(elem) {
      strsplit(elem, "\\|", perl = TRUE)[[1]]
    }))

    # Remove any empty strings resulting from the split
    # (e.g. from "||" or leading/trailing pipes)
    clean_result <- final_result[final_result != ""]
  })
}

# Remove any entities with less than 3 characters
filter_short_codes <- function(col) {
  lapply(col, function(vec) {
    # Keep only elements in the vector that have 3 or more characters
    vec[nchar(vec) >= 3]
  })
}

split_to_vector <- function(column) {
  lapply(column, function(long_string) {
    # Initialize result vector
    result <- character()

    # Ensure long_string is not NA before proceeding
    if (is.na(long_string)) {
      return(result)
    }

    # Step 1: Extract COVID codes
    covid_matches <- gregexpr(covid_pattern, long_string, perl = TRUE)[[1]]
    if (!is.na(covid_matches[1]) && covid_matches[1] != -1) {
      covid_codes <- regmatches(long_string, list(covid_matches))[[1]]
      result <- c(result, covid_codes)
      # Remove COVID codes from long_string
      long_string <- gsub(covid_pattern, "", long_string, perl = TRUE)
    }

    # Step 2: Extract Neoplasm codes
    neoplasm_matches <- gregexpr(neoplasm_pattern,
      long_string,
      perl = TRUE
    )[[1]]
    if (!is.na(neoplasm_matches[1]) && neoplasm_matches[1] != -1) {
      neoplasm_codes <- regmatches(long_string, list(neoplasm_matches))[[1]]
      result <- c(result, neoplasm_codes)
      # Remove Neoplasm codes from long_string
      long_string <- gsub(neoplasm_pattern, "", long_string, perl = TRUE)
    }

    # Step 3: Extract RVS codes using individual patterns
    rvs_patterns <- c(
      "[A-Za-z]{3}[0-9]{2}", # Three letters followed by one or two digits
      "[A-Za-z]{2}[0-9]{3}", # Two letters followed by two or three digits
      "[A-Za-z][0-9]{4}", # A letter followed by four or five digits
      # TODO: delete [A-Za-z][0-9]{4} because it could match an ICD code
      "[0-9]{5}" # Five consecutive numbers
    )

    for (pattern in rvs_patterns) {
      rvs_matches <- gregexpr(pattern, long_string, perl = TRUE)[[1]]
      if (!is.na(rvs_matches[1]) && rvs_matches[1] != -1) {
        rvs_codes <- regmatches(long_string, list(rvs_matches))[[1]]
        result <- c(result, rvs_codes)
        # Remove RVS codes from long_string
        long_string <- gsub(pattern, "", long_string, perl = TRUE)
      }
    }

    # Step 4: Remaining content in long_string should be lumped ICD codes
    if (!is.na(long_string) && nchar(long_string) > 0) {
      result <- c(result, long_string)
    }

    # Final Step: Split each element of result by "||" and flatten the output
    final_result <- unlist(lapply(result, function(element) {
      strsplit(element, "\\|\\|", perl = TRUE)[[1]]
    }))

    # Remove any empty strings
    clean_result <- final_result[final_result != ""]
    return(clean_result)
  })
}

# ICD dictionary setup: group ICD codes by their first character (prefix)
split_icd_by_prefix <- function(icd_codes) {
  # Split the input vector of ICD codes into groups based on their first letter
  prefix_map <- split(icd_codes, substr(icd_codes, 1, 1))

  # Return only those groups whose prefix is an uppercase letter A–Z
  prefix_map[names(prefix_map) %in% LETTERS]
}

extend_icd_dict_with_custom_codes <- function(icd_dict, custom_codes) {
  # Filter out custom codes that start with a digit
  # (keep only those starting with letters)
  valid_custom <- custom_codes[!grepl("^[0-9]", custom_codes)]

  # Group valid custom codes by their first letter (prefix)
  custom_by_prefix <- split(valid_custom, substr(valid_custom, 1, 1))

  # Merge custom codes into the existing icd_dict by prefix
  for (prefix in names(custom_by_prefix)) {
    if (prefix %in% names(icd_dict)) {
      # If prefix already exists, append and deduplicate
      icd_dict[[prefix]] <- unique(c(
        icd_dict[[prefix]],
        custom_by_prefix[[prefix]]
      ))
    } else {
      # If prefix is new, create a new entry
      icd_dict[[prefix]] <- unique(custom_by_prefix[[prefix]])
    }
  }

  # Return the updated ICD dictionary
  icd_dict
}

# Sort ICD dictionary entries by code length in descending order
sort_icd_dict_by_length <- function(icd_dict) {
  lapply(icd_dict, function(codes) {
    # Within each prefix group, sort codes so that longer codes come first
    codes[order(-nchar(codes))]
  })
}

# Stage 1: match any known custom codes using regex
step1_match <- function(text) {
  # Build a single regex pattern from all known custom codes
  # (longer ones should come first)
  pattern <- paste0(custom_codes_sorted, collapse = "|")

  # Extract all matches of custom codes from the input text
  matches <- regmatches(text, gregexpr(pattern, text, perl = TRUE))[[1]]

  # If no matches are found, return the original text as a
  # single-element remainder
  if (length(matches) == 0) {
    return(list(matches = character(0), remainders = text))
  }

  # Initialize a copy of the original text for removing matches
  text_no_match <- text

  # Replace each match in the text with a space (to isolate remainders cleanly)
  for (m in matches) {
    text_no_match <- gsub(m, " ", text_no_match, fixed = TRUE)
  }

  # Split the modified text by whitespace to extract non-matching parts
  remainders <- unlist(strsplit(text_no_match, "\\s+"))

  # Filter out any empty strings from the remainders
  remainders <- remainders[nzchar(remainders)]

  # Return a list of matched codes and remaining unmatched substrings
  list(matches = matches, remainders = remainders)
}

# Stage 2: split strings by digit → letter transitions
# (e.g., "123A" → "123", "A")
chunk_by_letter_switch <- function(strings) {
  chunks <- c() # Initialize empty vector to hold all resulting chunks

  for (s in strings) {
    # Insert "||" delimiter between a digit followed
    # immediately by an uppercase letter
    marked <- gsub("(?<=[0-9])(?=[A-Z])", "||", s, perl = TRUE)

    # Split the marked string by the custom "||" delimiter
    parts <- unlist(strsplit(marked, "\\|\\|"))

    # Remove any empty strings from the result
    parts <- parts[nzchar(parts)]

    # Collect all valid chunks
    chunks <- c(chunks, parts)
  }

  return(chunks)
}

# Stage 3: regex match using relevant ICD codes based on
# starting letter of each chunk
step3_match <- function(chunks) {
  matches <- c() # Store ICD code matches
  remainders <- c() # Store unmatched substrings

  for (chunk in chunks) {
    # Extract the first character as the prefix (assumed to be a letter)
    prefix <- substr(chunk, 1, 1)

    # If prefix is not an uppercase letter (A-Z), treat chunk as a remainder
    if (!prefix %in% LETTERS) {
      remainders <- c(remainders, chunk)
      next
    }

    # Get all known codes that start with the same prefix letter
    relevant_codes <- grep(paste0("^", prefix), all_codes, value = TRUE)

    # If no relevant codes are found, treat as unmatched
    if (length(relevant_codes) == 0) {
      remainders <- c(remainders, chunk)
      next
    }

    # Construct regex pattern from relevant codes
    pattern <- paste0(relevant_codes, collapse = "|")

    # Attempt to find any matches in the current chunk
    found <- regmatches(chunk, gregexpr(pattern, chunk, perl = TRUE))[[1]]

    if (length(found) > 0 && nzchar(found[1])) {
      # If matches are found, add them to the result
      matches <- c(matches, found)

      # Remove matched segments from chunk to isolate any remaining substrings
      chunk_clean <- chunk
      for (f in found) {
        chunk_clean <- gsub(f, " ", chunk_clean, fixed = TRUE)
      }

      # Split leftovers by whitespace and filter empty strings
      leftovers <- unlist(strsplit(chunk_clean, "\\s+"))
      leftovers <- leftovers[nzchar(leftovers)]

      # Add leftovers to remainders
      remainders <- c(remainders, leftovers)
    } else {
      # If no matches are found, treat entire chunk as a remainder
      remainders <- c(remainders, chunk)
    }
  }

  # Return both matches and remaining unmatched substrings
  list(matches = matches, remainders = remainders)
}

# Stage 4: Match codes that begin with digits (typically RVS/custom codes)
step4_match <- function(chunks) {
  # Initialize result structure with empty matches and remainders
  results <- list(matches = character(0), remainders = character(0))

  for (chunk in chunks) {
    # Check if the chunk starts with a digit
    if (grepl("^[0-9]", chunk)) {
      # Extract the first digit to use as a prefix for
      # filtering relevant RVS codes
      digit_prefix <- substr(chunk, 1, 1)

      # Filter custom RVS codes that begin with the same digit
      relevant_custom_rvs <- custom_rvs_codes[
        substr(custom_rvs_codes, 1, 1) == digit_prefix
      ]

      if (length(relevant_custom_rvs) > 0) {
        # Build regex pattern from the relevant codes
        pattern <- paste0(relevant_custom_rvs, collapse = "|")

        # Search for any matches in the chunk
        found <- regmatches(chunk, gregexpr(pattern, chunk, perl = TRUE))[[1]]

        if (length(found) > 0 && nzchar(found[1])) {
          # If matches are found, append them to the results
          results$matches <- c(results$matches, found)

          # Remove matched strings from the chunk
          chunk_clean <- chunk
          for (f in found) {
            chunk_clean <- gsub(f, " ", chunk_clean, fixed = TRUE)
          }

          # Split the cleaned chunk into remainders, filtering out empty strings
          leftovers <- unlist(strsplit(chunk_clean, "\\s+"))
          leftovers <- leftovers[nzchar(leftovers)]

          # Append leftovers to remainders
          results$remainders <- c(results$remainders, leftovers)

          # Continue to next chunk
          next
        }
      }
    }

    # If not matched or not digit-prefixed, add the chunk to remainders
    results$remainders <- c(results$remainders, chunk)
  }

  # Return both matches and remainders
  results
}

# Final pipeline: staged delumping of ICD and related codes
# from concatenated text
delump_icd_staged <- function(list_col) {
  lapply(list_col, function(char_vec) {
    # Apply processing to each character string in the vector
    unlist(lapply(char_vec, function(text) {
      # Handle NA or empty input
      if (is.na(text) || text == "") {
        return(character(0))
      }

      # Step 1: Match known custom codes (e.g., neoplasm, COVID, RVS-alphabetic)
      s1 <- step1_match(text)
      if (to_debug) cat("step 1 matches\n")
      if (to_debug) str(s1$matches)

      # Step 2: Split remaining text into chunks by digit→letter transitions
      chunks <- chunk_by_letter_switch(s1$remainders)

      # Step 3: Match ICD codes using starting letter (alphabetic-prefixed)
      s3 <- step3_match(chunks)
      if (to_debug) cat("step 3 matches\n")
      if (to_debug) str(s3$matches)
      if (to_debug) cat("step 3 remainders\n")
      if (to_debug) str(s3$remainders)

      # Step 4: Match numeric-prefixed custom codes (e.g., RVS codes)
      s4 <- step4_match(s3$remainders)
      if (to_debug) cat("step 4 matches\n")
      if (to_debug) str(s4$matches)
      if (to_debug) cat("step 4 remainders\n")
      if (to_debug) str(s4$remainders)

      # Combine all extracted matches and remaining unmatched chunks
      final_result <- c(s1$matches, s3$matches, s4$matches, s4$remainders)

      # If nothing was extracted, return the original text as a fallback
      if (length(final_result) == 0) {
        return(as.character(text))
      }

      # Return all matches and any leftovers
      final_result
    }), recursive = FALSE) # Prevent deep recursion; ensure proper flattening
  })
}
