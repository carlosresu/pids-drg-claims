# Pre-split any input that already contains '||'
pre_split_on_double_pipe <- function(col) {
  lapply(col, function(vec) {
    # Final Step: Split each element of result by "||"
    # and flatten the output
    final_result <- unlist(lapply(vec, function(elem) {
      strsplit(elem, "\\|\\|", perl = TRUE)[[1]]
    }))

    # Remove any empty strings
    clean_result <- final_result[final_result != ""]
  })
}

# Pre-split any input that already contains '|'
pre_split_on_single_pipe <- function(col) {
  lapply(col, function(vec) {
    # Final Step: Split each element of result by "|"
    # and flatten the output
    final_result <- unlist(lapply(vec, function(elem) {
      strsplit(elem, "\\|", perl = TRUE)[[1]]
    }))

    # Remove any empty strings
    clean_result <- final_result[final_result != ""]
  })
}

# Remove any entities with less than 3 characters
filter_short_codes <- function(col) {
  lapply(col, function(vec) vec[nchar(vec) >= 3])
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

# ICD dictionary setup
split_icd_by_prefix <- function(icd_codes) {
  prefix_map <- split(icd_codes, substr(icd_codes, 1, 1))
  prefix_map[names(prefix_map) %in% LETTERS]
}

extend_icd_dict_with_custom_codes <- function(icd_dict, custom_codes) {
  valid_custom <- custom_codes[!grepl("^[0-9]", custom_codes)]
  custom_by_prefix <- split(valid_custom, substr(valid_custom, 1, 1))

  for (prefix in names(custom_by_prefix)) {
    if (prefix %in% names(icd_dict)) {
      icd_dict[[prefix]] <- unique(c(icd_dict[[prefix]], custom_by_prefix[[prefix]]))
    } else {
      icd_dict[[prefix]] <- unique(custom_by_prefix[[prefix]])
    }
  }

  icd_dict
}

sort_icd_dict_by_length <- function(icd_dict) {
  lapply(icd_dict, function(codes) codes[order(-nchar(codes))])
}

# Stage 1: match any known custom codes using regex
step1_match <- function(text) {
  pattern <- paste0(custom_codes_sorted, collapse = "|")
  matches <- regmatches(text, gregexpr(pattern, text, perl = TRUE))[[1]]

  if (length(matches) == 0) {
    return(list(matches = character(0), remainders = text))
  }

  text_no_match <- text
  for (m in matches) {
    text_no_match <- gsub(m, " ", text_no_match, fixed = TRUE)
  }

  remainders <- unlist(strsplit(text_no_match, "\\s+"))
  remainders <- remainders[nzchar(remainders)]

  list(matches = matches, remainders = remainders)
}

# Stage 2: split by digit → letter transitions
chunk_by_letter_switch <- function(strings) {
  chunks <- c()
  for (s in strings) {
    marked <- gsub("(?<=[0-9])(?=[A-Z])", "||", s, perl = TRUE)
    parts <- unlist(strsplit(marked, "\\|\\|"))
    parts <- parts[nzchar(parts)]
    chunks <- c(chunks, parts)
  }
  chunks
}

# Stage 3: regex match using relevant ICD codes based on starting letter
step3_match <- function(chunks) {
  matches <- c()
  remainders <- c()

  for (chunk in chunks) {
    prefix <- substr(chunk, 1, 1)
    if (!prefix %in% LETTERS) {
      remainders <- c(remainders, chunk)
      next
    }

    relevant_codes <- grep(paste0("^", prefix), all_codes, value = TRUE)
    if (length(relevant_codes) == 0) {
      remainders <- c(remainders, chunk)
      next
    }

    pattern <- paste0(relevant_codes, collapse = "|")
    found <- regmatches(chunk, gregexpr(pattern, chunk, perl = TRUE))[[1]]

    if (length(found) > 0 && nzchar(found[1])) {
      matches <- c(matches, found)
      chunk_clean <- chunk
      for (f in found) {
        chunk_clean <- gsub(f, " ", chunk_clean, fixed = TRUE)
      }
      leftovers <- unlist(strsplit(chunk_clean, "\\s+"))
      leftovers <- leftovers[nzchar(leftovers)]
      remainders <- c(remainders, leftovers)
    } else {
      remainders <- c(remainders, chunk)
    }
  }

  list(matches = matches, remainders = remainders)
}

step4_match <- function(chunks) {
  results <- list(matches = character(0), remainders = character(0))

  for (chunk in chunks) {
    if (grepl("^[0-9]", chunk)) {
      digit_prefix <- substr(chunk, 1, 1)
      relevant_custom_rvs <- custom_rvs_codes[substr(custom_rvs_codes, 1, 1) == digit_prefix]

      if (length(relevant_custom_rvs) > 0) {
        pattern <- paste0(relevant_custom_rvs, collapse = "|")
        found <- regmatches(chunk, gregexpr(pattern, chunk, perl = TRUE))[[1]]

        if (length(found) > 0 && nzchar(found[1])) {
          results$matches <- c(results$matches, found)

          chunk_clean <- chunk
          for (f in found) {
            chunk_clean <- gsub(f, " ", chunk_clean, fixed = TRUE)
          }

          leftovers <- unlist(strsplit(chunk_clean, "\\s+"))
          leftovers <- leftovers[nzchar(leftovers)]
          results$remainders <- c(results$remainders, leftovers)
          next
        }
      }
    }

    results$remainders <- c(results$remainders, chunk)
  }

  results
}

# Final pipeline
delump_icd_staged <- function(list_col) {
  lapply(list_col, function(char_vec) {
    unlist(lapply(char_vec, function(text) {
      if (is.na(text) || text == "") {
        return(character(0))
      }
      s1 <- step1_match(text)
      if (to_debug) cat("step 1 matches\n")
      if (to_debug) str(s1$matches)
      chunks <- chunk_by_letter_switch(s1$remainders)
      s3 <- step3_match(chunks)
      if (to_debug) cat("step 3 matches\n")
      if (to_debug) str(s3$matches)
      if (to_debug) cat("step 3 remainders\n")
      if (to_debug) str(s3$remainders)
      s4 <- step4_match(s3$remainders)
      if (to_debug) cat("step 4 matches\n")
      if (to_debug) str(s4$matches)
      if (to_debug) cat("step 4 remainders\n")
      if (to_debug) str(s4$remainders)
      final_result <- c(s1$matches, s3$matches, s4$matches, s4$remainders)
      if (length(final_result) == 0) {
        return(as.character(text))
      }
      final_result
    }), recursive = FALSE)
  })
}
