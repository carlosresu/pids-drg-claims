# TODO: Delete this, we don't use it anymore
# Function to collapse the replaced text with "||" as separator
collapse_to_string <- function(vec) {
  # Collapse non-empty elements with "||" as the separator
  vec <- vec[vec != "" & !is.na(vec)]
  if (length(vec) > 0) {
    vector <- paste(vec, collapse = "||")
    return(vector)
  } else {
    empty_vec <- NA_character_
    return(empty_vec)
  }
}

# TODO: Delete this, we don't use it anymore
# USING ENVIRONMENTS
# Function to process ICD codes
remove_lumped_icd_codes <- function(column) {
  ## Processes a list column of character vectors, splitting lumped ICD-10 codes
  unlumped <- lapply(as.list(column), function(vec) {
    # Iterate through each element of the vector
    processed <- unlist(lapply(vec, function(element) {
      if ((is.na(element) || element == "")
      ) {
        # Keep intact if it's a valid neoplasm or COVID code
        return(character())
      } else {
        # Perform regex-based splitting for ICD-10
        # codes using the combined regex
        return(unlist(strsplit(element, "(?<=\\d)(?=[A-Z][0-9]{2,})",
          perl = TRUE
        )))
      }
    }))

    # Filter out empty strings and return the cleaned vector
    processed <- processed[processed != ""]
    return(processed)
  })
  return(unlumped)
}

# TODO: Delete this, we don't use it anymore
remove_lumped_rvs_codes <- function(column) {
  ## Separates out lumped RVS codes by splitting into chunks of 5 chars each
  modified_column <- sapply(
    as.character(column),
    function(code) {
      # Check if the code is NA, empty, or NULL, and return NA if so
      if (is.na(code) || code == "" || is.null(code)) {
        return(NA_character_)
      }

      # Remove all non-alphanumeric characters and clean the code
      # Remove all "|" characters
      code_clean <- gsub("\\|", "", code)
      # Remove non-alphanumeric characters
      code_clean <- gsub("[^A-Z0-9]", "", code_clean)

      # If the cleaned code length is 0, return NA
      if (nchar(code_clean) == 0) {
        return(NA_character_)
      }

      # If the cleaned code length is not a multiple of 5,
      # log a message and return NA
      if (nchar(code_clean) %% 5 != 0) {
        return(NA_character_)
      }

      # Insert "||" every 5 characters to split the code
      modified_code <- gsub("(.{5})", "\\1||", code_clean)

      # Remove trailing "||" if present
      modified_code <- gsub("\\|\\|$", "", modified_code)

      return(modified_code)
    },
    USE.NAMES = FALSE
  )
  return(modified_column) # Return the modified column with split RVS codes
}

# TODO: Delete this, we don't use it anymore
remove_whitespace <- function(x) {
  if (is.null(x) || length(x) == 0) {
    # Return NA for NULL or empty lists
    return(NA_character_)
  } else {
    # Remove all whitespace characters
    return(gsub("\\s+", "", x))
  }
}

# TODO: Delete this, we don't use it anymore
flatten_then_check_empty <- function(input) {
  # Fully flatten all nested lists into a character vector
  input <- unlist(input, recursive = TRUE)

  # Check if the flattened result is empty or only contains NULL/NA
  if (length(input) == 0 || all(is.null(input)) || all(is.na(input))) {
    return("\u200B") # Temporary placeholder for empty columns
  } else {
    return(input) # Already a flat character vector
  }
}

# TODO: Delete this, we don't use it anymore
prep_icd_for_mapping <- function(text) {
  text %>%
    manual_replacement() %>%
    collapse_to_string() %>%
    split_to_vector() %>%
    remove_lumped_icd_codes() %>%
    flatten_then_check_empty()
}

# TODO: Delete this, we don't use it anymore
# Helper function to handle NULL or NA safely
safe_split <- function(x) {
  if (is.null(x) || all(is.na(x))) {
    # Return an empty character vector for consistency
    return(NA_character_)
  }
  # Split valid strings by '|'
  unlisted_and_split <- unlist(strsplit(x, "\\|"))
  return(unlisted_and_split)
}
