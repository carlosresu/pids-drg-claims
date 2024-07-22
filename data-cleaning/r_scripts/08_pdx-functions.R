# Function to find PDX from clinical ICD codes
find_pdx_from_icd <- function(clin_icd, acc_pdx) {
  pdxs <- intersect(clin_icd, acc_pdx)
  if (length(pdxs) == 0) {
    return(list(pdx = NA_character_, pdx_code = 99))
  } else if (length(pdxs) == 1) {
    return(list(pdx = pdxs[1], pdx_code = 3))
  } else {
    return(list(pdx = sample(pdxs, 1), pdx_code = 6))
  }
}

# Function to find the most similar PDX
find_most_similar_pdx <- function(code, pdxs) {
  if (is.null(pdxs) || length(pdxs) == 0) {
    return(list(pdx = NA_character_, pdx_code = NA_integer_))
  }

  starting_letter <- substr(code, 1, 1)
  starting_codes <- pdxs[substr(pdxs, 1, 1) == starting_letter]

  if (length(starting_codes) == 1) {
    return(list(pdx = starting_codes[1], pdx_code = 4))
  } else if (length(starting_codes) > 1) {
    similarities <- sapply(starting_codes, function(candidate) {
      sum(substr(code, 1, nchar(candidate)) == substr(candidate, 1, nchar(candidate)))
    })
    most_similar_pdx <- starting_codes[which.max(similarities)]
    return(list(pdx = most_similar_pdx, pdx_code = 5))
  } else {
    return(list(pdx = NA_character_, pdx_code = NA_integer_))
  }
}

# Function to find the primary diagnosis (PDX)
find_pdx <- function(clin_c1, clin_c2, clin_icd, acc_pdx) {
  clin_icd <- unlist(clin_icd)

  # Helper function to check if a clinical code is an acceptable PDX
  assess_pdx_code <- function(code, code_num, acc_pdx) {
    if (!is.null(code) && code %in% acc_pdx) {
      return(list(pdx = code, pdx_code = code_num))
    } else {
      return(list(pdx = NA_character_, pdx_code = NA_integer_))
    }
  }

  # Check if clin_c1 or clin_c2 is an acceptable PDX
  pdx_check <- assess_pdx_code(clin_c1, 1, acc_pdx)
  if (!is.na(pdx_check$pdx)) {
    return(pdx_check)
  }

  pdx_check <- assess_pdx_code(clin_c2, 2, acc_pdx)
  if (!is.na(pdx_check$pdx)) {
    return(pdx_check)
  }

  # Find PDX from clinical ICD codes
  pdx_result <- find_pdx_from_icd(clin_icd, acc_pdx)
  if (!is.na(pdx_result$pdx)) {
    return(pdx_result)
  }

  # Find the most similar PDX based on clin_c1 or clin_c2
  for (cr in list(clin_c1, clin_c2)) {
    if (!is.na(cr) && cr != "") {
      most_similar_pdx <- find_most_similar_pdx(cr, pdx_result$pdx)
      if (!is.na(most_similar_pdx$pdx)) {
        return(most_similar_pdx)
      }
    }
  }

  # If no specific match, return the result from find_pdx_from_icd
  return(pdx_result)
}

# Function to apply find_pdx to a dataset
apply_find_pdx <- function(clin_c1, clin_c2, clin_icd, acc_pdx) {
  n <- length(clin_c1)
  pdx <- character(n)
  pdx_code <- integer(n)

  # Assign PDX based on clin_c1 and clin_c2
  for (i in 1:n) {
    result <- find_pdx(clin_c1[i], clin_c2[i], clin_icd[[i]], acc_pdx)
    if (!is.na(result$pdx) && !(result$pdx %in% acc_pdx)) {
      stop(sprintf("Invalid PDX code found: %s", result$pdx))
    }
    pdx[i] <- result$pdx
    pdx_code[i] <- result$pdx_code
  }

  return(list(pdx = pdx, pdx_code = pdx_code))
}
