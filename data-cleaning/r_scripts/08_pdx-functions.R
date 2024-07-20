find_pdx_from_icd <- function(clin_icd) {
  pdxs <- intersect(clin_icd, acc_pdx)
  result <- if (length(pdxs) == 0) { # Check if no acceptable PDX codes
    # are found
    list(pdx = NA_character_, pdx_code = 99)
  } else if (length(pdxs) == 1) { # Check if exactly one acceptable PDX
    # code is found
    list(pdx = pdxs[1], pdx_code = 3)
  } else {
    list(pdx = sample(pdxs, 1), pdx_code = 6) # If multiple acceptable
    # PDX codes are found, return a random one
  }
  return(result)
}

find_most_similar_pdx <- function(code, pdxs) {
  starting_letter <- substr(code, 1, 1)
  starting_codes <- pdxs[substr(pdxs, 1, 1) == starting_letter]

  result <- if (length(starting_codes) == 1) { # Check if exactly one
    # PDX code starts with the same letter
    list(pdx = starting_codes[1], pdx_code = 4)
  } else if (length(starting_codes) > 1) { # Check if multiple PDX
    # codes start with the same letter
    similarities <- sapply(starting_codes, function(candidate) {
      sum(
        substr(
          code, 1, nchar(candidate)
        ) == substr(
          candidate,
          1,
          nchar(candidate)
        )
      )
    })
    most_similar_pdx <- starting_codes[which.max(similarities)]
    list(pdx = most_similar_pdx, pdx_code = 5)
  } else {
    list(pdx = NA_character_, pdx_code = NA_integer_)
  }

  return(result)
}

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
  pdx_result <- find_pdx_from_icd(clin_icd)
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

apply_find_pdx <- function(clin_c1, clin_c2, clin_icd, acc_pdx) {
  n <- length(clin_c1)
  pdx <- character(n)
  pdx_code <- integer(n)

  # Assign PDX based on clin_c1 and clin_c2
  pdx[clin_c1 %in% acc_pdx] <- clin_c1[clin_c1 %in% acc_pdx]
  pdx_code[clin_c1 %in% acc_pdx] <- 1

  pdx[clin_c2 %in% acc_pdx] <- clin_c2[clin_c2 %in% acc_pdx]
  pdx_code[clin_c2 %in% acc_pdx] <- 2

  # Identify rows without a PDX
  missing_pdx_indices <- which(is.na(pdx) | pdx == "")

  if (length(missing_pdx_indices) > 0) {
    # Check if there are rows without a PDX
    for (i in missing_pdx_indices) {
      result <- find_pdx(clin_c1[i], clin_c2[i], clin_icd[[i]], acc_pdx)
      if (!is.na(result$pdx) && !(result$pdx %in% acc_pdx)) {
        stop(sprintf("Invalid PDX code found: %s", result$pdx))
      }
      pdx[i] <- result$pdx
      pdx_code[i] <- result$pdx_code
    }
  }

  return(list(pdx = pdx, pdx_code = pdx_code))
}
