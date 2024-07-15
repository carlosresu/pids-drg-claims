# Helper function to check if a clinical code is an acceptable PDX
assess_pdx_code <- function(code, code_num) {
  #' @title Check PDX Code
  #' @description Checks if a clinical code is an acceptable PDX.
  #' @param code A clinical code to check.
  #' @param code_num The code number to return if the code is acceptable.
  #' @return A list containing the PDX and its code number, or NA if not acceptable.
  result <- if (!is.null(code) && code %in% acc_pdx) {  # Check if code is not NULL and is in the list of acceptable PDX codes
    list(pdx = code, pdx_code = code_num)
  } else {
    list(pdx = NA_character_, pdx_code = NA_integer_)
  }
  return(result)
}

# Helper function to find PDX from clinical ICD codes
find_pdx_from_icd <- function(clin_icd) {
  #' @title Find PDX from ICD Codes
  #' @description Finds the PDX from a list of clinical ICD codes.
  #' @param clin_icd A list of clinical ICD codes.
  #' @return A list containing the PDX and its code number.
  pdxs <- intersect(clin_icd, acc_pdx)
  result <- if (length(pdxs) == 0) {  # Check if no acceptable PDX codes are found
    list(pdx = NA_character_, pdx_code = 99)
  } else if (length(pdxs) == 1) {  # Check if exactly one acceptable PDX code is found
    list(pdx = pdxs[1], pdx_code = 3)
  } else {
    list(pdx = sample(pdxs, 1), pdx_code = 6)  # If multiple acceptable PDX codes are found, return a random one
  }
  return(result)
}

# Helper function to find the most similar PDX
find_most_similar_pdx <- function(code, pdxs) {
  #' @title Find Most Similar PDX
  #' @description Finds the most similar PDX from a list of PDX codes based on a given code.
  #' @param code A clinical code to compare.
  #' @param pdxs A list of PDX codes to compare against.
  #' @return A list containing the most similar PDX and its code number.
  starting_letter <- substr(code, 1, 1)
  starting_codes <- pdxs[substr(pdxs, 1, 1) == starting_letter]
  
  result <- if (length(starting_codes) == 1) {  # Check if exactly one PDX code starts with the same letter
    list(pdx = starting_codes[1], pdx_code = 4)
  } else if (length(starting_codes) > 1) {  # Check if multiple PDX codes start with the same letter
    similarities <- sapply(starting_codes, function(candidate) {
      sum(substr(code, 1, nchar(candidate)) == substr(candidate, 1, nchar(candidate)))
    })
    most_similar_pdx <- starting_codes[which.max(similarities)]
    list(pdx = most_similar_pdx, pdx_code = 5)
  } else {
    list(pdx = NA_character_, pdx_code = NA_integer_)
  }
  
  return(result)
}

# Main function to find the primary diagnosis (PDX)
find_pdx <- function(clin_c1, clin_c2, clin_icd) {
  #' @title Find Primary Diagnosis (PDX)
  #' @description Finds the primary diagnosis (PDX) in a given set of clinical codes.
  #' @param clin_c1 The first clinical code.
  #' @param clin_c2 The second clinical code.
  #' @param clin_icd A list of clinical ICD codes.
  #' @return A list containing the PDX and its code number.
  
  clin_icd <- unlist(clin_icd)
  
  # Check if clin_c1 or clin_c2 is an acceptable PDX
  pdx_check <- assess_pdx_code(clin_c1, 1)
  if (!is.na(pdx_check$pdx)) return(pdx_check)  # Return if clin_c1 is an acceptable PDX
  
  pdx_check <- assess_pdx_code(clin_c2, 2)
  if (!is.na(pdx_check$pdx)) return(pdx_check)  # Return if clin_c2 is an acceptable PDX
  
  # Find PDX from clinical ICD codes
  pdx_result <- find_pdx_from_icd(clin_icd)
  if (!is.na(pdx_result$pdx)) return(pdx_result)  # Return if a PDX is found from the ICD codes
  
  # Find the most similar PDX based on clin_c1 or clin_c2
  for (cr in list(clin_c1, clin_c2)) {
    if (!is.na(cr) && cr != "") {  # Check if clin_c1 or clin_c2 is not NA or empty
      most_similar_pdx <- find_most_similar_pdx(cr, pdx_result$pdx)
      if (!is.na(most_similar_pdx$pdx)) return(most_similar_pdx)  # Return if a similar PDX is found
    }
  }
  
  # If no specific match, return the result from find_pdx_from_icd
  return(pdx_result)
}

apply_find_pdx <- function(clin_c1, clin_c2, clin_icd) {
  #' @title Apply Find PDX to Specific Columns
  #' @description Applies the find_pdx function to specific columns and returns PDX and PDX code vectors.
  #' @param clin_c1 A vector of the first clinical codes.
  #' @param clin_c2 A vector of the second clinical codes.
  #' @param clin_icd A list of clinical ICD codes.
  #' @return A list containing the PDX vector and the PDX code vector.
  
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
  
  if (length(missing_pdx_indices) > 0) {  # Check if there are rows without a PDX
    for (i in missing_pdx_indices) {
      result <- find_pdx(clin_c1[i], clin_c2[i], clin_icd[[i]])
      pdx[i] <- result$pdx
      pdx_code[i] <- result$pdx_code
    }
  }
  
  return(list(pdx = pdx, pdx_code = pdx_code))
}
