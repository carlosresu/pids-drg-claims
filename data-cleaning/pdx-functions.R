# Helper function to check if a clinical code is an acceptable PDX
assess_pdx_code <- function(code, acc_pdx_vector = acc_pdx, code_num) {
  #' @title Check PDX Code
  #' @description Checks if a clinical code is an acceptable PDX.
  #' @param code A clinical code to check.
  #' @param acc_pdx_vector A vector of acceptable PDX codes.
  #' @param code_num The code number to return if the code is acceptable.
  #' @return A list containing the PDX and its code number, or NULL if not acceptable.
  if (!is.null(code) && code %in% acc_pdx_vector) {
    return(list(pdx = code, pdx_code = code_num))
  }
  return(NULL)
}

# Helper function to find PDX from clinical ICD codes
find_pdx_from_icd <- function(clin_icd, acc_pdx_vector = acc_pdx) {
  #' @title Find PDX from ICD Codes
  #' @description Finds the PDX from a list of clinical ICD codes.
  #' @param clin_icd A list of clinical ICD codes.
  #' @param acc_pdx_vector A vector of acceptable PDX codes.
  #' @return A list containing the PDX and its code number, or NULL if not found.
  pdxs <- intersect(clin_icd, acc_pdx_vector)
  if (length(pdxs) == 0) {
    return(list(pdx = NA_character_, pdx_code = 99))
  } else if (length(pdxs) == 1) {
    return(list(pdx = pdxs[1], pdx_code = 3))
  }
  return(pdxs)
}

# Helper function to compute similarities between a code and candidates
compute_similarity <- function(code, candidates) {
  #' @title Compute Similarity
  #' @description Computes similarities between a code and a list of candidate codes.
  #' @param code A clinical code to compare.
  #' @param candidates A list of candidate codes to compare against.
  #' @return A numeric vector representing the similarity scores.
  sapply(candidates, function(candidate) {
    sum(substr(code, 1, nchar(candidate)) == substr(candidate, 1, nchar(candidate)))
  })
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
  if (length(starting_codes) == 1) {
    return(list(pdx = starting_codes[1], pdx_code = 4))
  } else if (length(starting_codes) > 1) {
    similarities <- compute_similarity(code, starting_codes)
    most_similar_pdx <- starting_codes[which.max(similarities)]
    return(list(pdx = most_similar_pdx, pdx_code = 5))
  }
  return(NULL)
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
  if (!is.null(pdx_check)) return(pdx_check)
  
  pdx_check <- assess_pdx_code(clin_c2, 2)
  if (!is.null(pdx_check)) return(pdx_check)
  
  # Find PDX from clinical ICD codes
  pdxs <- find_pdx_from_icd(clin_icd)
  if (is.list(pdxs)) return(pdxs)
  
  # Find the most similar PDX based on clin_c1 or clin_c2
  for (cr in list(clin_c1, clin_c2)) {
    if (!is.na(cr) && cr != "") {
      most_similar_pdx <- find_most_similar_pdx(cr, pdxs)
      if (!is.null(most_similar_pdx)) return(most_similar_pdx)
    }
  }
  
  # If no specific match, return a random PDX from the list
  if (length(pdxs) > 0) {
    return(list(pdx = sample(pdxs, 1), pdx_code = 6))
  }
  
  return(list(pdx = NA_character_, pdx_code = 99))
}

apply_find_pdx <- function(clin_c1, clin_c2, clin_icd, acc_pdx_vector = acc_pdx) {
  #' @title Apply Find PDX to Specific Columns
  #' @description Applies the find_pdx function to specific columns and returns PDX and PDX code vectors.
  #' @param clin_c1 A vector of the first clinical codes.
  #' @param clin_c2 A vector of the second clinical codes.
  #' @param clin_icd A list of clinical ICD codes.
  #' @param acc_pdx_vector A vector of acceptable PDX codes.
  #' @return A list containing the PDX vector and the PDX code vector.
  
  n <- length(clin_c1)
  pdx <- character(n)
  pdx_code <- integer(n)
  
  # Assign PDX based on clin_c1 and clin_c2
  pdx[clin_c1 %in% acc_pdx_vector] <- clin_c1[clin_c1 %in% acc_pdx_vector]
  pdx_code[clin_c1 %in% acc_pdx_vector] <- 1
  
  pdx[clin_c2 %in% acc_pdx_vector] <- clin_c2[clin_c2 %in% acc_pdx_vector]
  pdx_code[clin_c2 %in% acc_pdx_vector] <- 2
  
  # Identify rows without a PDX
  missing_pdx_indices <- which(pdx == "")
  
  if (length(missing_pdx_indices) > 0) {
    for (i in missing_pdx_indices) {
      result <- find_pdx(clin_c1[i], clin_c2[i], clin_icd[[i]])
      pdx[i] <- result$pdx
      pdx_code[i] <- result$pdx_code
    }
  }
  
  return(list(pdx = pdx, pdx_code = pdx_code))
}