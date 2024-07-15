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

apply_find_pdx <- function(dt, acc_pdx_vector = acc_pdx) {
  #' @title Apply Find PDX to Data Table
  #' @description Applies the find_pdx function to each row of a data.table.
  #' @param dt A data.table to process.
  #' @param acc_pdx_vector A vector of acceptable PDX codes.
  #' @return The modified data.table with PDX information added.
  #'
  #' @details
  #' This function processes each row in the data.table to identify the primary diagnosis (PDX).
  #' It first attempts to assign PDX based on clin_c1 and clin_c2 columns.
  #' If no PDX is found, it uses the find_pdx function to determine the PDX from the clin_icd column.
  #'
  
  # Assign PDX based on clin_c1 and clin_c2
  dt[, pdx := ifelse(clin_c1 %in% acc_pdx_vector, clin_c1, ifelse(clin_c2 %in% acc_pdx_vector, clin_c2, NA))]
  dt[, pdx_code := ifelse(clin_c1 %in% acc_pdx_vector, 1, ifelse(clin_c2 %in% acc_pdx_vector, 2, 99))]
  
  # Identify rows without a PDX
  missing_pdx_indices <- which(is.na(dt$pdx))
  
  if (length(missing_pdx_indices) > 0) {
    clin_icd_list <- dt$clin_icd[missing_pdx_indices]
    
    result_list <- lapply(clin_icd_list, function(icd_list) {
      find_pdx(NA, NA, icd_list)
    })
    
    # Update dt with results
    dt$pdx[missing_pdx_indices] <- sapply(result_list, `[[`, "pdx")
    dt$pdx_code[missing_pdx_indices] <- sapply(result_list, `[[`, "pdx_code")
  }
  
  return(dt)
}