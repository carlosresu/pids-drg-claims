# Function to find the primary diagnosis (PDX) based on the provided logic
find_pdx <- function(clin_c1, clin_c2, clin_icd, acc_pdx_env) {
  check_similarity <- function(x, y) {
    score <- 0
    min_len <- min(nchar(x), nchar(y))
    for (i in 1:min_len) {
      if (substr(x, i, i) == substr(y, i, i)) {
        score <- score + 1
      }
    }
    return(score)
  }

  clin_icd <- unlist(clin_icd)

  # Get a list of all SDx that may be chosen as PDx
  pdxs <- unique(clin_icd)
  pdxs <- pdxs[sapply(pdxs, function(x) exists(x, acc_pdx_env))]

  # For those with no acceptable PDx or only 1 acceptable PDx
  if (length(pdxs) == 0) {
    return(list(pdx = NA_character_, pdx_code = 99))
  } else if (length(pdxs) == 1) {
    return(list(pdx = pdxs[1], pdx_code = 3))
  }

  # If there are multiple eligible PDx,
  # see if any are related to the starting letters
  for (cr in c(clin_c1, clin_c2)) {
    if (!is.na(cr)) {
      if (exists(cr, acc_pdx_env)) { # If clin_c* is a valid ICD-10
        # Get starting letter of clin_c*
        starting_letter <- substr(cr, 1, 1)
        # List all valid ICD-10 codes with same starting letter
        starting_codes <- pdxs[substr(pdxs, 1, 1) == starting_letter]
        # If there's only one similar eligible PDx, choose that
        if (length(starting_codes) == 1) {
          return(list(pdx = starting_codes[1], pdx_code = 4))
        }
        # If there are multiple similar eligible PDx
        if (length(starting_codes) > 1) {
          # Obtain the one that most resembles the case rate
          starting_codes <- starting_codes[
            order(sapply(starting_codes, function(x) check_similarity(cr, x)), decreasing = TRUE)
          ]
          return(list(pdx = starting_codes[1], pdx_code = 5))
        }
      }
    }
  }

  # If there is no related starting letter, choose randomly
  if (length(pdxs) > 0) {
    return(list(pdx = sample(pdxs, 1), pdx_code = 6))
  }

  return(list(pdx = NA_character_, pdx_code = 99))
}

apply_find_pdx <- function(clin_c1, clin_c2, clin_icd, acc_pdx) {
  acc_pdx_env <- new.env(hash = TRUE, parent = emptyenv())
  for (code in acc_pdx) {
    assign(code, TRUE, envir = acc_pdx_env)
  }

  dt <- data.table(
    clin_c1 = clin_c1,
    clin_c2 = clin_c2,
    clin_icd = clin_icd
  )

  # Vectorized application of find_pdx function
  find_pdx_vectorized <- function(clin_c1, clin_c2, clin_icd) {
    # Convert lists to characters for easy handling
    clin_c1_char <- sapply(clin_c1, function(x) if (is.null(x)) NA_character_ else x)
    clin_c2_char <- sapply(clin_c2, function(x) if (is.null(x)) NA_character_ else x)
    clin_icd_char <- sapply(clin_icd, function(x) paste(x, collapse = ","))

    # Initialize result vectors
    pdx <- rep(NA_character_, length(clin_c1))
    pdx_code <- rep(NA_integer_, length(clin_c1))

    # Batch check clin_c1 and clin_c2
    clin_c1_check <- sapply(clin_c1_char, function(x) exists(x, acc_pdx_env))
    clin_c2_check <- sapply(clin_c2_char, function(x) exists(x, acc_pdx_env))

    pdx[clin_c1_check] <- clin_c1_char[clin_c1_check]
    pdx_code[clin_c1_check] <- 1

    clin_c2_only_check <- !clin_c1_check & clin_c2_check
    pdx[clin_c2_only_check] <- clin_c2_char[clin_c2_only_check]
    pdx_code[clin_c2_only_check] <- 2

    # Apply find_pdx function to remaining rows
    remaining_indices <- which(is.na(pdx))
    for (i in remaining_indices) {
      result <- find_pdx(clin_c1_char[i], clin_c2_char[i], clin_icd_char[i], acc_pdx_env)
      pdx[i] <- result$pdx
      pdx_code[i] <- result$pdx_code
    }

    return(list(pdx = pdx, pdx_code = pdx_code))
  }

  pdx_results <- find_pdx_vectorized(dt$clin_c1, dt$clin_c2, dt$clin_icd)
  dt[, pdx := pdx_results$pdx]
  dt[, pdx_code := pdx_results$pdx_code]

  return(list(pdx = dt$pdx, pdx_code = dt$pdx_code))
}
