find_pdx <- function(c1, c2, clin_icd, accpdx = acc_pdx) {
  # Step 1: Create a new environment for accepted PDX codes
  acc_pdx_env <- new.env(hash = TRUE, parent = emptyenv())

  # Step 2: Populate the environment with accepted PDX codes
  for (code in accpdx) {
    assign(code, TRUE, envir = acc_pdx_env)
  }

  # Step 3: Helper logic to check similarity between two strings
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

  # Step 4: Apply the PDX finding logic row-wise
  result <- mapply(function(c1, c2, clin_icd) {
    # Split c1 and c2 by '|' if necessary
    c1_split <- unlist(strsplit(c1, "\\|"))
    c2_split <- unlist(strsplit(c2, "\\|"))

    # Step 5: Check if any element in c1 or c2 is an acceptable PDx
    for (cr_list in list(c1_split, c2_split)) {
      for (cr in cr_list) {
        if (!is.na(cr) && exists(cr, envir = acc_pdx_env)) {
          return(list(pdx = cr, pdx_code = ifelse(cr %in% c1_split, 1, 2)))
        }
      }
    }

    # Step 6: Unlist clin_icd by splitting if necessary
    clin_icd_split <- unlist(strsplit(clin_icd, "\\|"))

    # Step 7: Get a list of acceptable PDx from clin_icd
    pdxs <- unique(clin_icd_split)
    pdxs <- pdxs[sapply(pdxs, function(x) exists(x, envir = acc_pdx_env))]

    # Step 8: Handle cases with no or only one acceptable PDx
    if (length(pdxs) == 0) {
      return(list(pdx = NA_character_, pdx_code = 99))
    } else if (length(pdxs) == 1) {
      return(list(pdx = pdxs[1], pdx_code = 3))
    }

    # Step 9: Check c1 and c2 for matching starting letters
    for (cr_list in list(c1_split, c2_split)) {
      for (cr in cr_list) {
        if (!is.na(cr)) {
          starting_letter <- substr(cr, 1, 1)
          starting_codes <- pdxs[substr(pdxs, 1, 1) == starting_letter]

          if (length(starting_codes) == 1) {
            return(list(pdx = starting_codes[1], pdx_code = 4))
          } else if (length(starting_codes) > 1) {
            # Sort by similarity score in descending order
            starting_codes <- starting_codes[
              order(
                sapply(
                  starting_codes,
                  function(x) check_similarity(cr, x)
                ),
                decreasing = TRUE
              )
            ]
            return(list(pdx = starting_codes[1], pdx_code = 5))
          }
        }
      }
    }

    # Step 10: If no matching starting letter, pick a random PDx
    if (length(pdxs) > 0) {
      return(list(pdx = sample(pdxs, 1), pdx_code = 6))
    }

    # Step 11: Return NA and code 99 if no PDx is found
    return(list(pdx = NA_character_, pdx_code = 99))
  }, c1, c2, clin_icd, SIMPLIFY = FALSE)

  # Step 12: Extract PDX and PDX codes into vectors
  pdx <- sapply(result, function(x) x$pdx)
  pdx_code <- sapply(result, function(x) x$pdx_code)

  # Return the PDX values and codes
  return(list(pdx = pdx, pdx_code = pdx_code))
}
