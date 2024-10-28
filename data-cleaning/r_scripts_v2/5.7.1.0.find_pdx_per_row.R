# Define helper function to find PDX for each row
find_pdx_for_row <- function(c1, c2, clin_icd) {
  # Split c1 and c2 by '|' if necessary
  c1 <- unlist(strsplit(c1, "\\|"))
  c2 <- unlist(strsplit(c2, "\\|"))

  # Step 1: Check if any element in c1 or c2 is an acceptable PDx
  for (cr_list in list(c1, c2)) {
    for (cr in cr_list) {
      if (!is.na(cr) && exists(cr, envir = acc_pdx_env)) {
        return(list(pdx = cr, pdx_code = ifelse(cr %in% c1, 1, 2)))
      }
    }
  }

  # Step 2: Unlist clin_icd by splitting if necessary
  clin_icd <- unlist(strsplit(clin_icd, "\\|"))

  # Step 3: Get a list of acceptable PDx from clin_icd
  pdxs <- unique(clin_icd)
  pdxs <- pdxs[sapply(pdxs, function(x) exists(x, envir = acc_pdx_env))]

  # Step 4: Handle cases with no or only one acceptable PDx
  if (length(pdxs) == 0) {
    return(list(pdx = NA_character_, pdx_code = 99))
  } else if (length(pdxs) == 1) {
    return(list(pdx = pdxs[1], pdx_code = 3))
  }

  # Step 5: Check c1 and c2 for matching starting letters
  for (cr_list in list(c1, c2)) {
    for (cr in cr_list) {
      if (!is.na(cr)) {
        starting_letter <- substr(cr, 1, 1)
        starting_codes <- pdxs[substr(pdxs, 1, 1) == starting_letter]

        if (length(starting_codes) == 1) {
          return(list(pdx = starting_codes[1], pdx_code = 4))
        } else if (length(starting_codes) > 1) {
          starting_codes <- starting_codes[
            order(sapply(
              starting_codes,
              function(x) check_similarity(cr, x)
            ), decreasing = TRUE)
          ]
          return(list(pdx = starting_codes[1], pdx_code = 5))
        }
      }
    }
  }

  # Step 6: If no matching starting letter, pick a random PDx
  if (length(pdxs) > 0) {
    return(list(pdx = sample(pdxs, 1), pdx_code = 6))
  }

  # Step 7: Return NA and code 99 if no PDx is found
  return(list(pdx = NA_character_, pdx_code = 99))
}
