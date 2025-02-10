find_pdx <- function(
    # Inputs:
    c1_split, c2_split, clin_icd_split,
    # Parameters:
    seed) {
  # Function to calculate similarity between two strings
  check_similarity <- function(x, y) {
    min_len <- min(nchar(x), nchar(y))
    ret_flag <- sum(substr(x, 1, min_len) == substr(y, 1, min_len))
    return(ret_flag)
  }

  # Process each row, saving it so we run it only once, but extract
  # two things from it later.

  # REMEMBER: we already filter c1 thru clin_icd and leave only acceptable pdxs
  # this is why we dont check
  algo_result <- mapply(function(c1_split, c2_split, clin_icd_split) {
    # Step A: Check if any element in c1_split or c2_split is an accepted PDX
    for (cr_list in list(c1_split, c2_split)) {
      if (length(cr_list) > 0) {
        ret_list <- list(
          pdx = cr_list[1],
          pdx_code = ifelse(cr_list[1] %in% c1_split, 1, 2)
        )
        return(ret_list)
      }
    }

    # Step B: Find accepted PDX from clin_icd_split
    if (length(clin_icd_split) > 0) {
      pdxs <- clin_icd_split
    } else {
      ret_list <- list(
        pdx = NA_character_,
        pdx_code = 99
      )
      return(ret_list)
    }

    # Step C: Handle cases with only one accepted PDX
    if (length(pdxs) == 1) {
      ret_list <- list(
        pdx = pdxs[1],
        pdx_code = 3
      )
      return(ret_list)
    }

    # Step D: Check for matching starting letters in c1_split and c2_split
    for (cr_list in list(c1_split, c2_split)) {
      for (cr in cr_list) {
        starting_codes <- pdxs[substr(pdxs, 1, 1) == substr(cr, 1, 1)]
        if (length(starting_codes) == 1) {
          ret_list <- list(
            pdx = starting_codes[1],
            pdx_code = 4
          )
          return(ret_list)
        } else if (length(starting_codes) > 1) {
          best_match <- starting_codes[which.max(
            sapply(starting_codes, check_similarity, y = cr)
          )]
          ret_list <- list(
            pdx = best_match,
            pdx_code = 5
          )
          return(ret_list)
        }
      }
    }

    # Step E: Pick a random PDX if no match is found
    set.seed(seed)
    ret_list <- list(
      pdx = sample(pdxs, 1),
      pdx_code = 6
    )
    return(ret_list)
  }, c1_split, c2_split, clin_icd_split, SIMPLIFY = FALSE)

  # Return the PDX values and codes
  ret_list <- list(
    pdx = sapply(algo_result, `[[`, "pdx"),
    pdx_code = sapply(algo_result, `[[`, "pdx_code")
  )
  return(ret_list)
}
