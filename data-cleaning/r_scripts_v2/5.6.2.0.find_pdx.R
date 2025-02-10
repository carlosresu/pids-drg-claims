find_pdx <- function(
    c1_split, c2_split, clin_icd_split, # Inputs:
    seed # Parameters:
    ) {
  # Function to calculate similarity between two strings
  check_similarity <- function(x, y) {
    min_len <- min(nchar(x), nchar(y))
    sum(substr(x, 1, min_len) == substr(y, 1, min_len))
  }

  # Process each row, saving it so we run it only once, but extract
  # two things from it later.

  # REMEMBER: we already filter c1 thru clin_icd and leave only acceptable pdxs
  # this is why we dont check
  algo_result <- mapply(function(c1_split, c2_split, clin_icd_split) {
    # Step A: Check if any element in c1_split or c2_split is an accepted PDX
    for (cr_list in list(c1_split, c2_split)) {
      if (length(cr_list) > 0) {
        return(list(
          pdx = cr_list[1],
          pdx_code = ifelse(cr_list[1] %in% c1_split, 1, 2)
        ))
      }
    }

    # Step B: Find accepted PDX from clin_icd_split
    if (length(clin_icd_split) > 0) {
      pdxs <- clin_icd_split
    } else {
      return(list(
        pdx = NA_character_,
        pdx_code = 99
      ))
    }

    # Step C: Handle cases with only one accepted PDX
    if (length(pdxs) == 1) {
      return(list(
        pdx = pdxs[1],
        pdx_code = 3
      ))
    }

    # Step D: Check for matching starting letters in c1_split and c2_split
    for (cr_list in list(c1_split, c2_split)) {
      for (cr in cr_list) {
        starting_codes <- pdxs[substr(pdxs, 1, 1) == substr(cr, 1, 1)]
        if (length(starting_codes) == 1) {
          return(list(
            pdx = starting_codes[1],
            pdx_code = 4
          ))
        } else if (length(starting_codes) > 1) {
          best_match <- starting_codes[which.max(
            sapply(starting_codes, check_similarity, y = cr)
          )]
          return(list(
            pdx = best_match,
            pdx_code = 5
          ))
        }
      }
    }

    # Step E: Pick a random PDX if no match is found
    set.seed(seed)
    return(list(
      pdx = sample(pdxs, 1),
      pdx_code = 6
    ))
  }, c1_split, c2_split, clin_icd_split, SIMPLIFY = FALSE)

  # Return the PDX values and codes
  return(list(
    pdx = sapply(algo_result, `[[`, "pdx"),
    pdx_code = sapply(algo_result, `[[`, "pdx_code")
  ))
}
