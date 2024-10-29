# find_pdx <- function(c1, c2, clin_icd, accpdx = acc_pdx) {
#   # Step 1: Helper function to check similarity between two strings
#   check_similarity <- function(x, y) {
#     score <- 0
#     min_len <- min(nchar(x), nchar(y))
#     for (i in 1:min_len) {
#       if (substr(x, i, i) == substr(y, i, i)) {
#         score <- score + 1
#       }
#     }
#     return(score)
#   }

#   # Step 2: Apply the PDX finding logic row-wise
#   result <- mapply(function(c1, c2, clin_icd) {
#     # Split c1 and c2 by '|' if necessary
#     c1_split <- unlist(strsplit(c1, "\\|"))
#     c2_split <- unlist(strsplit(c2, "\\|"))

#     # Step 3: Check if any element in c1 or c2 is an accepted PDX
#     for (cr_list in list(c1_split, c2_split)) {
#       for (cr in cr_list) {
#         if (!is.na(cr) && cr %in% accpdx) {
#           return(list(pdx = cr, pdx_code = ifelse(cr %in% c1_split, 1, 2)))
#         }
#       }
#     }

#     # Step 4: Unlist clin_icd by splitting if necessary
#     clin_icd_split <- unlist(strsplit(clin_icd, "\\|"))

#     # Step 5: Get a list of accepted PDX from clin_icd
#     pdxs <- unique(clin_icd_split[clin_icd_split %in% accpdx])

#     # Step 6: Handle cases with no or only one accepted PDX
#     if (length(pdxs) == 0) {
#       return(list(pdx = NA_character_, pdx_code = 99))
#     } else if (length(pdxs) == 1) {
#       return(list(pdx = pdxs[1], pdx_code = 3))
#     }

#     # Step 7: Check c1 and c2 for matching starting letters
#     for (cr_list in list(c1_split, c2_split)) {
#       for (cr in cr_list) {
#         if (!is.na(cr)) {
#           starting_letter <- substr(cr, 1, 1)
#           starting_codes <- pdxs[substr(pdxs, 1, 1) == starting_letter]

#           if (length(starting_codes) == 1) {
#             return(list(pdx = starting_codes[1], pdx_code = 4))
#           } else if (length(starting_codes) > 1) {
#             # Sort by similarity score in descending order
#             starting_codes <- starting_codes[
#               order(
#                 sapply(starting_codes, function(x) check_similarity(cr, x)),
#                 decreasing = TRUE
#               )
#             ]
#             return(list(pdx = starting_codes[1], pdx_code = 5))
#           }
#         }
#       }
#     }

#     # Step 8: If no matching starting letter, pick a random PDX
#     if (length(pdxs) > 0) {
#       return(list(pdx = sample(pdxs, 1), pdx_code = 6))
#     }

#     # Step 9: Return NA and code 99 if no PDX is found
#     return(list(pdx = NA_character_, pdx_code = 99))
#   }, c1, c2, clin_icd, SIMPLIFY = FALSE)

#   # Step 10: Extract PDX and PDX codes into vectors
#   pdx <- sapply(result, function(x) x$pdx)
#   pdx_code <- sapply(result, function(x) x$pdx_code)

#   # Step 11: Return the PDX values and codes
#   return(list(pdx = pdx, pdx_code = pdx_code))
# }

find_pdx <- function(c1, c2, clin_icd, accpdx = acc_pdx) {
  # Step 1: Store accepted PDX codes in an environment for fast lookups
  accpdx_env <- new.env(hash = TRUE)
  list2env(setNames(as.list(rep(TRUE, length(accpdx))), accpdx), envir = accpdx_env)

  # Step 2: Helper function to check similarity between two strings
  check_similarity <- function(x, y) {
    min_len <- min(nchar(x), nchar(y))
    sum(substr(x, 1, min_len) == substr(y, 1, min_len))
  }

  # Step 3: Helper to batch-check if elements exist in the accpdx environment
  is_accpdx <- function(codes) {
    valid_codes <- codes[!is.na(codes) & codes != ""]
    exists_list <- mget(valid_codes, envir = accpdx_env, ifnotfound = list(NULL))
    as.logical(sapply(exists_list, Negate(is.null)))
  }

  # Step 4: Apply the PDX finding logic row-wise
  result <- mapply(function(c1, c2, clin_icd) {
    # Split c1, c2, and clin_icd by '|' if necessary
    c1_split <- unlist(strsplit(c1, "\\|"))
    c2_split <- unlist(strsplit(c2, "\\|"))
    clin_icd_split <- unlist(strsplit(clin_icd, "\\|"))

    # Step 5: Check if any element in c1 or c2 is an accepted PDX
    for (cr_list in list(c1_split, c2_split)) {
      valid_indices <- is_accpdx(cr_list)
      valid_pdx <- cr_list[valid_indices]
      if (length(valid_pdx) > 0) {
        return(list(pdx = valid_pdx[1], pdx_code = ifelse(valid_pdx[1] %in% c1_split, 1, 2)))
      }
    }

    # Step 6: Find accepted PDX from clin_icd_split
    valid_indices <- is_accpdx(clin_icd_split)
    pdxs <- clin_icd_split[valid_indices]

    # Step 7: Handle cases with no or only one accepted PDX
    if (length(pdxs) == 0) {
      return(list(pdx = NA_character_, pdx_code = 99))
    }
    if (length(pdxs) == 1) {
      return(list(pdx = pdxs[1], pdx_code = 3))
    }

    # Step 8: Check for matching starting letters in c1 and c2
    for (cr_list in list(c1_split, c2_split)) {
      for (cr in cr_list) {
        if (!is.na(cr)) {
          starting_letter <- substr(cr, 1, 1)
          starting_codes <- pdxs[substr(pdxs, 1, 1) == starting_letter]

          if (length(starting_codes) == 1) {
            return(list(pdx = starting_codes[1], pdx_code = 4))
          } else if (length(starting_codes) > 1) {
            # Sort by similarity score
            best_match <- starting_codes[
              which.max(sapply(starting_codes, check_similarity, y = cr))
            ]
            return(list(pdx = best_match, pdx_code = 5))
          }
        }
      }
    }

    # Step 9: Pick a random PDX if no match is found
    if (length(pdxs) > 0) {
      return(list(pdx = sample(pdxs, 1), pdx_code = 6))
    }

    # Step 10: Default case with no PDX found
    return(list(pdx = NA_character_, pdx_code = 99))
  }, c1, c2, clin_icd, SIMPLIFY = FALSE)

  # Step 11: Extract PDX and PDX codes into vectors
  pdx <- sapply(result, `[[`, "pdx")
  pdx_code <- sapply(result, `[[`, "pdx_code")

  # Step 12: Return the PDX values and codes
  return(list(pdx = pdx, pdx_code = pdx_code))
}
