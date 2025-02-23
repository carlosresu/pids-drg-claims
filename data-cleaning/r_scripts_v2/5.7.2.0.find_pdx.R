find_pdx <- function(
    # Inputs:
    c1_split, c2_split, clin_sdx_split,
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

  # REMEMBER: we already filter c1 thru clin_sdx and leave only acceptable pdxs
  # this is why we dont check
  algo_result <- mapply(function(c1_split, c2_split, clin_sdx_split) {
    # Step A: Check if any element in c1_split or c2_split is an accepted clin_pdx
    for (cr_list in list(c1_split, c2_split)) {
      if (length(cr_list) > 0) {
        ret_list <- list(
          clin_pdx = cr_list[1],
          clin_pdx_source = ifelse(cr_list[1] %in% c1_split, 1, 2)
        )
        return(ret_list)
      }
    }

    # Step B: Find accepted clin_pdx from clin_sdx_split
    if (length(clin_sdx_split) > 0) {
      pdxs <- clin_sdx_split
    } else {
      ret_list <- list(
        clin_pdx = NA_character_,
        clin_pdx_source = 99
      )
      return(ret_list)
    }

    # Step C: Handle cases with only one accepted clin_pdx
    if (length(pdxs) == 1) {
      ret_list <- list(
        clin_pdx = pdxs[1],
        clin_pdx_source = 3
      )
      return(ret_list)
    }

    # Step D: Check for matching starting letters in c1_split and c2_split
    for (cr_list in list(c1_split, c2_split)) {
      for (cr in cr_list) {
        starting_codes <- pdxs[substr(pdxs, 1, 1) == substr(cr, 1, 1)]
        if (length(starting_codes) == 1) {
          ret_list <- list(
            clin_pdx = starting_codes[1],
            clin_pdx_source = 4
          )
          return(ret_list)
        } else if (length(starting_codes) > 1) {
          best_match <- starting_codes[which.max(
            sapply(starting_codes, check_similarity, y = cr)
          )]
          ret_list <- list(
            clin_pdx = best_match,
            clin_pdx_source = 5
          )
          return(ret_list)
        }
      }
    }

    # Step E: Pick a random clin_pdx if no match is found
    set.seed(seed)
    ret_list <- list(
      clin_pdx = sample(pdxs, 1),
      clin_pdx_source = 6
    )
    return(ret_list)
  }, c1_split, c2_split, clin_sdx_split, SIMPLIFY = FALSE)

  # Return the clin_pdx values and codes
  ret_list <- list(
    clin_pdx = sapply(algo_result, `[[`, "clin_pdx"),
    clin_pdx_source = sapply(algo_result, `[[`, "clin_pdx_source")
  )
  return(ret_list)
}

# find_pdx <- function(
#     # Inputs:
#     c1_split, c2_split, clin_sdx_split,
#     # Parameters:
#     seed, accpdx = acc_pdx,
#     accpdxenv = acc_pdx_env) {
#   # Function to calculate similarity between two strings
#   check_similarity <- function(x, y) {
#     min_len <- min(nchar(x), nchar(y))
#     ret_flag <- sum(substr(x, 1, min_len) == substr(y, 1, min_len))
#     return(ret_flag)
#   }

#   # Process each row, saving it so we run it only once, but extract
#   # two things from it later.

#   # REMEMBER: we already filter c1 thru clin_sdx and leave only acceptable pdxs
#   # this is why we dont check
#   algo_result <- mapply(function(c1_split, c2_split, clin_sdx_split) {
#     # Step A: Check if any element in c1_split or c2_split is an accepted clin_pdx
#     for (cr_list in list(c1_split, c2_split)) {
#       if (length(cr_list) > 0) {
#         ret_list <- list(
#           clin_pdx = cr_list[1],
#           clin_pdx_source = ifelse(cr_list[1] %in% c1_split, 1, 2)
#         )
#         return(ret_list)
#       }
#     }

#     # Step B: Find accepted clin_pdx from clin_sdx_split
#     if (length(clin_sdx_split) > 0) {
#       pdxs <- clin_sdx_split
#     } else {
#       ret_list <- list(
#         clin_pdx = NA_character_,
#         clin_pdx_source = 99
#       )
#       return(ret_list)
#     }

#     # Step C: Handle cases with only one accepted clin_pdx
#     if (length(pdxs) == 1) {
#       ret_list <- list(
#         clin_pdx = pdxs[1],
#         clin_pdx_source = 3
#       )
#       return(ret_list)
#     }

#     # Step D: Check for matching starting letters in c1_split and c2_split
#     for (cr_list in list(c1_split, c2_split)) {
#       for (cr in cr_list) {
#         starting_codes <- pdxs[substr(pdxs, 1, 1) == substr(cr, 1, 1)]
#         if (length(starting_codes) == 1) {
#           ret_list <- list(
#             clin_pdx = starting_codes[1],
#             clin_pdx_source = 4
#           )
#           return(ret_list)
#         } else if (length(starting_codes) > 1) {
#           best_match <- starting_codes[which.max(
#             sapply(starting_codes, check_similarity, y = cr)
#           )]
#           ret_list <- list(
#             clin_pdx = best_match,
#             clin_pdx_source = 5
#           )
#           return(ret_list)
#         }
#       }
#     }

#     # Step E: Pick a random clin_pdx if no match is found
#     set.seed(seed)
#     ret_list <- list(
#       clin_pdx = sample(pdxs, 1),
#       clin_pdx_source = 6
#     )
#     return(ret_list)
#   }, c1_split, c2_split, clin_sdx_split, SIMPLIFY = FALSE)

#   # Extract results
#   clin_pdx_values <- sapply(algo_result, `[[`, "clin_pdx")
#   clin_pdx_sources <- sapply(algo_result, `[[`, "clin_pdx_source")

#   # Ensure all returned codes are in acc_pdx (set) or acc_pdx_env (environment)
#   valid_codes <- unique(accpdx)
#   if (!is.null(accpdxenv)) {
#     valid_codes <- unique(c(valid_codes, ls(accpdxenv)))
#   }

#   # Identify and log invalid codes
#   invalid_indices <- !(clin_pdx_values %in% valid_codes)

#   # Replace invalid codes with NA_character_ and set clin_pdx_source to 99
#   clin_pdx_values[invalid_indices] <- NA_character_
#   clin_pdx_sources[invalid_indices] <- 99

#   # Return updated result along with replaced codes
#   ret_list <- list(
#     clin_pdx = clin_pdx_values,
#     clin_pdx_source = clin_pdx_sources
#   )
#   return(ret_list)
# }
