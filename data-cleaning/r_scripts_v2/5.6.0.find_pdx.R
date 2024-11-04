# find_pdx <- function(c1, c2, clin_icd, accpdx = acc_pdx) {
#   # Step 1: Store accepted PDX codes in an environment for fast lookups
#   accpdx_env <- new.env(hash = TRUE)
#   list2env(setNames(as.list(rep(TRUE, length(accpdx))), accpdx), envir = accpdx_env)

#   # Step 2: Helper function to check similarity between two strings
#   check_similarity <- function(x, y) {
#     min_len <- min(nchar(x), nchar(y))
#     sum(substr(x, 1, min_len) == substr(y, 1, min_len))
#   }

#   # Step 3: Helper to batch-check if elements exist in the accpdx environment
#   is_accpdx <- function(codes) {
#     valid_codes <- codes[!is.na(codes) & codes != ""]
#     exists_list <- mget(valid_codes, envir = accpdx_env, ifnotfound = list(NULL))
#     as.logical(sapply(exists_list, Negate(is.null)))
#   }

#   # Step 4: Apply the PDX finding logic row-wise
#   result <- mapply(function(c1, c2, clin_icd) {
#     # Split c1, c2, and clin_icd by '|' if necessary
#     c1_split <- unlist(strsplit(c1, "\\|"))
#     c2_split <- unlist(strsplit(c2, "\\|"))
#     clin_icd_split <- unlist(strsplit(clin_icd, "\\|"))

#     # Step 5: Check if any element in c1 or c2 is an accepted PDX
#     for (cr_list in list(c1_split, c2_split)) {
#       valid_indices <- is_accpdx(cr_list)
#       valid_pdx <- cr_list[valid_indices]
#       if (length(valid_pdx) > 0) {
#         return(list(pdx = valid_pdx[1], pdx_code = ifelse(valid_pdx[1] %in% c1_split, 1, 2)))
#       }
#     }

#     # Step 6: Find accepted PDX from clin_icd_split
#     valid_indices <- is_accpdx(clin_icd_split)
#     pdxs <- clin_icd_split[valid_indices]

#     # Step 7: Handle cases with no or only one accepted PDX
#     if (length(pdxs) == 0) {
#       return(list(pdx = NA_character_, pdx_code = 99))
#     }
#     if (length(pdxs) == 1) {
#       return(list(pdx = pdxs[1], pdx_code = 3))
#     }

#     # Step 8: Check for matching starting letters in c1 and c2
#     for (cr_list in list(c1_split, c2_split)) {
#       for (cr in cr_list) {
#         if (!is.na(cr)) {
#           starting_letter <- substr(cr, 1, 1)
#           starting_codes <- pdxs[substr(pdxs, 1, 1) == starting_letter]

#           if (length(starting_codes) == 1) {
#             return(list(pdx = starting_codes[1], pdx_code = 4))
#           } else if (length(starting_codes) > 1) {
#             # Sort by similarity score
#             best_match <- starting_codes[
#               which.max(sapply(starting_codes, check_similarity, y = cr))
#             ]
#             return(list(pdx = best_match, pdx_code = 5))
#           }
#         }
#       }
#     }

#     # Step 9: Pick a random PDX if no match is found
#     if (length(pdxs) > 0) {
#       return(list(pdx = sample(pdxs, 1), pdx_code = 6))
#     }

#     # Step 10: Default case with no PDX found
#     return(list(pdx = NA_character_, pdx_code = 99))
#   }, c1, c2, clin_icd, SIMPLIFY = FALSE)

#   # Step 11: Extract PDX and PDX codes into vectors
#   pdx <- sapply(result, `[[`, "pdx")
#   pdx_code <- sapply(result, `[[`, "pdx_code")

#   # Step 12: Return the PDX values and codes
#   return(list(pdx = pdx, pdx_code = pdx_code))
# }
find_pdx <- function(c1, c2, clin_icd, accpdx = acc_pdx,
                     neoplasmsdtactual = neoplasms_dt_actual,
                     acrrvs = acr_rvs, covidrvs = covid_rvs) {
  # Step 1: Define sets for filtering and accepted PDX codes
  acc_pdx_set <- unique(accpdx)
  neoplasm_codes <- unique(neoplasmsdtactual$icd10)
  rvs_codes <- unique(acrrvs$rvs)

  # Step 2: Helper function to check similarity between two strings
  check_similarity <- function(x, y) {
    min_len <- min(nchar(x), nchar(y))
    sum(substr(x, 1, min_len) == substr(y, 1, min_len))
  }

  # Step 3: Filter ICD codes based on exclusion criteria
  filter_icds <- function(codes) {
    codes <- codes[!is.na(codes) & !grepl("^[0-9]", codes) &
      !grepl("^[A-Z]{2}", codes) & !grepl("/", codes) &
      !(codes %in% neoplasm_codes) & !(codes %in% rvs_codes) &
      !(codes %in% covidrvs)]
    codes[codes %in% acc_pdx_set]
  }

  # Step 4: Apply the PDX finding logic row-wise
  result <- mapply(function(c1, c2, clin_icd) {
    # Split c1, c2, and clin_icd by '|' if necessary
    c1_split <- filter_icds(unlist(strsplit(c1, "\\|")))
    c2_split <- filter_icds(unlist(strsplit(c2, "\\|")))
    clin_icd_split <- filter_icds(unlist(strsplit(clin_icd, "\\|")))

    # Step 5: Check if any element in c1 or c2 is an accepted PDX
    for (cr_list in list(c1_split, c2_split)) {
      if (length(cr_list) > 0) {
        return(list(pdx = cr_list[1], pdx_code = ifelse(cr_list[1] %in% c1_split, 1, 2)))
      }
    }

    # Step 6: Find accepted PDX from clin_icd_split
    if (length(clin_icd_split) > 0) {
      pdxs <- clin_icd_split
    } else {
      return(list(pdx = NA_character_, pdx_code = 99))
    }

    # Step 7: Handle cases with only one accepted PDX
    if (length(pdxs) == 1) {
      return(list(pdx = pdxs[1], pdx_code = 3))
    }

    # Step 8: Check for matching starting letters in c1 and c2
    for (cr_list in list(c1_split, c2_split)) {
      for (cr in cr_list) {
        starting_codes <- pdxs[substr(pdxs, 1, 1) == substr(cr, 1, 1)]
        if (length(starting_codes) == 1) {
          return(list(pdx = starting_codes[1], pdx_code = 4))
        } else if (length(starting_codes) > 1) {
          best_match <- starting_codes[which.max(sapply(starting_codes, check_similarity, y = cr))]
          return(list(pdx = best_match, pdx_code = 5))
        }
      }
    }

    # Step 9: Pick a random PDX if no match is found
    list(pdx = sample(pdxs, 1), pdx_code = 6)
  }, c1, c2, clin_icd, SIMPLIFY = FALSE)

  # Step 10: Extract PDX and PDX codes into vectors
  pdx <- sapply(result, `[[`, "pdx")
  pdx_code <- sapply(result, `[[`, "pdx_code")

  # Step 11: Return the PDX values and codes
  list(pdx = pdx, pdx_code = pdx_code)
}
