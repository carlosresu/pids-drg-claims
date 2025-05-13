# Revised main function to impute the primary diagnosis (pdx)
# based on provided ICD-10 codes.
find_pdx <- function(c1_arg, c2_arg, clin_sdx_arg, seed, accpdx = acc_pdx) {
  # Step 0: Build a unique set of accepted ICD codes
  # (e.g., valid PDX candidates)
  acc_pdx_set_final <- unique(accpdx)

  # Helper function to keep only valid ICD codes that are present in
  # the accepted list
  filter_icds <- function(codes, acc_pdx_set) {
    if (is.null(codes)) {
      return(character(0))
    }
    codes[!is.na(codes) & codes %chin% acc_pdx_set]
  }

  # Helper function to remove NA values and return NULL if the result is empty
  clean_codes <- function(codes) {
    if (is.null(codes) || length(codes) == 0) {
      return(NULL)
    }
    cleaned <- codes[!is.na(codes)]
    if (length(cleaned) == 0) NULL else cleaned
  }

  # Helper function to measure character-level prefix similarity
  # between two codes
  check_similarity <- function(x, y) {
    chars_x <- strsplit(x, "")[[1]]
    chars_y <- strsplit(y, "")[[1]]
    min_len <- min(length(chars_x), length(chars_y))
    sum(chars_x[1:min_len] == chars_y[1:min_len])
  }

  # Step 1: Apply filtering to input ICD lists using the accepted ICD list
  c1_filtered <- lapply(c1_arg, filter_icds, acc_pdx_set_final)
  c2_filtered <- lapply(c2_arg, filter_icds, acc_pdx_set_final)
  clin_sdx_filtered <- lapply(clin_sdx_arg, filter_icds, acc_pdx_set_final)

  # Step 2: Also retain original (cleaned) versions of C1 and C2
  # for similarity logic
  c1_original <- lapply(c1_arg, clean_codes)
  c2_original <- lapply(c2_arg, clean_codes)

  # Step 3: Apply rules row-wise to determine the best PDX candidate
  algo_result <- mapply(
    function(c1f, c2f, sdxf, c1o, c2o) {
      # Rule 1: Use first accepted code from C1 if available
      if (!is.null(c1f) && length(c1f) > 0) {
        return(list(clin_pdx = c1f[1], clin_pdx_source = 1))
      }

      # Rule 2: Use first accepted code from C2 if C1 is empty
      if (!is.null(c2f) && length(c2f) > 0) {
        return(list(clin_pdx = c2f[1], clin_pdx_source = 2))
      }

      # Rule 99: No eligible SDx candidates → return NA
      if (is.null(sdxf) || length(sdxf) == 0) {
        return(list(clin_pdx = NA_character_, clin_pdx_source = 99))
      }

      # Step 4: Build a candidate pool from filtered SDx codes
      candidates <- sdxf

      # Step 5: Reference codes for similarity (first entries in raw C1/C2)
      ref1 <- if (!is.null(c1o) && length(c1o) > 0) c1o[1] else ""
      ref2 <- if (!is.null(c2o) && length(c2o) > 0) c2o[1] else ""

      # Rule 3/4: Attempt similarity match using CR1
      if (ref1 != "") {
        matching_cr1 <- candidates[
          substr(candidates, 1, 1) == substr(ref1, 1, 1)
        ]
        if (length(matching_cr1) > 0) {
          sim1 <- sapply(matching_cr1, function(x) check_similarity(ref1, x))
          if (max(sim1) > 0) {
            best_cr1 <- matching_cr1[sim1 == max(sim1)]
            if (length(best_cr1) == 1) {
              return(list(clin_pdx = best_cr1[1], clin_pdx_source = 3))
            } else {
              set.seed(seed)
              chosen <- sample(best_cr1, 1) # Tie-breaker
              return(list(clin_pdx = chosen, clin_pdx_source = 4))
            }
          }
        }
      }

      # Rule 5/6: Attempt similarity match using CR2
      if (ref2 != "") {
        matching_cr2 <- candidates[
          substr(candidates, 1, 1) == substr(ref2, 1, 1)
        ]
        if (length(matching_cr2) > 0) {
          sim2 <- sapply(matching_cr2, function(x) check_similarity(ref2, x))
          if (max(sim2) > 0) {
            best_cr2 <- matching_cr2[sim2 == max(sim2)]
            if (length(best_cr2) == 1) {
              return(list(clin_pdx = best_cr2[1], clin_pdx_source = 5))
            } else {
              set.seed(seed)
              chosen <- sample(best_cr2, 1) # Tie-breaker
              return(list(clin_pdx = chosen, clin_pdx_source = 6))
            }
          }
        }
      }

      # Rule 7: Only one eligible SDx candidate → use it
      if (length(candidates) == 1) {
        return(list(clin_pdx = candidates[1], clin_pdx_source = 7))
      } else {
        # Rule 8: Multiple SDx candidates but no similarity → pick randomly
        set.seed(seed)
        chosen <- sample(candidates, 1)
        return(list(clin_pdx = chosen, clin_pdx_source = 8))
      }
    }, c1_filtered, c2_filtered, clin_sdx_filtered, c1_original,
    c2_original,
    SIMPLIFY = FALSE
  )

  # Step 4: Return the list as two named output vectors
  return(list(
    clin_pdx = sapply(algo_result, `[[`, "clin_pdx"),
    clin_pdx_source = sapply(algo_result, `[[`, "clin_pdx_source")
  ))
}
