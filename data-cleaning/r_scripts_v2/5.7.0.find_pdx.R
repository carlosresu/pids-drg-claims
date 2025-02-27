# Revised main function to impute the primary diagnosis (pdx) based on provided ICD-10 codes.
find_pdx <- function(c1_arg, c2_arg, clin_sdx_arg, seed, accpdx = acc_pdx) {
  # Create a unique set of accepted ICD codes.
  acc_pdx_set_final <- unique(accpdx)

  # Function to filter ICD codes: keep only non-NA codes that are in the accepted set.
  filter_icds <- function(codes, acc_pdx_set) {
    if (is.null(codes)) {
      return(character(0))
    }
    codes[!is.na(codes) & codes %chin% acc_pdx_set]
  }

  # Helper function to "clean" the original codes by removing NAs.
  clean_codes <- function(codes) {
    if (is.null(codes) || length(codes) == 0) {
      return(NULL)
    }
    cleaned <- codes[!is.na(codes)]
    if (length(cleaned) == 0) NULL else cleaned
  }

  # Helper function to compute similarity between two strings.
  # It splits each string into characters and counts how many starting characters match.
  check_similarity <- function(x, y) {
    chars_x <- strsplit(x, "")[[1]]
    chars_y <- strsplit(y, "")[[1]]
    min_len <- min(length(chars_x), length(chars_y))
    sum(chars_x[1:min_len] == chars_y[1:min_len])
  }

  # Filter inputs to include only accepted ICD codes.
  c1_filtered <- lapply(c1_arg, filter_icds, acc_pdx_set_final)
  c2_filtered <- lapply(c2_arg, filter_icds, acc_pdx_set_final)
  clin_sdx_filtered <- lapply(clin_sdx_arg, filter_icds, acc_pdx_set_final)

  # Keep cleaned copies of the original c1 and c2 for similarity comparisons.
  c1_original <- lapply(c1_arg, clean_codes)
  c2_original <- lapply(c2_arg, clean_codes)

  # Process each row using mapply.
  algo_result <- mapply(function(c1f, c2f, sdxf, c1o, c2o) {
    ## Rule 1: If CR1 (c1 filtered) is available, return its first code.
    if (!is.null(c1f) && length(c1f) > 0) {
      return(list(clin_pdx = c1f[1], clin_pdx_source = 1))
    }

    ## Rule 2: Otherwise, if CR2 (c2 filtered) is available, return its first code.
    if (!is.null(c2f) && length(c2f) > 0) {
      return(list(clin_pdx = c2f[1], clin_pdx_source = 2))
    }

    # If no eligible SDx codes exist, then Rule 99.
    if (is.null(sdxf) || length(sdxf) == 0) {
      return(list(clin_pdx = NA_character_, clin_pdx_source = 99))
    }

    # Set all eligible SDx candidates.
    candidates <- sdxf

    # Get CR1 and CR2 references from the original (cleaned) inputs.
    ref1 <- if (!is.null(c1o) && length(c1o) > 0) c1o[1] else ""
    ref2 <- if (!is.null(c2o) && length(c2o) > 0) c2o[1] else ""

    ## Attempt similarity using CR1:
    if (ref1 != "") {
      # Consider only candidates sharing the same first letter as ref1.
      matching_cr1 <- candidates[substr(candidates, 1, 1) == substr(ref1, 1, 1)]
      if (length(matching_cr1) > 0) {
        sim1 <- sapply(matching_cr1, function(x) check_similarity(ref1, x))
        if (max(sim1) > 0) {
          best_cr1 <- matching_cr1[sim1 == max(sim1)]
          if (length(best_cr1) == 1) {
            return(list(clin_pdx = best_cr1[1], clin_pdx_source = 3))
          } else {
            set.seed(seed)
            chosen <- sample(best_cr1, 1)
            return(list(clin_pdx = chosen, clin_pdx_source = 4))
          }
        }
      }
    }

    ## If no candidate is similar to CR1, attempt similarity using CR2:
    if (ref2 != "") {
      matching_cr2 <- candidates[substr(candidates, 1, 1) == substr(ref2, 1, 1)]
      if (length(matching_cr2) > 0) {
        sim2 <- sapply(matching_cr2, function(x) check_similarity(ref2, x))
        if (max(sim2) > 0) {
          best_cr2 <- matching_cr2[sim2 == max(sim2)]
          if (length(best_cr2) == 1) {
            return(list(clin_pdx = best_cr2[1], clin_pdx_source = 5))
          } else {
            set.seed(seed)
            chosen <- sample(best_cr2, 1)
            return(list(clin_pdx = chosen, clin_pdx_source = 6))
          }
        }
      }
    }

    ## No candidate shows similarity to CR1 or CR2.
    if (length(candidates) == 1) {
      # Rule 7: Only one eligible SDx → return it.
      return(list(clin_pdx = candidates[1], clin_pdx_source = 7))
    } else {
      # Rule 8: Multiple eligible SDx with no similarity → choose one at random.
      set.seed(seed)
      chosen <- sample(candidates, 1)
      return(list(clin_pdx = chosen, clin_pdx_source = 8))
    }
  }, c1_filtered, c2_filtered, clin_sdx_filtered, c1_original, c2_original, SIMPLIFY = FALSE)

  # Return the results as vectors.
  return(list(
    clin_pdx = sapply(algo_result, `[[`, "clin_pdx"),
    clin_pdx_source = sapply(algo_result, `[[`, "clin_pdx_source")
  ))
}
