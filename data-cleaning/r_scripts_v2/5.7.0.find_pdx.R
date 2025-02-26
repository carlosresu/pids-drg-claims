# Main function to impute the primary diagnosis (pdx) based on provided ICD-10 codes.
find_pdx <- function(c1_arg, c2_arg, clin_sdx_arg, seed, accpdx = acc_pdx) {
  # Create a unique set of accepted ICD codes.
  acc_pdx_set_final <- unique(accpdx)

  # Function to filter ICD codes: keep only non-NA codes that are in the accepted set.
  filter_icds <- function(codes, acc_pdx_set) {
    # Return an empty character vector if codes is NULL.
    if (is.null(codes)) {
      return(character(0))
    }
    # Remove NA values and retain only codes present in the accepted set.
    filtered <- codes[!is.na(codes) & codes %chin% acc_pdx_set]
    return(filtered)
  }

  # Helper function to "clean" the original codes by removing NAs.
  clean_codes <- function(codes) {
    # Return NULL if the input is NULL or empty.
    if (is.null(codes) || length(codes) == 0) {
      return(NULL)
    }
    # Remove any NA values.
    cleaned <- codes[!is.na(codes)]
    # Return NULL if cleaning results in an empty vector; otherwise, return the cleaned vector.
    if (length(cleaned) == 0) {
      return(NULL)
    } else {
      return(cleaned)
    }
  }

  # Updated helper function to compute similarity between two strings.
  # It splits each string into characters and counts how many of the starting characters match.
  check_similarity <- function(x, y) {
    # Split each string into its individual characters.
    chars_x <- strsplit(x, "")[[1]]
    chars_y <- strsplit(y, "")[[1]]
    # Determine the number of characters to compare (the minimum length).
    min_len <- min(length(chars_x), length(chars_y))
    # Sum up the number of matching characters at the start.
    sim <- sum(chars_x[1:min_len] == chars_y[1:min_len])
    return(sim)
  }

  # Filter c1, c2, and clin_sdx to include only codes in the accepted set.
  c1_filtered <- lapply(c1_arg, filter_icds, acc_pdx_set_final)
  c2_filtered <- lapply(c2_arg, filter_icds, acc_pdx_set_final)
  clin_sdx_filtered <- lapply(clin_sdx_arg, filter_icds, acc_pdx_set_final)

  # Also, keep a cleaned copy of the original c1 for similarity comparisons.
  c1_original <- lapply(c1_arg, clean_codes)

  # Process each row of inputs using mapply.
  algo_result <- mapply(\(c1f, c2f, sdxf, c1o) {
    ## Rule 1: If clin_c1 (filtered) has an acceptable ICD, use its first value.
    if (!is.null(c1f) && length(c1f) > 0) {
      return(list(clin_pdx = c1f[1], clin_pdx_source = 1))
    }

    ## Rule 2: Otherwise, if clin_c2 (filtered) has an acceptable ICD, use its first value.
    if (!is.null(c2f) && length(c2f) > 0) {
      return(list(clin_pdx = c2f[1], clin_pdx_source = 2))
    }

    ## Rule 3: If exactly one acceptable ICD exists in clin_sdx, choose that.
    if (!is.null(sdxf) && length(sdxf) == 1) {
      return(list(clin_pdx = sdxf[1], clin_pdx_source = 3))
    }

    ## If no acceptable ICD exists in clin_sdx, return NA (Rule 99).
    if (is.null(sdxf) || length(sdxf) == 0) {
      return(list(clin_pdx = NA_character_, clin_pdx_source = 99))
    }

    ## Now we have multiple acceptable ICD codes in clin_sdx.
    ## Use the original clin_c1 as the reference for similarity comparisons.
    ref <- if (!is.null(c1o) && length(c1o) > 0) c1o[1] else ""

    # If no reference is available (i.e. clin_c1 original is empty), choose a random candidate.
    if (ref == "") {
      set.seed(seed)
      return(list(clin_pdx = sample(sdxf, 1), clin_pdx_source = 7))
    }

    # All acceptable clin_sdx codes are our candidates.
    candidates <- sdxf

    # Identify candidates that share the same starting letter as the reference.
    matching_candidates <- candidates[substr(candidates, 1, 1) == substr(ref, 1, 1)]

    ## Rule 4: If exactly one candidate shares starting letters with ref, choose it.
    if (length(matching_candidates) == 1) {
      return(list(clin_pdx = matching_candidates[1], clin_pdx_source = 4))
    }
    ## If more than one candidate shares the first letter, proceed to compare similarity.
    else if (length(matching_candidates) > 1) {
      # Compute similarity scores for each candidate against the reference.
      sim_scores <- sapply(matching_candidates, \(x) check_similarity(ref, x))
      max_sim <- max(sim_scores)
      # Identify the candidate(s) with the highest similarity score.
      best_candidates <- matching_candidates[sim_scores == max_sim]

      ## Rule 5: If there is a unique best candidate (i.e. highest similarity), choose it.
      if (length(best_candidates) == 1) {
        return(list(clin_pdx = best_candidates[1], clin_pdx_source = 5))
      } else {
        ## Rule 6: If there is a tie among candidates for most matching starting letters, choose one at random.
        set.seed(seed)
        chosen <- sample(best_candidates, 1)
        return(list(clin_pdx = chosen, clin_pdx_source = 6))
      }
    }
    ## Rule 7: If none of the acceptable clin_sdx codes share a starting letter with ref, choose one at random.
    else {
      set.seed(seed)
      return(list(clin_pdx = sample(candidates, 1), clin_pdx_source = 7))
    }
  }, c1_filtered, c2_filtered, clin_sdx_filtered, c1_original, SIMPLIFY = FALSE)

  # Combine the results from all rows and return them as vectors.
  return(list(
    clin_pdx = sapply(algo_result, `[[`, "clin_pdx"),
    clin_pdx_source = sapply(algo_result, `[[`, "clin_pdx_source")
  ))
}
