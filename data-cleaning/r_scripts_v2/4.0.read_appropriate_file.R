read_appropriate_file <- function(read_part, to_sample_argument = to_sample) {
  chunk_file <- if (to_sample_argument) {
    here(raw_claims_samples_path, paste0(
      "sampled_claims_", year, "_", sample_size_divisor,
      "_part_", sprintf("%02d", loop_part), "_of_", split_parts, ".rds"
    ))
  } else {
    here(raw_claims_parts_path, paste0(
      full_claims_prefix, year,
      "_part_", sprintf("%02d", loop_part), "_of_", split_parts, ".rds"
    ))
  }

  dt <- readRDS(chunk_file)

  available_columns <<- colnames(dt)

  # Drop columns
  if (any(drop_cols %in% available_columns)) {
    dt <- dt[, (drop_cols) := NULL]
  }

  if (any(drop_cols_manual %in% available_columns)) {
    dt <- dt[, (drop_cols_manual) := NULL]
  }

  dt <- replace_na_or_empty(dt = dt, replace_with = "NA_character_")

  ## Apply column classes only to the columns that exist in the data
  col_classes <- sapply(available_columns, function(col) {
    if (col %in% unlist(expected_types["character"])) {
      return("character")
    }
    if (col %in% unlist(expected_types["integer"])) {
      return("integer")
    }
    if (col %in% unlist(expected_types["factor"])) {
      return("factor")
    }
    if (col %in% unlist(expected_types["numeric"])) {
      return("numeric")
    }
  })

  # Cast column types with checks
  for (col in names(col_classes)) {
    original_values <- dt[[col]]

    dt[[col]] <- switch(col_classes[[col]],
      "character" = as.character(dt[[col]]),
      "factor" = {
        levels <- unique(dt[[col]])
        as.factor(dt[[col]])
      },
      "integer" = {
        suppressWarnings(as.integer(dt[[col]]))
      },
      "numeric" = {
        suppressWarnings(as.numeric(dt[[col]]))
      },
      dt[[col]] # Default case: no conversion if unrecognized type
    )
  }

  nrow_start[[read_part]] <<- nrow(dt)

  return(dt)
}
