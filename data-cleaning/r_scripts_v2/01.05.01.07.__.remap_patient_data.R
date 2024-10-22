remap_patient_data <- function(dt, to_view_checks = TRUE) {
  ## Remap the categorical columns in the inpatient data

  # Initialize unmapped variables and mapped data tables
  pat_unmap <- parent_unmap <- child_unmap <- NULL
  discharge_unmap <- claim_status_unmap <- NULL
  pat_mapped <- parent_mapped <- child_mapped <- NULL
  discharge_mapped <- claim_status_mapped <- NULL

  # Define the columns that need remapping
  columns_to_remap <- list(
    pat_type = "pat_type",
    pat_memcat_parent = "pat_memcat_parent",
    pat_memcat_child = "pat_memcat_child",
    clin_discharge = "clin_discharge",
    claim_status = "claim_status"
  )

  # Apply remapping for each column
  for (col_name in names(columns_to_remap)) {
    result <- remap_columns(
      dt,
      columns_to_remap[[col_name]],
      to_view_checks,
      known_values,
      remapped_column
    )

    # Update the original column using `set`
    set(dt, j = columns_to_remap[[col_name]], value = result$remapped)

    # Capture mapped and unmapped values
    if (col_name == "pat_type") {
      pat_mapped <- unique(
        data.table(
          Original = result$original,
          Mapped = result$remapped
        )
      )
      if (length(result$unmapped) > 0) {
        pat_unmap <- result$unmapped
      }
    } else if (col_name == "pat_memcat_parent") {
      parent_mapped <- unique(
        data.table(
          Original = result$original,
          Mapped = result$remapped
        )
      )
      if (length(result$unmapped) > 0) {
        parent_unmap <- result$unmapped
      }
    } else if (col_name == "pat_memcat_child") {
      child_mapped <- unique(
        data.table(
          Original = result$original,
          Mapped = result$remapped
        )
      )
      if (length(result$unmapped) > 0) {
        child_unmap <- result$unmapped
      }
    } else if (col_name == "clin_discharge") {
      discharge_mapped <- unique(
        data.table(
          Original = result$original,
          Mapped = result$remapped
        )
      )
      if (length(result$unmapped) > 0) {
        discharge_unmap <- result$unmapped
      }
    } else if (col_name == "claim_status") {
      claim_status_mapped <- unique(
        data.table(
          Original = result$original,
          Mapped = result$remapped
        )
      )
      if (length(result$unmapped) > 0) {
        claim_status_unmap <- result$unmapped
      }
    }
  }

  # Return the remapped data and all mapping/unmapped data
  return(
    list(
      data = dt,
      pat_type_mapped = pat_mapped,
      pat_memcat_parent_mapped = parent_mapped,
      pat_memcat_child_mapped = child_mapped,
      clin_discharge_mapped = discharge_mapped,
      claim_status_mapped = claim_status_mapped,
      pat_type_unmapped = pat_unmap,
      memcat_parent_unmapped = parent_unmap,
      memcat_child_unmapped = child_unmap,
      discharge_unmapped = discharge_unmap,
      claim_status_unmapped = claim_status_unmap
    )
  )
}
