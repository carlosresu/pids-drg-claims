# Function to remap patient data using the global remapped_column
remap_patient_data <- function(
    pat_type,
    pat_memcat_parent,
    pat_memcat_child,
    clin_discharge,
    claim_status,
    known_values,
    remapped_column) {
  # Initialize lists to store results
  mapped <- list()
  unmapped <- list()
  remapped <- list()

  # Define columns to process and their input data
  columns_to_remap <- list(
    pat_type = pat_type,
    pat_memcat_parent = pat_memcat_parent,
    pat_memcat_child = pat_memcat_child,
    clin_discharge = clin_discharge,
    claim_status = claim_status
  )

  # Loop through each column and apply the global remapped_column logic
  for (col_name in names(columns_to_remap)) {
    column_data <- columns_to_remap[[col_name]]

    # Convert to data.table for processing
    dt <- data.table(column_data = column_data)

    # Dynamically evaluate the global remapped_column for each column
    remapped_col <- eval(remapped_column, envir = list(dt = dt, column_name = "column_data"))

    # Identify unmapped values
    unknown_values <- setdiff(
      column_data[!is.na(column_data)],
      known_values[[col_name]]
    )

    # Issue a warning if unmapped values are found
    if (length(unknown_values) > 0) {
      warning(sprintf(
        "Unmapped values in column '%s': %s",
        col_name, paste(unknown_values, collapse = ", ")
      ))
    }

    # Store the remapped column
    remapped[[col_name]] <- remapped_col

    # Store the mapped data for this column
    mapped[[col_name]] <- unique(data.table(
      Original = column_data,
      Mapped = remapped_col
    ))

    # Store the unmapped values
    unmapped[[col_name]] <- unknown_values
  }

  # Prepare the return values
  return(
    list(
      remapped = remapped,
      pat_type_mapped = mapped$pat_type,
      pat_memcat_parent_mapped = mapped$pat_memcat_parent,
      pat_memcat_child_mapped = mapped$pat_memcat_child,
      clin_discharge_mapped = mapped$clin_discharge,
      claim_status_mapped = mapped$claim_status,
      pat_type_unmapped = unmapped$pat_type,
      memcat_parent_unmapped = unmapped$pat_memcat_parent,
      memcat_child_unmapped = unmapped$pat_memcat_child,
      discharge_unmapped = unmapped$clin_discharge,
      claim_status_unmapped = unmapped$claim_status
    )
  )
}
