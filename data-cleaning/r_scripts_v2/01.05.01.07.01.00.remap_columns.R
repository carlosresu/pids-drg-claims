### Helper functions for remapping/reformatting key columns in the claims
remap_columns <- function(dt, column_name, to_view_checks = TRUE, known_values, remap_logic = remapped_column) {
  ## Function to remap different categorical columns in the claims dataset
  # dt: data.table
  # column_name: name of the column to remap
  # to_view_checks: boolean flag to enable check and capture of unmapped values
  # known_values: a list of known values for the specific column
  # remap_logic: quoted fcase logic passed as an argument

  # First, initialize the column with the original values to ensure no row count mismatch
  original_values <- dt[[column_name]]

  # Use `set` to modify the data.table by reference to avoid copying
  set(dt, j = column_name, value = eval(remap_logic))

  # Check for unmapped entries
  unknown_values <- setdiff(original_values[!is.na(original_values)], known_values[[column_name]])

  if (length(unknown_values) > 0 && to_view_checks) {
    warning(sprintf(
      "Unmapped values in column '%s': %s",
      column_name, paste(unknown_values, collapse = ", ")
    ))
  }

  return(
    list(
      data = dt,
      original = original_values,
      remapped = dt[[column_name]],
      unmapped = unknown_values
    )
  )
}
