remap_patient_data <- function(col, remapping) {
  # Apply a dynamic remapping expression to a column of patient data.
  # 'remapping' is expected to be an expression that uses
  # a data.table named 'dt' and accesses the target column via
  # the variable name 'column_name'.

  remapped <- eval(
    remapping, # Evaluate the remapping expression (e.g., dt[, new := f(data)])
    list(
      dt = data.table(data = col), # Wrap the input vector as a
      # one-column data.table
      column_name = "data" # Provide column name reference as a variable
    )
  )

  # Return the result of the evaluated expression
  return(remapped)
}
