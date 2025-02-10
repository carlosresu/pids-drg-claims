remap_patient_data <- function(col, remapping) {
  remapped <- eval(
    remapping,
    list(
      dt = data.table(data = col),
      column_name = "data"
    )
  )
  return(remapped)
}
