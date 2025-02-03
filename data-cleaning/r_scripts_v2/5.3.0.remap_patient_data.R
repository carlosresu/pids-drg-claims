remap_patient_data <- function(col, remapping) {
  eval(remapping,
  list(dt = data.table(data = col),
  column_name = "data"))
}
