export_for_grouper <- function(dt, output_txt_file, chunk_number) {
  # This function exports data for batch grouper,
  # generating necessary columns and formatting them accordingly.

  # Create the output data.table with the same number of rows as 'dt'
  output_dt_thai <- data.table()

  output_dt_thai[, CASEID := dt$caseid]

  # Format Date of Birth (DOB)
  output_dt_thai[, DOB := as.character(
    format(as.Date(dt$pat_bdate), "%d/%m/%Y")
  )]

  # Format Sex
  output_dt_thai[, Sex := ifelse(dt$pat_sex == "M", 1, 2)]

  # Format Admission Date and Time
  output_dt_thai[, DateAdm := format(
    as.Date(dt$date_adm), "%d/%m/%Y"
  )]
  output_dt_thai[, TimeAdm := format(
    as.POSIXct(dt$time_adm, format = "%H:%M:%S"), "%H%M"
  )]

  # Format Discharge Date and Time
  output_dt_thai[, DateDsc := format(
    as.Date(dt$date_dis), "%d/%m/%Y"
  )]
  output_dt_thai[, TimeDsc := format(
    as.POSIXct(dt$time_dis, format = "%H:%M:%S"), "%H%M"
  )]

  # Discharge Type
  output_dt_thai[, DischT := dt$clin_discharge]

  # Admission Weight
  output_dt_thai[, AdmWt := dt$pat_bwt]

  # Principal Diagnosis Code
  output_dt_thai[, PDx := dt$clin_pdx]

  # Secondary Diagnosis Codes (SDx1 to SDx12)
  icd_codes_list <- lapply(dt$clin_sdx, function(icd_str) {
    codes <- unlist(icd_str)
    length(codes) <- 12 # Ensure there are 12 elements
    codes
  })
  icd_codes <- as.data.table(do.call(rbind, icd_codes_list))
  icd_cols <- paste0("SDx", 1:12)
  output_dt_thai[, (icd_cols) := icd_codes]

  # Procedure Codes (Proc1 to Proc20)
  proc_codes_list <- lapply(dt$clin_proc, function(proc_str) {
    codes <- unlist(proc_str)
    length(codes) <- 20 # Ensure there are 20 elements
    codes
  })
  proc_codes <- as.data.table(do.call(rbind, proc_codes_list))
  proc_cols <- paste0("Proc", 1:20)
  output_dt_thai[, (proc_cols) := proc_codes]

  # Replace NA values with '--'
  output_dt_thai[is.na(output_dt_thai)] <- "--"

  str(output_dt_thai)

  # Write the data.table to a file with vertical bar (|) as delimiter
  fwrite(output_dt_thai, output_txt_file, sep = "|", col.names = TRUE)

  if (to_debug) {
    return(NULL)
  }
}
