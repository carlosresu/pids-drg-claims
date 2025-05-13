export_for_grouper <- function(dt, output_txt_file, chunk_number) {
  # This function exports data for batch grouper,
  # generating necessary columns and formatting them accordingly.

  # Create the output data.table with the same number of rows as 'dt'
  output_dt_thai <- data.table()

  # Copy case IDs
  output_dt_thai[, CASEID := dt$caseid]

  # Format Date of Birth (DOB) as dd/mm/yyyy
  output_dt_thai[, DOB := as.character(
    format(as.Date(dt$pat_bdate), "%d/%m/%Y")
  )]

  # Format Sex: "M" → 1, others → 2
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

  # Copy discharge type
  output_dt_thai[, DischT := dt$clin_discharge]

  # Copy admission weight
  output_dt_thai[, AdmWt := dt$pat_bwt]

  # Copy principal diagnosis code
  output_dt_thai[, PDx := dt$clin_pdx]

  # Format Secondary Diagnosis Codes (ensure 12 columns)
  icd_codes_list <- lapply(dt$clin_sdx, function(icd_str) {
    codes <- unlist(icd_str) # Flatten to vector
    length(codes) <- 12 # Pad or trim to 12 elements
    codes
  })
  icd_codes <- as.data.table(do.call(rbind, icd_codes_list))
  icd_cols <- paste0("SDx", 1:12) # Column names: SDx1 to SDx12
  output_dt_thai[, (icd_cols) := icd_codes]

  # Format Procedure Codes (ensure 20 columns)
  proc_codes_list <- lapply(dt$clin_proc, function(proc_str) {
    codes <- unlist(proc_str) # Flatten to vector
    length(codes) <- 20 # Pad or trim to 20 elements
    codes
  })
  proc_codes <- as.data.table(do.call(rbind, proc_codes_list))
  proc_cols <- paste0("Proc", 1:20) # Column names: Proc1 to Proc20
  output_dt_thai[, (proc_cols) := proc_codes]

  # Replace all NA values with '--' (expected by the grouper)
  output_dt_thai[is.na(output_dt_thai)] <- "--"

  # Inspect final structure before writing
  str(output_dt_thai)

  # Write the data.table to a pipe-delimited text file
  fwrite(output_dt_thai, output_txt_file, sep = "|", col.names = TRUE)

  # In debug mode, don't return anything
  if (to_debug) {
    return(NULL)
  }
}
