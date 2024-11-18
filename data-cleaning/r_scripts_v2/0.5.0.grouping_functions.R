# generate_dob <- function(bdays, ages, date_adms) {
#   ## Function to generate dates of birth (DOB) based on birthdates, ages, and admission dates

#   set.seed(global_seed) # Ensure reproducibility by setting a global seed

#   require(lubridate) # Load lubridate for date manipulation

#   # Convert ages to numeric
#   ages <- as.numeric(ages)

#   # Initialize DOB vector with NA values
#   dob <- rep(NA_character_, length(ages))

#   ## Step 1: Use provided birthdates where available
#   valid_bdays_indices <- !is.na(bdays) & bdays != "" # Find valid birthdate indices

#   # Convert valid birthdates to desired format
#   dob[valid_bdays_indices] <- format(ymd(bdays[valid_bdays_indices]), "%d/%m/%Y")

#   ## Step 2: Handle missing birthdates
#   missing_bday_indices <- which(is.na(bdays) | bdays == "") # Find indices with missing birthdates
#   ref_dates <- ymd(date_adms[missing_bday_indices]) # Get reference dates (admission dates)

#   ## Step 3: Handle age == 0
#   zero_age_indices <- which(!is.na(ages[missing_bday_indices]) & ages[missing_bday_indices] == 0)

#   # Generate random days for age 0 cases
#   if (length(zero_age_indices) > 0) {
#     dob[missing_bday_indices[zero_age_indices]] <- format(
#       ref_dates[zero_age_indices] - days(sample(1:27, length(zero_age_indices), replace = TRUE)), "%d/%m/%Y"
#     )
#   }
#   # TODO for where birthdate exists impute it as the difference between date admission and birthdate
#   # Otherwise just "3"

#   ## Step 4: Handle positive ages
#   positive_age_indices <- which(!is.na(ages[missing_bday_indices]) & ages[missing_bday_indices] > 0)

#   # Subtract exact age in years from the reference date
#   if (length(positive_age_indices) > 0) {
#     truncated_ages <- floor(ages[missing_bday_indices][positive_age_indices])
#     dob[missing_bday_indices[positive_age_indices]] <- format(
#       ref_dates[positive_age_indices] - years(truncated_ages), "%d/%m/%Y"
#     )
#   }

#   # Return the vector of generated DOBs
#   return(dob)
# }

export_for_grouper <- function(dt, output_txt_file, chunk_number) {
  #' @title Export Data for Batch Grouper
  #'
  #' @description This function exports data for batch grouper,
  #' generating necessary columns and formatting them accordingly.
  #'
  #' @param dt data.table. The input data table.
  #' @param output_txt_file character. The path to the output text file.
  #'
  #' @return NULL.

  output_dt_thai <- data.table()

  # Create CASEID column
  if (chunk_number == 1) output_dt_thai[, CASEID := as.character(1:nrow(dt))]
  if (chunk_number == 2) output_dt_thai[, CASEID := as.character(5000001:10000000)]
  if (chunk_number == 3) output_dt_thai[, CASEID := as.character(10000001:(10000000 + nrow(dt)))]

  # Format Date of Birth (DOB)
  output_dt_thai[, DOB := as.character(format(as.Date(dt$pat_bdate), "%d/%m/%Y"))]

  # Ensure both data.tables have the correct keys for joining
  # setkey(output_dt_thai, CASEID)
  # setkey(pat_bdate_recomputed, id_series)

  # Step 1: Capture rows where DOB is NA and will be filled with recomputed DOB (before updating)
  # recomputed_dob_rows_before <- output_dt_thai[
  #   is.na(DOB) & CASEID %in% pat_bdate_recomputed$id_series,
  #   .(CASEID, DOB_before = DOB)
  # ]

  # Print rows before the DOB update
  # cat("Rows where DOB will be updated from recomputed values (Before):\n")
  # print(recomputed_dob_rows_before)

  # Step 2: Join recomputed DOB values
  # output_dt_thai <- merge(output_dt_thai, pat_bdate_recomputed, by.x = "CASEID", by.y = "id_series", all.x = TRUE, all.y = FALSE)

  # Step 3: Update DOB with recomputed DOB where applicable
  # output_dt_thai[
  #   is.na(DOB) & !is.na(pat_bdate_recomputed),
  #   DOB := as.character(format(as.Date(pat_bdate_recomputed), "%d/%m/%Y"))
  # ]

  # Step 4: Capture rows after the DOB update (only the updated ones)
  # recomputed_dob_rows_after <- output_dt_thai[
  #   CASEID %in% recomputed_dob_rows_before$CASEID,
  #   .(CASEID, DOB_after = DOB)
  # ]

  # Print rows after the DOB update
  # cat("Rows where DOB was updated from recomputed values (After):\n")
  # print(recomputed_dob_rows_after)

  # Format DOB
  # output_dt_thai[, pat_bdate_recomputed := NULL]
  # Format Sex
  output_dt_thai[, Sex := ifelse(dt$pat_sex == "M", 1, 2)]

  # Format Admission Date and Time
  output_dt_thai[, DateAdm := format(as.Date(dt$date_adm), "%d/%m/%Y")]
  output_dt_thai[, TimeAdm := format(as.POSIXct(dt$time_adm, format = "%H:%M:%S"), "%H%M")]

  # Format Discharge Date and Time
  output_dt_thai[, DateDsc := format(as.Date(dt$date_dis), "%d/%m/%Y")]
  output_dt_thai[, TimeDsc := format(as.POSIXct(dt$time_dis, format = "%H:%M:%S"), "%H%M")]

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
