generate_dob_vectorized <- function(bdays, ages, date_adms) {
  #' @title Generate Date of Birth (DOB) Vectorized
  #' @description Generates date of birth (DOB) values vectorized
  #' from birthdates, ages, and admission dates.
  #' @param bdays A vector of birthdates.
  #' @param ages A vector of ages.
  #' @param date_adms A vector of admission dates.
  #' @return A vector of generated DOB values.
  require(lubridate)

  dob <- rep(NA_character_, length(ages))

  # Use provided birthdates where available
  valid_bdays_indices <- !is.na(bdays) & bdays != ""
  dob[valid_bdays_indices] <- format(
    mdy(bdays[valid_bdays_indices]),
    "%d/%m/%Y"
  )

  # Identify indices where birthdates are missing
  missing_bday_indices <- which(is.na(bdays) | bdays == "")
  ref_dates <- mdy(date_adms[missing_bday_indices])

  # Handle cases where ages are zero
  zero_age_indices <- which(
    !is.na(ages[missing_bday_indices]) & ages[missing_bday_indices] == 0
  )
  dob[missing_bday_indices[zero_age_indices]] <- format(
    ref_dates[zero_age_indices] - days(
      sample(
        1:27, length(zero_age_indices),
        replace = TRUE
      )
    ), "%d/%m/%Y"
  )

  # Handle cases where ages are positive
  positive_age_indices <- which(
    !is.na(ages[missing_bday_indices]) & ages[missing_bday_indices] > 0
  )
  truncated_ages <- floor(
    ages[missing_bday_indices][positive_age_indices]
  )
  dob[missing_bday_indices[positive_age_indices]] <- format(
    ref_dates[positive_age_indices] - years(truncated_ages) - days(
      sample(1:170, length(positive_age_indices), replace = TRUE)
    ), "%d/%m/%Y"
  )

  return(dob)
}

# Helper function to generate date of birth (DOB) values
generate_dob_column <- function(dt) {
  #' @title Generate DOB Column
  #' @description Generates date of birth (DOB) values for a data.table.
  #' @param dt A data.table containing birthdates, ages, and admission dates.
  #' @return A vector of generated DOB values.
  generate_dob_vectorized(dt$pat_bdate, dt$pat_age, dt$date_adm)
}

# Helper function to format dates
format_dates <- function(date_vector) {
  #' @title Format Dates
  #' @description Formats dates in a vector to "dd/mm/yyyy" format.
  #' @param date_vector A vector of dates to format.
  #' @return A vector of formatted dates.
  format(mdy(date_vector), "%d/%m/%Y")
}

# Helper function to format times
format_times <- function(time_vector) {
  #' @title Format Times
  #' @description Formats times in a vector by removing colons.
  #' @param time_vector A vector of times to format.
  #' @return A vector of formatted times.
  gsub(":", "", time_vector)
}

# Helper function to split ICD codes
split_icd_codes_for_batch_grouper <- function(icd_str) {
  #' @title Split ICD Codes
  #' @description Splits ICD codes into a fixed-length vector.
  #' @param icd_str A string of ICD codes.
  #' @return A fixed-length vector of ICD codes.
  codes <- unlist(icd_str)
  length(codes) <- 12
  codes
}

# Helper function to split RVS codes
split_rvs_codes_for_batch_grouper <- function(rvs_str) {
  #' @title Split RVS Codes
  #' @description Splits RVS codes into a fixed-length vector.
  #' @param rvs_str A string of RVS codes.
  #' @return A fixed-length vector of RVS codes.
  codes <- unlist(rvs_str)
  length(codes) <- 20
  codes
}

# Helper function to prepare and write output data.table
prepare_and_write_output <- function(output_dt, output_txt_file) {
  #' @title Prepare and Write Output
  #' @description Prepares and writes the output data.table to a file.
  #' @param output_dt The data.table to write.
  #' @param output_txt_file The path of the output text file.
  # Replace NA values with '--'
  output_dt[is.na(output_dt)] <- "--"
  # Convert list columns to comma-separated strings
  for (col in names(output_dt)) {
    if (is.list(output_dt[[col]])) {
      output_dt[[col]] <- sapply(output_dt[[col]], paste, collapse = ",")
    }
  }
  # Write the data.table to a file
  fwrite(output_dt, output_txt_file, sep = "|", col.names = TRUE)
}

# Main function to export data for batch grouper processing
export_for_batch_grouper <- function(dt, year_to_load, output_txt_file) {
  #' @title Export Data for Batch Grouper
  #' @description Exports data for batch grouper processing.
  #' @param dt A data.table to export.
  #' @param year_to_load The year to load for the export.
  #' @param output_txt_file The path of the output text file.

  output_dt <- data.table(CASEID = 1:nrow(dt))
  output_dt[, DOB := generate_dob_column(dt)]
  output_dt[, Sex := ifelse(dt$pat_sex == "M", 1, 2)]
  output_dt[, DateAdm := format_dates(dt$date_adm)]
  output_dt[, TimeAdm := format_times(dt$time_adm)]
  output_dt[, DateDsc := format_dates(dt$date_dis)]
  output_dt[, TimeDsc := format_times(dt$time_dis)]
  output_dt[, DischT := dt$clin_discharge]
  output_dt[, AdmWt := dt$pat_bwt]
  output_dt[, PDx := dt$pdx]

  icd_codes_list <- lapply(dt$clin_icd, split_icd_codes_for_batch_grouper)
  icd_codes <- as.data.table(do.call(rbind, icd_codes_list))
  icd_cols <- paste0("SDx", 1:12)
  output_dt[, (icd_cols) := icd_codes]

  rvs_codes_list <- lapply(dt$icd9_list, split_rvs_codes_for_batch_grouper)
  rvs_codes <- as.data.table(do.call(rbind, rvs_codes_list))
  proc_cols <- paste0("Proc", 1:20)
  output_dt[, (proc_cols) := rvs_codes]

  prepare_and_write_output(output_dt, output_txt_file)
}
