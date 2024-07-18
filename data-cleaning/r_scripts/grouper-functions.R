source(here("data-cleaning", "r_scripts", "libraries.R"))

generate_dob_vectorized <- function(bdays, ages, date_adms) {
  require(lubridate)

  # Ensure ages are numeric
  ages <- as.numeric(ages)

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


generate_dob_column <- function(dt) {
  generate_dob_vectorized(dt$pat_bdate, dt$pat_age, dt$date_adm)
}

format_dates <- function(date_vector) {
  format(mdy(date_vector), "%d/%m/%Y")
}

format_times <- function(time_vector) {
  gsub(":", "", time_vector)
}

split_icd_codes_for_batch_grouper <- function(icd_str) {
  codes <- unlist(icd_str)
  length(codes) <- 12
  codes
}

split_rvs_codes_for_batch_grouper <- function(rvs_str) {
  codes <- unlist(rvs_str)
  length(codes) <- 20
  codes
}

prepare_and_write_output <- function(output_dt, output_txt_file) {
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

export_for_batch_grouper <- function(dt, year_to_load, output_txt_file) {
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
