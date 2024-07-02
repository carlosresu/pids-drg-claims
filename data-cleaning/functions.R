### Read Entire File

read_entire_file <- function() {
  tic("Reading entire data file")
  #' Read the entire data file.
  #'
  #' @return A data.table containing the entire data file.
  dt <- fread(full_claims, na.strings = na_values)
  toc()
  return(dt)
}

### Read Sampled File

read_sampled_file <- function() {
  #' Read the sampled data file.
  #'
  #' @return A data.table containing the sampled data file.
  dt <- fread(sampled_claims, na.strings = na_values)
  return(dt)
}

### Sample Data

sample_data <- function(dt) {
  #' Sample data from the data table.
  #'
  #' @param dt A data.table to sample from.
  #' @return A sampled data.table.
  dt <- dt[sample(.N, min(sample_size, .N))]
  return(dt)
}

### Write Data

write_data <- function(dt, path) {
  #' Write data to a specified path.
  #'
  #' @param dt A data.table to write.
  #' @param path A character string specifying the path to write the data to.
  fwrite(dt, path)
}

### Keep Necessary Columns

keep_necessary_columns <- function(dt) {
  #' Keep necessary columns in the data table.
  #'
  #' @param dt A data.table.
  #' @return A data.table with only the necessary columns.
  cols_to_keep <- setdiff(1:ncol(dt), drop_cols)
  dt <- dt[, ..cols_to_keep]
  return(dt)
}

### Add Year Column

add_year_column <- function(dt, year_to_load) {
  #' Add a year column to the data table.
  #'
  #' @param dt A data.table.
  #' @param year_to_load A character string specifying the year to add.
  #' @return A data.table with the year column added.
  dt[, SRC_YR := as.integer(year_to_load)]
  return(dt)
}

### Process and Collapse Columns

process_and_collapse_columns <- function(dt, cols_to_process, new_col_name) {
  #' Process and collapse specified columns into a new column.
  #'
  #' @param dt A data.table.
  #' @param cols_to_process A character vector specifying columns to process.
  #' @param new_col_name A character string specifying the name of the new column.
  dt[, (cols_to_process) := lapply(.SD, function(col) {
    col <- iconv(col, to = "UTF-8", sub = "byte")
    col <- toupper(col)
    col <- str_trim(col)
    col <- str_replace_all(col, " ", "")
    col <- str_replace_all(col, "\n", "")
    col <- str_replace_all(col, "[^\\w\\d\\/\\s]+", "")
    col <- ifelse(col %in% na_like_strings, NA_character_, col)
    col
  }), .SDcols = cols_to_process]
  
  dt[, (new_col_name) := do.call(paste, c(.SD, sep = "||")), .SDcols = cols_to_process]
  dt[, (new_col_name) := str_replace_all(get(new_col_name), "\\|\\|NA", "")]
  dt[, (new_col_name) := str_replace_all(get(new_col_name), "NA\\|\\|", "")]
  dt[, (new_col_name) := str_replace_all(get(new_col_name), "\\|\\|$", "")]
  dt[, (new_col_name) := ifelse(get(new_col_name) == "NA", NA_character_, get(new_col_name))]
  
  dt[, (cols_to_process) := NULL]
}

### Process Patient Type

process_patient_type <- function(dt) {
  #' Process patient type.
  #'
  #' @param dt A data.table.
  #' @return A data.table with processed patient type.
  dt[, PATIENT_TYPE := fcase(
    PATIENT_TYPE == "MEMBER", "MEM",
    PATIENT_TYPE == "DEPENDENT", "DEP"
  )]
  return(dt)
}

### Process Memcat Parent Description

process_memcat_parent_desc <- function(dt) {
  #' Process memcat parent description.
  #'
  #' @param dt A data.table.
  #' @return A data.table with processed memcat parent description.
  dt[, MEMCAT_PARENT_DESC := fcase(
    MEMCAT_PARENT_DESC == "DIRECT CONTRIBUTOR", "DIRECT",
    MEMCAT_PARENT_DESC == "INDIRECT CONTRIBUTOR", "INDIRECT"
  )]
  return(dt)
}

### Process Memcat Child Description

process_memcat_child_desc <- function(dt) {
  #' Process memcat child description.
  #'
  #' @param dt A data.table.
  #' @return A data.table with processed memcat child description.
  dt[, MEMCAT_CHILD_DESC := fcase(
    MEMCAT_CHILD_DESC == "EMPLOYED PRIVATE", "FORMAL",
    MEMCAT_CHILD_DESC == "SELF-EARNING INDIVIDUAL", "INFORMAL",
    MEMCAT_CHILD_DESC == "SENIOR CITIZEN", "SENIOR",
    MEMCAT_CHILD_DESC == "INDIGENT", "INDIGENT",
    MEMCAT_CHILD_DESC == "LIFETIME MEMBER", "LIFETIME",
    MEMCAT_CHILD_DESC == "SPONSORED", "SPONSORED",
    MEMCAT_CHILD_DESC == "MIGRANT WORKER", "INFORMAL",
    MEMCAT_CHILD_DESC == "EMPLOYED GOVERNMENT", "FORMAL",
    MEMCAT_CHILD_DESC == "INFORMAL ECONOMY", "INFORMAL",
    MEMCAT_CHILD_DESC == "HOUSEHOLD HELP/KASAMBAHAY", "FORMAL",
    MEMCAT_CHILD_DESC == "FOREIGN NATIONAL", "INFORMAL",
    MEMCAT_CHILD_DESC == "FILIPINOS WITH DUAL CITIZENSHIP / LIVING ABROAD", "INFORMAL",
    MEMCAT_CHILD_DESC == "SELF EARNING INDIVIDUAL", "INFORMAL",
    MEMCAT_CHILD_DESC == "FAMILY DRIVER", "FORMAL"
  )]
  return(dt)
}

### Process Disposition

process_disposition <- function(dt) {
  #' Process disposition.
  #'
  #' @param dt A data.table.
  #' @return A data.table with processed disposition.
  dt[, DISPOSITION := fcase(
    DISPOSITION == "IMPROVED", 1L,
    DISPOSITION == "RECOVERED", 1L,
    DISPOSITION == "HOME/DISCHARGED AGAINST MEDICAL ADVICE", 2L,
    DISPOSITION == "ABSCONDED", 3L,
    DISPOSITION == "TRANSFERRED/REFERRED", 4L,
    DISPOSITION == "EXPIRED", 9L,
    DISPOSITION == "UNDEFINED", NA_integer_
  )]
  return(dt)
}

### Process ICD Codes

process_icd_codes <- function(dt) {
  #' Process ICD codes.
  #'
  #' @param dt A data.table.
  #' @return A data.table with processed ICD codes.
  icd_cols <- intersect(colnames(dt), c(paste0("ICDCODE", c(1:14, 16:170)), "ICCODED15"))
  process_and_collapse_columns(dt, icd_cols, "ICD_CODES")
  return(dt)
}

### Process RVS Codes

process_rvs_codes <- function(dt) {
  #' Process RVS codes.
  #'
  #' @param dt A data.table.
  #' @return A data.table with processed RVS codes.
  rvs_cols <- intersect(colnames(dt), paste0("RVSCODE", 1:20))
  process_and_collapse_columns(dt, rvs_cols, "RVS_CODES")
  return(dt)
}

### Find Lumped Codes

find_lumped_codes <- function(codes) {
  #' Find lumped codes.
  #'
  #' @param codes A character vector of codes.
  #' @return A logical vector indicating whether each code is lumped.
  sapply(codes, function(code) {
    if (is.na(code)) {
      return(FALSE)
    }
    nchar(code) > 4 &&
      sum(str_detect(strsplit(code, NULL)[[1]], "[A-Za-z]")) > 1 &&
      sum(str_detect(strsplit(code, NULL)[[1]], "[0-9]")) > 1
  })
}

### Remove Lumped ICD Codes

remove_lumped_icd_codes <- function(dt) {
  #' Remove lumped ICD codes.
  #'
  #' @param dt A data.table.
  #' @return A data.table with lumped ICD codes removed.
  to_delist <- unique(unlist(strsplit(dt$ICD_CODES, "\\|\\|")))[find_lumped_codes(unique(unlist(strsplit(dt$ICD_CODES, "\\|\\|"))))]
  
  if (length(to_delist) > 0 && !all(is.na(to_delist))) {
    delist_pattern <- paste(to_delist[!is.na(to_delist)], collapse = "|")
  } else {
    delist_pattern <- ""
  }
  
  if (delist_pattern != "") {
    delist_mask <- str_detect(dt$ICD_CODES, delist_pattern)
    dt[delist_mask, ICD_CODES := sapply(ICD_CODES, function(x) {
      paste(setdiff(unlist(strsplit(x, "\\|\\|")), to_delist), collapse = "||")
    })]
  }
  return(dt)
}

### Process RVS Code Mapping

process_rvs_code_mapping <- function(dt, rvs_icd9) {
  #' Process RVS code mapping.
  #'
  #' @param dt A data.table.
  #' @param rvs_icd9 A data.table containing RVS to ICD-9 mapping.
  #' @return A data.table with processed RVS code mapping.
  with_drg <- rvs_icd9[is_drg == TRUE]
  
  without_drg <- rvs_icd9[!rvs %in% with_drg$rvs]
  
  cat(sprintf('There are %d RVS codes without an ICD-9CM equivalent recognized by the TDRG ICD9CM\n', length(unique(without_drg$rvs))))
  
  rvs_map_list <- list()
  rvs_map_solo <- list()
  
  with_drg <- with_drg[order(rvs, -is_drg)]
  
  for (i in seq_len(nrow(with_drg))) {
    row <- with_drg[i, ]
    r <- row$rvs
    sub <- with_drg[rvs == r]
    if (nrow(sub) == 1) {
      rvs_map_solo[[r]] <- sub$icd9cm[1]
    } else {
      rvs_map_list[[r]] <- sub$icd9cm
    }
  }
  
  dt[, RVS_CODES := strsplit(as.character(RVS_CODES), "\\|\\|")]
  
  rvss <- unique(unlist(dt$RVS_CODES))
  rvss <- intersect(rvss, acr_rvs$rvs)
  
  cat(sprintf('There are %d unique RVS codes that appear in the claims.\n', length(rvss)))
  
  mappable_rvs <- intersect(rvss, rvs_icd9$rvs)
  cat(sprintf('Of these, %d (%.2f%%) have a mapping to an ICD-9-CM code.\n', length(mappable_rvs), length(mappable_rvs) * 100 / length(rvss)))
  
  multi_mapped_rvs <- intersect(rvss, names(rvs_map_list))
  cat(sprintf('Of these, there are %d (%.2f%%) with more than one ICD9 equivalent recognized by the Thai ICD9 library.\n', length(multi_mapped_rvs), length(multi_mapped_rvs) * 100 / length(mappable_rvs)))
  
  unmappable_rvs <- setdiff(rvss, mappable_rvs)
  cat(sprintf('There are %d (%.2f%%) with no ICD-9-CM equivalents.\n', length(unmappable_rvs), length(unmappable_rvs) * 100 / length(rvss)))
  
  dt2 <- dt[lengths(RVS_CODES) > 0]
  
  dt2_info <- dt2[, .N, by = PSEUDO_CLAIMSERIES]
  
  dt2 <- dt2[, .(RVS_CODES), by = .(PSEUDO_CLAIMSERIES)]
  dt2[, icd9_list := sapply(RVS_CODES, function(x) {
    codes <- unlist(strsplit(x, ","))
    mappable <- codes[codes %in% names(rvs_map_solo)]
    if (length(mappable) > 0) {
      paste(unique(unlist(lapply(mappable, function(c) rvs_map_solo[[c]]))), collapse = ",")
    } else {
      NA_character_
    }
  })]
  dt2[, rvs_unmap_list := sapply(RVS_CODES, function(x) {
    codes <- unlist(strsplit(x, ","))
    unmappable <- codes[!codes %in% names(rvs_map_solo) & !codes %in% names(rvs_map_list)]
    if (length(unmappable) > 0) {
      paste(unique(unmappable), collapse = ",")
    } else {
      NA_character_
    }
  })]
  
  dt <- merge(dt, dt2[, .(PSEUDO_CLAIMSERIES, icd9_list, rvs_unmap_list)], by = "PSEUDO_CLAIMSERIES", all.x = TRUE)
  
  return(dt)
}

### Process ICD10 Mapping

process_icd10_mapping <- function(dt) {
  #' Process ICD10 mapping.
  #'
  #' @param dt A data.table.
  #' @return A data.table with processed ICD10 mapping.
  tdrg_icd10 <- fread(here(path_to_aux, "i10.csv"))
  
  dt[, ICD_CODES := as.character(ICD_CODES)]
  
  icds <- unique(c(unlist(strsplit(dt$PRIMARY_ILLNESS, ",")), 
                   unlist(strsplit(dt$SECONDARY_ILLNESS, ",")), 
                   unlist(strsplit(dt$ICD_CODES, "\\|\\|"))))
  
  icds <- icds[!is.na(icds)]
  
  thai_icd10 <- unique(tdrg_icd10$CODE)
  icd10_tdrg <- intersect(icds, thai_icd10)
  
  cat(sprintf("There are %d unique entries for ICD-10 codes, of which %d (%.2f%%) are already in the Thai ICD-10 library\n",
              length(icds), length(icd10_tdrg), length(icd10_tdrg) * 100 / length(icds)))
  
  icd_mapping <- list()
  
  neoplasms <- unique(tdrg_icd10[grepl("/", tdrg_icd10$CODE), "CODE"])
  
  for (d in icds) {
    d <- str_trim(d)
    if (d %in% thai_icd10) {
      icd_mapping[[d]] <- d
    } else if (!(d %in% neoplasms) && grepl("[A-Za-z]", d) && grepl("[0-9]", d)) {
      if (nchar(d) == 3 && paste0(d, "9") %in% thai_icd10) {
        icd_mapping[[d]] <- paste0(d, "9")
      } else if (nchar(d) >= 4) {
        for (i in seq_len(nchar(d) - 3)) {
          new_d <- substr(d, 1, nchar(d) - i)
          if (new_d %in% thai_icd10) {
            icd_mapping[[d]] <- new_d
            break
          }
        }
      }
    }
  }
  
  cat(sprintf('The modifications led to a total of %d codes being mapped to an equivalent in the Thai ICD10 library.\n', length(icd_mapping)))
  
  icd10_map <- data.table(phl_icd10 = names(icd_mapping), tdrg_icd10 = unlist(icd_mapping))
  
  fwrite(icd10_map, here(path_to_cache, paste0("icd10_map_file_", year_to_load, ".csv")))
  
  icd10_dict <- setNames(icd10_map$tdrg_icd10, icd10_map$phl_icd10)
  
  dt[, clin_c1 := sapply(PRIMARY_ILLNESS, function(x) ifelse(x %in% names(icd10_dict), icd10_dict[x], x))]
  dt[, clin_c2 := sapply(SECONDARY_ILLNESS, function(x) ifelse(x %in% names(icd10_dict), icd10_dict[x], x))]
  
  dt[, icd_list_temp := sapply(strsplit(ICD_CODES, "\\|\\|"), function(codes) {
    mapped <- sapply(codes, function(code) ifelse(code %in% names(icd10_dict), icd10_dict[code], code))
    paste(unique(mapped), collapse = ",")
  })]
  
  dt[, ICD_CODES := icd_list_temp]
  dt[, icd_list_temp := NULL]
  
  return(dt)
}

### Find PDx Code

find_pdx_code <- function(codes, target) {
  #' Find the principal diagnosis (PDx) code.
  #'
  #' @param codes A character vector of codes.
  #' @param target A character string specifying the target code.
  #' @return The principal diagnosis (PDx) code.
  if (length(codes) == 0 || is.na(codes)) return(NA)
  pdx <- target
  target <- str_remove_all(target, "(Case | Not Case | Uncertain )")
  target_available <- target %in% codes
  if (target_available) {
    pdx <- target
  } else {
    pdx <- codes[1]
  }
  return(pdx)
}

### Process Data

process_data <- function(dt, year_to_load, rvs_icd9) {
  #' Process the data.
  #'
  #' @param dt A data.table.
  #' @param year_to_load A character string specifying the year to add.
  #' @param rvs_icd9 A data.table containing RVS to ICD-9 mapping.
  #' @return A processed data.table.
  tic("Processing data")
  
  dt <- keep_necessary_columns(dt)
  dt <- add_year_column(dt, year_to_load)
  dt <- process_patient_type(dt)
  dt <- process_memcat_parent_desc(dt)
  dt <- process_memcat_child_desc(dt)
  dt <- process_disposition(dt)
  dt <- process_icd_codes(dt)
  dt <- process_rvs_codes(dt)
  dt <- remove_lumped_icd_codes(dt)
  dt <- process_rvs_code_mapping(dt, rvs_icd9)
  dt <- process_icd10_mapping(dt)
  
  # Remove periods and apply NA for specified columns
  cols_to_clean <- c("PRIMARY_ILLNESS", "SECONDARY_ILLNESS", "clin_c1", "clin_c2", "clin_c1_pdx", "ICD_CODES", "icd9_list", "rvs_unmap_list", "icd_list")
  dt <- clean_columns(dt, cols_to_clean, na_like_strings)
  
  # Identify Likely Principal Diagnosis (PDx)
  dt[, clin_c1_pdx := mapply(find_pdx_code, strsplit(ICD_CODES, "\\|\\|"), PRIMARY_ILLNESS)]
  
  toc()
  return(dt)
}

### Clean Columns by Removing Periods and Applying NA

clean_columns <- function(dt, cols, na_like_strings) {
  #' Clean columns by removing periods and applying NA.
  #'
  #' @param dt A data.table.
  #' @param cols A character vector specifying the columns to clean.
  #' @param na_like_strings A character vector of strings considered as NA.
  #' @return A data.table with cleaned columns.
  for (col in cols) {
    set(dt, j = col, value = gsub("\\.", "", dt[[col]]))
    set(dt, j = col, value = ifelse(dt[[col]] %in% na_like_strings, NA_character_, dt[[col]]))
  }
  return(dt)
}

### Clean ICD columns

clean_icd_columns <- function(dt) {
  #' Clean ICD columns by removing periods and applying NA.
  #'
  #' @param dt A data.table.
  #' @return A data.table with cleaned ICD columns.
  icd_cols <- grep("^icd_list", colnames(dt), value = TRUE)
  for (col in icd_cols) {
    set(dt, j = col, value = gsub("\\.", "", dt[[col]]))
    set(dt, j = col, value = ifelse(dt[[col]] %in% na_like_strings, NA_character_, dt[[col]]))
  }
  return(dt)
}

### Clean Code List

#' Clean a list of codes by removing periods and applying NA for specified values.
#'
#' @param codes A character vector of codes to be cleaned.
#' @return A character vector with cleaned codes, where periods are removed and specified NA-like values are replaced with NA.
#' @examples
#' codes <- c("A.123", "B.456", "N/A")
#' clean_code_list(codes)
clean_code_list <- function(codes) {
  # Remove periods from the codes
  codes <- gsub("\\.", "", codes)
  
  # Replace specified NA-like values with NA
  codes <- ifelse(codes %in% na_like_strings, NA_character_, codes)
  
  return(codes)
}

### Clean up ICD List Columns

clean_icd_list_columns <- function(dt) {
  #' Clean up ICD list columns.
  #'
  #' @param dt A data.table.
  #' @return A data.table with cleaned ICD list columns.
  icd_list_cols <- grep("^icd_list", colnames(dt), value = TRUE)
  for (col in icd_list_cols) {
    set(dt, j = col, value = gsub("\\.", "", dt[[col]]))
    set(dt, j = col, value = ifelse(dt[[col]] %in% na_like_strings, NA_character_, dt[[col]]))
  }
  return(dt)
}

### Clean up ICD and RVS Codes

clean_icd_and_rvs_codes <- function(dt) {
  #' Clean up ICD and RVS codes.
  #'
  #' @param dt A data.table.
  #' @return A data.table with cleaned ICD and RVS codes.
  dt[, icd9_list := sapply(icd9_list, clean_code_list)]
  dt[, icd_list_1 := sapply(icd_list_1, clean_code_list)]
  return(dt)
}

### Find the Likely Primary Diagnosis (PDx)

find_pdx <- function(clin_c1, clin_c2, icd_list, acc_pdx) {
  #' Find the likely primary diagnosis (PDx).
  #'
  #' @param clin_c1 A character string of clin_c1.
  #' @param clin_c2 A character string of clin_c2.
  #' @param icd_list A character vector of icd_list.
  #' @param acc_pdx A character vector of accepted PDx codes.
  #' @return A list containing the PDx and PDx code.
  if (is.null(clin_c1) || is.na(clin_c1)) clin_c1 <- ""
  if (is.null(clin_c2) || is.na(clin_c2)) clin_c2 <- ""
  
  if (clin_c1 %in% acc_pdx) {
    return(list(pdx = clin_c1, pdx_code = 1))
  }
  if (clin_c2 %in% acc_pdx) {
    return(list(pdx = clin_c2, pdx_code = 2))
  }
  
  pdxs <- unique(icd_list)
  pdxs <- intersect(pdxs, acc_pdx)
  
  if (length(pdxs) == 0) {
    return(list(pdx = NA, pdx_code = 99))
  } else if (length(pdxs) == 1) {
    return(list(pdx = pdxs[1], pdx_code = 3))
  } else {
    starting_letters <- sapply(pdxs, substr, start = 1, stop = 1)
    clin_c1_letter <- substr(clin_c1, 1, 1)
    clin_c2_letter <- substr(clin_c2, 1, 1)
    similar_pdxs <- pdxs[starting_letters %in% c(clin_c1_letter, clin_c2_letter)]
    
    if (length(similar_pdxs) == 1) {
      return(list(pdx = similar_pdxs[1], pdx_code = 4))
    } else if (length(similar_pdxs) > 1) {
      most_similar_pdx <- similar_pdxs[which.max(sapply(similar_pdxs, function(x) sum(strsplit(x, NULL)[[1]] == strsplit(clin_c1, NULL)[[1]])))]
      return(list(pdx = most_similar_pdx, pdx_code = 5))
    } else {
      return(list(pdx = sample(pdxs, 1), pdx_code = 6))
    }
  }
}

### Prepare Data

prepare_data <- function(dt) {
  #' Prepare data for processing.
  #'
  #' @param dt A data.table.
  #' @return A prepared data.table.
  dt[, icd_list := strsplit(ICD_CODES, "\\|\\|")]
  dt[, icd_list := lapply(icd_list, unique)]
  dt[, icd_list := lapply(icd_list, function(x) setdiff(x, c(clin_c1, clin_c2)))]
  
  # Find the maximum length of icd_list elements
  max_length <- max(sapply(dt$icd_list, length))
  
  # Ensure all icd_list elements have the same length by padding with NA
  dt[, icd_list := lapply(icd_list, function(x) {
    length(x) <- max_length
    return(x)
  })]
  
  # Split the icd_list into separate columns
  icd_cols <- dt[, tstrsplit(sapply(icd_list, function(x) paste(x, collapse = ",")), ",", fixed=TRUE, type.convert=FALSE)]
  setnames(icd_cols, paste0("icd_list_", seq_along(icd_cols)))
  
  dt <- cbind(dt, icd_cols)
  return(dt)
}


### Apply the PDx Finding Process

apply_find_pdx <- function(dt, acc_pdx) {
  #' Apply the PDx finding process to the data.
  #'
  #' @param dt A data.table.
  #' @param acc_pdx A character vector of accepted PDx codes.
  #' @return A data.table with the PDx finding process applied.
  pdx_data <- mapply(find_pdx, dt$clin_c1, dt$clin_c2, dt$icd_list, MoreArgs = list(acc_pdx = acc_pdx), SIMPLIFY = FALSE)
  pdx_data <- do.call(rbind, pdx_data)
  dt[, `:=`(pdx = pdx_data[, "pdx"], pdx_code = pdx_data[, "pdx_code"])]
  return(dt)
}


### Main Function to Process Data for PDx

process_data_for_pdx <- function(dt, rvs_icd9, path_to_cache, year_to_load) {
  #' Process data for likely PDx.
  #'
  #' @param dt A data.table.
  #' @param rvs_icd9 A data.table containing RVS to ICD-9 mapping.
  #' @param path_to_cache A character string specifying the path to cache.
  #' @param year_to_load A character string specifying the year to load.
  #' @return A processed data.table.
  dt <- prepare_data(dt)
  dt <- apply_find_pdx(dt, acc_pdx)
  dt <- clean_icd_columns(dt)
  dt <- clean_icd_and_rvs_codes(dt)
  
  saveRDS(dt, here(path_to_cache, paste0("claims_prepdx_", year_to_load, ".rds")))
  return(dt)
}

### Export for Batch Grouper Software

export_for_batch_grouper <- function(dt, year_to_load, output_txt_file) {
  #' Export data for batch grouper software.
  #'
  #' @param dt A data.table.
  #' @param year_to_load A character string specifying the year to load.
  #' @param output_txt_file A character string specifying the output text file path.
  # Prepare the data for the text file
  output_dt <- data.table(CASEID = 1:nrow(dt))
  output_dt[, DOB := generate_dob_vectorized(dt$PAT_BDAY, dt$PATAGE, dt$DATE_ADM)]
  output_dt[, Sex := ifelse(dt$PATSEX == "M", 1, 2)]
  output_dt[, DateAdm := format(mdy(dt$DATE_ADM), "%d/%m/%Y")]
  output_dt[, TimeAdm := gsub(":", "", dt$TIME_ADM)]
  output_dt[, DateDsc := format(mdy(dt$DATE_DIS), "%d/%m/%Y")]
  output_dt[, TimeDsc := gsub(":", "", dt$TIME_DIS)]
  output_dt[, DischT := dt$DISPOSITION]
  output_dt[, AdmWt := dt$PAT_BWT_KG]
  output_dt[, PDx := dt$pdx]
  
  # Split icd_list_1 into multiple columns, ensuring 12 elements per row
  split_icd_codes <- function(icd_str) {
    codes <- unlist(strsplit(icd_str, ","))
    length(codes) <- 12  # Ensuring 12 elements, with NA for missing
    codes
  }
  
  icd_codes_list <- lapply(dt$icd_list_1, split_icd_codes)
  icd_codes <- do.call(rbind, icd_codes_list)
  icd_codes <- as.data.table(icd_codes)
  icd_cols <- paste0("SDx", 1:12)
  output_dt[, (icd_cols) := icd_codes]
  
  # Split icd9_list into multiple columns, ensuring 20 elements per row
  split_rvs_codes <- function(rvs_str) {
    codes <- unlist(strsplit(rvs_str, ","))
    length(codes) <- 20  # Ensuring 20 elements, with NA for missing
    codes
  }
  
  rvs_codes_list <- lapply(dt$icd9_list, split_rvs_codes)
  rvs_codes <- do.call(rbind, rvs_codes_list)
  rvs_codes <- as.data.table(rvs_codes)
  proc_cols <- paste0("Proc", 1:20)
  output_dt[, (proc_cols) := rvs_codes]
  
  # Replace NA values with '--'
  output_dt[is.na(output_dt)] <- '--'
  
  # Convert list columns to character if any
  for (col in names(output_dt)) {
    if (is.list(output_dt[[col]])) {
      output_dt[[col]] <- sapply(output_dt[[col]], paste, collapse = ",")
    }
  }
  
  # Write the data to a text file
  fwrite(output_dt, output_txt_file, sep = "|", col.names = TRUE)
}

### Generate DOB from age and admission date

generate_dob_vectorized <- function(bdays, ages, date_adms) {
  #' Generate DOB from age and admission date.
  #'
  #' @param bdays A character vector of birth dates.
  #' @param ages A numeric vector of ages.
  #' @param date_adms A character vector of admission dates.
  #' @return A character vector of generated DOBs.
  dob <- rep(NA_character_, length(ages))  # Initialize dob vector
  dob[!is.na(bdays) & bdays != ""] <- format(mdy(bdays[!is.na(bdays) & bdays != ""]), "%d/%m/%Y")  # Use PAT_BDAY where available
  
  # Indices where PAT_BDAY is not available
  missing_bday_indices <- which(is.na(bdays) | bdays == "")
  
  # Use age and date_adm to generate DOB for those with missing PAT_BDAY
  ref_dates <- mdy(date_adms[missing_bday_indices])
  
  # Case 1: age == 0
  zero_age_indices <- which(!is.na(ages[missing_bday_indices]) & ages[missing_bday_indices] == 0)
  dob[missing_bday_indices[zero_age_indices]] <- format(ref_dates[zero_age_indices] - days(sample(1:27, length(zero_age_indices), replace = TRUE)), "%d/%m/%Y")
  
  # Case 2: age > 0
  positive_age_indices <- which(!is.na(ages[missing_bday_indices]) & ages[missing_bday_indices] > 0)
  truncated_ages <- floor(ages[missing_bday_indices][positive_age_indices])
  dob[missing_bday_indices[positive_age_indices]] <- format(ref_dates[positive_age_indices] - years(truncated_ages) - days(sample(1:170, length(positive_age_indices), replace = TRUE)), "%d/%m/%Y")
  
  return(dob)
}

### Clean Code List

clean_code_list <- function(codes) {
  #' Clean a list of codes by removing periods and applying NA for specified values.
  #'
  #' @param codes A character vector of codes to be cleaned.
  #' @return A character vector with cleaned codes, where periods are removed and specified NA-like values are replaced with NA.
  codes <- gsub("\\.", "", codes)
  codes <- ifelse(codes %in% na_like_strings, NA_character_, codes)
  return(codes)
}