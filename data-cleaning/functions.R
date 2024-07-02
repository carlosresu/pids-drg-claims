### Read Entire File

read_entire_file <- function() {
  tic("Reading entire data file")
  #' Read the entire data file.
  #'
  #' @return A data.table containing the entire data file.
  df <- fread(full_claims, na.strings = na_values)
  toc()
  return(df)
}

### Read Sampled File

read_sampled_file <- function() {
  #' Read the sampled data file.
  #'
  #' @return A data.table containing the sampled data file.
  df <- fread(sampled_claims, na.strings = na_values)
  return(df)
}

### Sample Data

sample_data <- function(df) {
  #' Sample data from the data table.
  #'
  #' @param df A data.table to sample from.
  #' @return A sampled data.table.
  df <- df[sample(.N, min(sample_size, .N))]
  return(df)
}

### Write Data

write_data <- function(df, path) {
  #' Write data to a specified path.
  #'
  #' @param df A data.table to write.
  #' @param path A character string specifying the path to write the data to.
  fwrite(df, path)
}

### Keep Necessary Columns

keep_necessary_columns <- function(df) {
  #' Keep necessary columns in the data table.
  #'
  #' @param df A data.table.
  #' @return A data.table with only the necessary columns.
  cols_to_keep <- setdiff(1:ncol(df), drop_cols)
  df <- df[, ..cols_to_keep]
  return(df)
}

### Add Year Column

add_year_column <- function(df, year_to_load) {
  #' Add a year column to the data table.
  #'
  #' @param df A data.table.
  #' @param year_to_load A character string specifying the year to add.
  #' @return A data.table with the year column added.
  df[, SRC_YR := as.integer(year_to_load)]
  return(df)
}

### Process and Collapse Columns

process_and_collapse_columns <- function(df, cols_to_process, new_col_name) {
  #' Process and collapse specified columns into a new column.
  #'
  #' @param df A data.table.
  #' @param cols_to_process A character vector specifying columns to process.
  #' @param new_col_name A character string specifying the name of the new column.
  for (col in cols_to_process) {
    set(df, j = col, value = iconv(df[[col]], to = "UTF-8", sub = "byte"))
    set(df, j = col, value = toupper(df[[col]]))
    set(df, j = col, value = str_trim(df[[col]]))
    set(df, j = col, value = str_replace_all(df[[col]], " ", ""))
    set(df, j = col, value = str_replace_all(df[[col]], "\n", ""))
    set(df, j = col, value = str_replace_all(df[[col]], "[^\\w\\d\\/\\s]+", ""))
    set(df, j = col, value = ifelse(df[[col]] %in% na_like_strings, NA_character_, df[[col]]))
  }
  
  df[, (new_col_name) := do.call(paste, c(.SD, sep = "||")), .SDcols = cols_to_process]
  df[, (new_col_name) := str_replace_all(get(new_col_name), "\\|\\|NA", "")]
  df[, (new_col_name) := str_replace_all(get(new_col_name), "NA\\|\\|", "")]
  df[, (new_col_name) := str_replace_all(get(new_col_name), "\\|\\|$", "")]
  df[, (new_col_name) := ifelse(get(new_col_name) == "NA", NA_character_, get(new_col_name))]
  
  df[, (cols_to_process) := NULL]
}

### Process Patient Type

process_patient_type <- function(df) {
  #' Process patient type.
  #'
  #' @param df A data.table.
  #' @return A data.table with processed patient type.
  df[, PATIENT_TYPE := fcase(
    PATIENT_TYPE == "MEMBER", "MEM",
    PATIENT_TYPE == "DEPENDENT", "DEP"
  )]
  return(df)
}

### Process Memcat Parent Description

process_memcat_parent_desc <- function(df) {
  #' Process memcat parent description.
  #'
  #' @param df A data.table.
  #' @return A data.table with processed memcat parent description.
  df[, MEMCAT_PARENT_DESC := fcase(
    MEMCAT_PARENT_DESC == "DIRECT CONTRIBUTOR", "DIRECT",
    MEMCAT_PARENT_DESC == "INDIRECT CONTRIBUTOR", "INDIRECT"
  )]
  return(df)
}

### Process Memcat Child Description

process_memcat_child_desc <- function(df) {
  #' Process memcat child description.
  #'
  #' @param df A data.table.
  #' @return A data.table with processed memcat child description.
  df[, MEMCAT_CHILD_DESC := fcase(
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
  return(df)
}

### Process Disposition

process_disposition <- function(df) {
  #' Process disposition.
  #'
  #' @param df A data.table.
  #' @return A data.table with processed disposition.
  df[, DISPOSITION := fcase(
    DISPOSITION == "IMPROVED", 1L,
    DISPOSITION == "RECOVERED", 1L,
    DISPOSITION == "HOME/DISCHARGED AGAINST MEDICAL ADVICE", 2L,
    DISPOSITION == "ABSCONDED", 3L,
    DISPOSITION == "TRANSFERRED/REFERRED", 4L,
    DISPOSITION == "EXPIRED", 9L,
    DISPOSITION == "UNDEFINED", NA_integer_
  )]
  return(df)
}

### Process ICD Codes

process_icd_codes <- function(df) {
  #' Process ICD codes.
  #'
  #' @param df A data.table.
  #' @return A data.table with processed ICD codes.
  icd_cols <- intersect(colnames(df), c(paste0("ICDCODE", c(1:14, 16:170)), "ICCODED15"))
  process_and_collapse_columns(df, icd_cols, "ICD_CODES")
  return(df)
}

### Process RVS Codes

process_rvs_codes <- function(df) {
  #' Process RVS codes.
  #'
  #' @param df A data.table.
  #' @return A data.table with processed RVS codes.
  rvs_cols <- intersect(colnames(df), paste0("RVSCODE", 1:20))
  process_and_collapse_columns(df, rvs_cols, "RVS_CODES")
  return(df)
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

remove_lumped_icd_codes <- function(df) {
  #' Remove lumped ICD codes.
  #'
  #' @param df A data.table.
  #' @return A data.table with lumped ICD codes removed.
  to_delist <- unique(unlist(strsplit(df$ICD_CODES, "\\|\\|")))[find_lumped_codes(unique(unlist(strsplit(df$ICD_CODES, "\\|\\|"))))]
  
  if (length(to_delist) > 0 && !all(is.na(to_delist))) {
    delist_pattern <- paste(to_delist[!is.na(to_delist)], collapse = "|")
  } else {
    delist_pattern <- ""
  }
  
  if (delist_pattern != "") {
    delist_mask <- str_detect(df$ICD_CODES, delist_pattern)
    df[delist_mask, ICD_CODES := sapply(ICD_CODES, function(x) {
      paste(setdiff(unlist(strsplit(x, "\\|\\|")), to_delist), collapse = "||")
    })]
  }
  return(df)
}

### Process RVS Code Mapping

process_rvs_code_mapping <- function(df, rvs_icd9) {
  #' Process RVS code mapping.
  #'
  #' @param df A data.table.
  #' @param rvs_icd9 A data.frame containing RVS to ICD-9 mapping.
  #' @return A data.table with processed RVS code mapping.
  with_drg <- rvs_icd9 %>%
    filter(is_drg)
  
  without_drg <- rvs_icd9 %>%
    filter(!rvs %in% with_drg$rvs)
  
  cat(sprintf('There are %d RVS codes without an ICD-9CM equivalent recognized by the TDRG ICD9CM\n', without_drg$rvs %>% n_distinct()))
  
  rvs_map_list <- list()
  rvs_map_solo <- list()
  
  with_drg <- with_drg %>%
    arrange(rvs, desc(is_drg))
  
  for (i in seq_len(nrow(with_drg))) {
    row <- with_drg[i, ]
    r <- row$rvs
    sub <- with_drg %>%
      filter(rvs == r)
    if (nrow(sub) == 1) {
      rvs_map_solo[[r]] <- sub$icd9cm[1]
    } else {
      rvs_map_list[[r]] <- sub$icd9cm
    }
  }
  
  df$RVS_CODES <- strsplit(as.character(df$RVS_CODES), "\\|\\|")
  
  rvss <- unique(unlist(df$RVS_CODES)) %>%
    intersect(acr_rvs$rvs)
  
  cat(sprintf('There are %d unique RVS codes that appear in the claims.\n', length(rvss)))
  
  mappable_rvs <- intersect(rvss, rvs_icd9$rvs)
  cat(sprintf('Of these, %d (%.2f%%) have a mapping to an ICD-9-CM code.\n', 
              length(mappable_rvs), length(mappable_rvs) * 100 / length(rvss)))
  
  multi_mapped_rvs <- intersect(rvss, names(rvs_map_list))
  cat(sprintf('Of these, there are %d (%.2f%%) with more than one ICD9 equivalent recognized by the Thai ICD9 library.\n', 
              length(multi_mapped_rvs), length(multi_mapped_rvs) * 100 / length(mappable_rvs)))
  
  unmappable_rvs <- setdiff(rvss, mappable_rvs)
  cat(sprintf('There are %d (%.2f%%) with no ICD-9-CM equivalents.\n',
              length(unmappable_rvs), length(unmappable_rvs) * 100 / length(rvss)))
  
  df2 <- df[lengths(df$RVS_CODES) > 0, ]
  
  df2_info <- df2 %>% as.data.table() %>% .[, .N, by = PSEUDO_CLAIMSERIES]
  
  df2 <- df2 %>% unnest(RVS_CODES)
  
  df2 <- df2 %>%
    mutate(icd9_list = map_chr(RVS_CODES, ~ifelse(. %in% names(rvs_map_solo), rvs_map_solo[[.]], 
                                                  ifelse(. %in% names(rvs_map_list), toString(rvs_map_list[[.]]), NA))))
  
  df2 <- df2 %>%
    mutate(rvs_unmap_list = map_chr(RVS_CODES, ~ifelse(. %in% names(rvs_map_solo) | . %in% names(rvs_map_list), NA, .)))
  
  df2 <- df2 %>%
    group_by(PSEUDO_CLAIMSERIES) %>%
    summarise(icd9_list = toString(unique(unlist(strsplit(icd9_list, ",")))),
              rvs_unmap_list = toString(unique(unlist(strsplit(rvs_unmap_list, ",")))))
  
  df <- df %>%
    left_join(df2, by = "PSEUDO_CLAIMSERIES")
  
  rm(df2)
  gc()
  return(df)
}

### Process ICD10 Mapping

process_icd10_mapping <- function(df) {
  #' Process ICD10 mapping.
  #'
  #' @param df A data.table.
  #' @return A data.table with processed ICD10 mapping.
  tdrg_icd10 <- read_csv(here(path_to_aux, "i10.csv"))
  
  df$ICD_CODES <- as.character(df$ICD_CODES)
  
  icds <- unique(c(unlist(strsplit(df$PRIMARY_ILLNESS, ",")), 
                   unlist(strsplit(df$SECONDARY_ILLNESS, ",")), 
                   unlist(strsplit(df$ICD_CODES, "\\|\\|"))))
  
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
  
  icd10_map <- data.frame(phl_icd10 = names(icd_mapping), tdrg_icd10 = unlist(icd_mapping), stringsAsFactors = FALSE)
  
  write_csv(icd10_map, here(path_to_cache, paste0("icd10_map_file_", year_to_load, ".csv")))
  
  icd10_dict <- setNames(icd10_map$tdrg_icd10, icd10_map$phl_icd10)
  
  df <- df %>%
    mutate(clin_c1 = map_chr(PRIMARY_ILLNESS, ~ifelse(. %in% names(icd10_dict), icd10_dict[.], .)),
           clin_c2 = map_chr(SECONDARY_ILLNESS, ~ifelse(. %in% names(icd10_dict), icd10_dict[.], .)))
  
  df <- df %>%
    mutate(icd_list_temp = strsplit(ICD_CODES, "\\|\\|"),
           icd_list_temp = map(icd_list_temp, ~sapply(., function(x) ifelse(x %in% names(icd10_dict), icd10_dict[x], x))),
           icd_list_temp = map_chr(icd_list_temp, ~paste(unique(.), collapse = ",")))
  
  df <- df %>%
    mutate(ICD_CODES = icd_list_temp) %>%
    select(-icd_list_temp)
  
  return(df)
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

process_data <- function(df, year_to_load, rvs_icd9) {
  #' Process the data.
  #'
  #' @param df A data.table.
  #' @param year_to_load A character string specifying the year to add.
  #' @param rvs_icd9 A data.frame containing RVS to ICD-9 mapping.
  #' @return A processed data.table.
  tic("Processing data")
  
  df <- keep_necessary_columns(df)
  df <- add_year_column(df, year_to_load)
  df <- process_patient_type(df)
  df <- process_memcat_parent_desc(df)
  
  
  df <- process_memcat_child_desc(df)
  df <- process_disposition(df)
  df <- process_icd_codes(df)
  df <- process_rvs_codes(df)
  df <- remove_lumped_icd_codes(df)
  df <- process_rvs_code_mapping(df, rvs_icd9)
  df <- process_icd10_mapping(df)
  
  # Remove periods and apply NA for specified columns
  cols_to_clean <- c("PRIMARY_ILLNESS", "SECONDARY_ILLNESS", "clin_c1", "clin_c2", "clin_c1_pdx", "ICD_CODES", "icd9_list", "rvs_unmap_list", "icd_list")
  df <- clean_columns(df, cols_to_clean, na_like_strings)
  
  # Identify Likely Principal Diagnosis (PDx)
  df <- df %>%
    mutate(
      clin_c1_pdx = pmap_chr(list(clin_c1, clin_c2), find_pdx_code)
    )
  
  toc()
  return(df)
}

### Clean Columns by Removing Periods and Applying NA

clean_columns <- function(df, cols, na_like_strings) {
  #' Clean columns by removing periods and applying NA.
  #'
  #' @param df A data.table.
  #' @param cols A character vector specifying the columns to clean.
  #' @param na_like_strings A character vector of strings considered as NA.
  #' @return A data.table with cleaned columns.
  for (col in cols) {
    set(df, j = col, value = gsub("\\.", "", df[[col]]))
    set(df, j = col, value = ifelse(df[[col]] %in% na_like_strings, NA_character_, df[[col]]))
  }
  return(df)
}

### Clean ICD columns

clean_icd_columns <- function(df) {
  #' Clean ICD columns by removing periods and applying NA.
  #'
  #' @param df A data.table.
  #' @return A data.table with cleaned ICD columns.
  icd_cols <- grep("^icd_list", colnames(df), value = TRUE)
  for (col in icd_cols) {
    set(df, j = col, value = gsub("\\.", "", df[[col]]))
    set(df, j = col, value = ifelse(df[[col]] %in% na_like_strings, NA_character_, df[[col]]))
  }
  return(df)
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

clean_icd_list_columns <- function(df) {
  #' Clean up ICD list columns.
  #'
  #' @param df A data.table.
  #' @return A data.table with cleaned ICD list columns.
  icd_list_cols <- grep("^icd_list", colnames(df), value = TRUE)
  for (col in icd_list_cols) {
    set(df, j = col, value = gsub("\\.", "", df[[col]]))
    set(df, j = col, value = ifelse(df[[col]] %in% na_like_strings, NA_character_, df[[col]]))
  }
  return(df)
}

### Clean up ICD and RVS Codes

clean_icd_and_rvs_codes <- function(df) {
  #' Clean up ICD and RVS codes.
  #'
  #' @param df A data.table.
  #' @return A data.table with cleaned ICD and RVS codes.
  df$icd9_list <- sapply(df$icd9_list, clean_code_list)
  df$icd_list_1 <- sapply(df$icd_list_1, clean_code_list)
  return(df)
}

### Find the Likely Primary Diagnosis (PDx)

find_pdx <- function(row, acc_pdx) {
  #' Find the likely primary diagnosis (PDx).
  #'
  #' @param row A data.table row.
  #' @param acc_pdx A character vector of accepted PDx codes.
  #' @return A list containing the PDx and PDx code.
  if (row$clin_c1 %in% acc_pdx) {
    return(list(pdx = row$clin_c1, pdx_code = 1))
  }
  if (row$clin_c2 %in% acc_pdx) {
    return(list(pdx = row$clin_c2, pdx_code = 2))
  }
  
  pdxs <- unlist(row[grep("^icd_list_", names(row))]) %>%
    unique() %>%
    intersect(acc_pdx)
  
  if (length(pdxs) == 0) {
    return(list(pdx = NA, pdx_code = 99))
  } else if (length(pdxs) == 1) {
    return(list(pdx = pdxs[1], pdx_code = 3))
  } else {
    starting_letters <- sapply(pdxs, substr, start = 1, stop = 1)
    clin_c1_letter <- substr(row$clin_c1, 1, 1)
    clin_c2_letter <- substr(row$clin_c2, 1, 1)
    similar_pdxs <- pdxs[starting_letters %in% c(clin_c1_letter, clin_c2_letter)]
    
    if (length(similar_pdxs) == 1) {
      return(list(pdx = similar_pdxs[1], pdx_code = 4))
    } else if (length(similar_pdxs) > 1) {
      most_similar_pdx <- similar_pdxs[which.max(sapply(similar_pdxs, function(x) sum(strsplit(x, NULL)[[1]] == strsplit(row$clin_c1, NULL)[[1]])))]
      return(list(pdx = most_similar_pdx, pdx_code = 5))
    } else {
      return(list(pdx = sample(pdxs, 1), pdx_code = 6))
    }
  }
}

### Prepare Data

prepare_data <- function(df) {
  #' Prepare data for processing.
  #'
  #' @param df A data.table.
  #' @return A prepared data.table.
  df <- df %>%
    mutate(icd_list = strsplit(ICD_CODES, "\\|\\|")) %>%
    rowwise() %>%
    mutate(icd_list = list(unique(icd_list))) %>%
    ungroup() %>%
    rowwise() %>%
    mutate(icd_list = list(setdiff(icd_list, c(clin_c1, clin_c2)))) %>%
    ungroup()
  
  icd_cols <- df %>%
    select(icd_list) %>%
    unnest_wider(icd_list, names_sep = "_", simplify = TRUE)
  
  df <- bind_cols(df, icd_cols)
  return(df)
}

### Apply the PDx Finding Process

apply_find_pdx <- function(df, acc_pdx) {
  #' Apply the PDx finding process to the data.
  #'
  #' @param df A data.table.
  #' @param acc_pdx A character vector of accepted PDx codes.
  #' @return A data.table with the PDx finding process applied.
  df <- df %>%
    rowwise() %>%
    mutate(pdx_data = list(find_pdx(cur_data(), acc_pdx))) %>%
    unnest_wider(pdx_data)
  return(df)
}

### Main Function to Process Data for PDx

process_data_for_pdx <- function(df, rvs_icd9, path_to_cache, year_to_load) {
  #' Process data for likely PDx.
  #'
  #' @param df A data.table.
  #' @param rvs_icd9 A data.frame containing RVS to ICD-9 mapping.
  #' @param path_to_cache A character string specifying the path to cache.
  #' @param year_to_load A character string specifying the year to load.
  #' @return A processed data.table.
  acc_pdx <- acc_pdx
  df <- prepare_data(df)
  df <- apply_find_pdx(df, acc_pdx)
  df <- clean_icd_columns(df)
  df <- clean_icd_and_rvs_codes(df)
  
  saveRDS(df, here(path_to_cache, paste0("claims_prepdx_", year_to_load, ".rds")))
  return(df)
}

### Export for Batch Grouper Software

export_for_batch_grouper <- function(df, year_to_load, output_txt_file) {
  #' Export data for batch grouper software.
  #'
  #' @param df A data.table.
  #' @param year_to_load A character string specifying the year to load.
  #' @param output_txt_file A character string specifying the output text file path.
  # Prepare the data for the text file
  output_df <- data.table(CASEID = 1:nrow(df))
  output_df[, DOB := generate_dob_vectorized(df$PAT_BDAY, df$PATAGE, df$DATE_ADM)]
  output_df[, Sex := ifelse(df$PATSEX == "M", 1, 2)]
  output_df[, DateAdm := format(mdy(df$DATE_ADM), "%d/%m/%Y")]
  output_df[, TimeAdm := gsub(":", "", df$TIME_ADM)]
  output_df[, DateDsc := format(mdy(df$DATE_DIS), "%d/%m/%Y")]
  output_df[, TimeDsc := gsub(":", "", df$TIME_DIS)]
  output_df[, DischT := df$DISPOSITION]
  output_df[, AdmWt := df$PAT_BWT_KG]
  output_df[, PDx := df$pdx]
  
  # Split icd_list_1 into multiple columns, ensuring 12 elements per row
  split_icd_codes <- function(icd_str) {
    codes <- unlist(strsplit(icd_str, ","))
    length(codes) <- 12  # Ensuring 12 elements, with NA for missing
    codes
  }
  
  icd_codes_list <- lapply(df$icd_list_1, split_icd_codes)
  icd_codes <- do.call(rbind, icd_codes_list)
  icd_codes <- as.data.table(icd_codes)
  icd_cols <- paste0("SDx", 1:12)
  output_df[, (icd_cols) := icd_codes]
  
  
  
  # Split icd9_list into multiple columns, ensuring 20 elements per row
  split_rvs_codes <- function(rvs_str) {
    codes <- unlist(strsplit(rvs_str, ","))
    length(codes) <- 20  # Ensuring 20 elements, with NA for missing
    codes
  }
  
  rvs_codes_list <- lapply(df$icd9_list, split_rvs_codes)
  rvs_codes <- do.call(rbind, rvs_codes_list)
  rvs_codes <- as.data.table(rvs_codes)
  proc_cols <- paste0("Proc", 1:20)
  output_df[, (proc_cols) := rvs_codes]
  
  # Replace NA values with '--'
  output_df[is.na(output_df)] <- '--'
  
  # Write the data to a text file
  fwrite(output_df, output_txt_file, sep = "|", col.names = TRUE)
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