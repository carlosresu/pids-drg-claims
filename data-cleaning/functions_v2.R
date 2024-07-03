### Read Entire File

read_entire_file <- function(drop_cols) {
  dt <- fread(full_claims, na.strings = na_values, drop = drop_cols, colClasses = col_classes)
  return(dt)
}

### Read Sampled File

read_sampled_file <- function() {
  dt <- fread(sampled_claims, na.strings = na_values, colClasses = col_classes)
  return(dt)
}

### Sample Data

sample_data <- function(dt) {
  dt <- dt[sample(.N, min(sample_size, .N))]
  return(dt)
}

### Write Data

write_data <- function(dt, path) {
  fwrite(dt, path)
}

### Add Year Column

add_year_column <- function(dt, year_to_load) {
  dt[, SRC_YR := as.integer(year_to_load)]
  return(dt)
}

### Rename Columns

rename_columns <- function(dt) {
  setnames(dt, old = old_colnames, new = new_colnames)
  return(dt)
}

clean_columns <- function(cols) {
  cols <- lapply(cols, function(col) {
    col <- iconv(col, to = "UTF-8", sub = "byte")
    col <- toupper(col)
    col <- stri_trim_both(col)
    col <- stri_replace_all_regex(col, " ", "")
    col <- stri_replace_all_regex(col, "\n", "")
    col <- stri_replace_all_regex(col, "[^\\w\\d\\/\\s]+", "")
    col <- ifelse(col %in% na_like_strings, NA_character_, col)
    return(col)
  })
  return(cols)
}

clean_columns_in_dt <- function(dt, cols_to_clean) {
  dt[, (cols_to_clean) := clean_columns(.SD), .SDcols = cols_to_clean]
  return(dt)
}

### Process and Collapse Columns

process_and_collapse_columns <- function(dt, cols_to_process, new_col_name) {
  dt[, (cols_to_process) := clean_columns(.SD), .SDcols = cols_to_process]
  
  dt[, (new_col_name) := do.call(paste, c(.SD, sep = "||")), .SDcols = cols_to_process]
  dt[, (new_col_name) := stri_replace_all_regex(get(new_col_name), "\\|\\|NA", "")]
  dt[, (new_col_name) := stri_replace_all_regex(get(new_col_name), "NA\\|\\|", "")]
  dt[, (new_col_name) := stri_replace_all_regex(get(new_col_name), "\\|\\|$", "")]
  dt[, (new_col_name) := ifelse(get(new_col_name) %in% na_like_strings, NA_character_, get(new_col_name))]
  
  dt[, (cols_to_process) := NULL]
}

### Find Lumped Codes

find_lumped_codes <- function(codes) {
  sapply(codes, function(code) {
    if (is.na(code)) {
      return(FALSE)
    }
    nchar(code) > 4 &&
      str_count(code, "[A-Za-z]") > 1 &&
      str_count(code, "[0-9]") > 1
  })
}

### Replace "NA" or empty strings with NA_character_

replace_NA_as_char <- function(result) {
  result[result == "NA" | result == ""] <- NA_character_
  return(result)
}

### Remove Lumped ICD Codes
remove_lumped_icd_codes <- function(dt, column) {
  dt[, (column) := {
    # Check if the cell contains "||"
    needs_processing <- grepl("\\|\\|", get(column), fixed = TRUE)
    
    if (any(needs_processing)) {
      codes_list <- strsplit(get(column)[needs_processing], "\\|\\|", fixed = TRUE)
      
      cleaned_codes_list <- lapply(codes_list, function(codes) {
        if (is.null(codes) || all(is.na(codes))) return(NA_character_)
        
        # Detect lumped codes in a vectorized manner
        lumped_mask <- !is.na(codes) & nchar(codes) > 4 & str_count(codes, "[A-Za-z]") > 1 & str_count(codes, "[0-9]") > 1
        
        if (any(lumped_mask, na.rm = TRUE)) {
          # Efficiently split lumped codes
          split_codes <- unlist(lapply(codes[lumped_mask], function(code) {
            unlist(strsplit(code, "(?<=\\d)(?=[A-Za-z])", perl = TRUE))
          }))
          codes <- c(codes[!lumped_mask], split_codes)
        }
        
        # Clean and remove empty codes
        codes <- gsub("[^A-Za-z0-9/\\|]", "", codes)
        codes <- codes[codes != ""]
        result <- paste(codes, collapse = "||")
        
        replace_NA_as_char(result)
      })
      
      cleaned_codes <- unlist(cleaned_codes_list)
      new_values <- get(column)
      new_values[needs_processing] <- cleaned_codes
      new_values
    } else {
      get(column)
    }
  }]
  return(dt)
}


### Replace empty strings in character and factor columns with NA_character_

replace_empty_with_na <- function(dt) {
  # Identify character and factor columns
  char_factor_cols <- names(dt)[sapply(dt, function(col) is.character(col) || is.factor(col))]
  
  # Apply the replacement
  dt[, (char_factor_cols) := lapply(.SD, function(x) {
    x[x == "" | x == "NA"] <- NA
    if (is.factor(x)) {
      levels(x) <- c(levels(x), NA)
    }
    return(x)
  }), .SDcols = char_factor_cols]
  
  return(dt)
}

# Define a function to split strings by "||" and handle NA values
split_to_vector <- function(column) {
  # Split the column using strsplit and filter out NAs directly
  result <- lapply(column, function(x) {
    if (is.na(x)) {
      return(NA_character_)
    } else {
      return(unlist(strsplit(x, "||", fixed = TRUE)))
    }
  })
  return(result)
}

# process_icd10_codes <- function(dt, col) {
#   #split col (a string delimited by "||") into a vector
#   dt[, col := split_to_vector(col)]
#   #loop through all cells of col
#   for (i in seq(1:nrow(dt))){
#     #while length of col is greater than 1
#     while (length(col[i])>1){
#       #append last element of col vector to clin_icd col of the same row
#       clin_icd <- c(clin_icd, col[i][length(col[i])])
#       #delete last element of col vector
#       col[i] <- col[i][-length(col[i])]
#     }
#   }
# }

process_icd10_codes <- function(dt, col) {
  # Ensure clin_icd is initialized if it's empty
  dt[, clin_icd := lapply(clin_icd, function(x) if (is.null(x)) character() else x)]
  
  # Process each row where the length of col is more than 1
  dt[lengths(get(col)) > 1, `:=` (
    # Append all but the first element of col to clin_icd
    clin_icd = mapply(function(icd, c1) c(icd, c1[-1]), clin_icd, get(col), SIMPLIFY = FALSE),
    # Retain only the first element in col
    col = lapply(get(col), function(x) x[1])
  )]
  
  # Copy non-null values from col to clin_c1
  dt[col != "NULL", clin_c1 := col]
  
  # Remove the temporary col column
  dt[, col := NULL]
  
  # Return the modified data table
  return(dt)
}

