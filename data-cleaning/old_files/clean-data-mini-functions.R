source(here("data-cleaning", "r_scripts", "libraries.R"))

# Define individual task functions
clean_data_add_year_column <- function(dt) {
    dt[, SRC_YR := as.integer(year_to_load)]
}

clean_data_rename_columns <- function(dt) {
    setnames(dt, old = old_colnames, new = new_colnames)
    if (!all(new_colnames %in% colnames(dt))) {
        missing_cols <- setdiff(new_colnames, colnames(dt))
        warning("Failed to rename the following columns: ", paste(missing_cols, collapse = ", "))
        stop("Column renaming failed.")
    }
    if (to_view_checks) {
        print("Successfully renamed columns; All expected columns exist")
    }
}

clean_data_collapse_columns_icd <- function(dt) {
    dt[, clin_icd := collapse_columns(
        mget(paste0("clin_icd", 1:12), envir = as.environment(dt)),
        na_like_strings
    )]
    dt[, paste0("clin_icd", 1:12) := NULL]
}

clean_data_collapse_columns_rvs <- function(dt) {
    dt[, clin_rvs := collapse_columns(
        mget(paste0("clin_rvs", 1:20), envir = as.environment(dt)),
        na_like_strings
    )]
    dt[, paste0("clin_rvs", 1:20) := NULL]
}

clean_data_remove_lumped_icd_codes_func <- function(dt) {
    dt[, clin_icd := remove_lumped_icd_codes(dt$clin_icd)]
}

clean_data_turn_to_lists <- function(dt) {
    dt[, clin_icd := split_to_vector(clin_icd)]
    dt[, clin_rvs := split_to_vector(clin_rvs)]
}

clean_data_clean_unlump_clin_c1_c2 <- function(dt) {
    dt[, clin_c1_orig := clin_c1]
    dt[, clin_c1 := clean_column(dt$clin_c1, na_like_strings)]
    clin_c1_cleaning_comparison <- dt[clin_c1 != clin_c1_orig, .(clin_c1_orig, clin_c1)]
    if (to_view_checks) {
        print(head(clin_c1_cleaning_comparison)) # Check: Print head of changes
    }
    dt[, clin_c1_orig := NULL]

    dt[, clin_c2_orig := clin_c2]
    dt[, clin_c2 := clean_column(dt$clin_c2, na_like_strings)]
    clin_c2_cleaning_comparison <- dt[clin_c2 != clin_c2_orig, .(clin_c2_orig, clin_c2)]
    if (to_view_checks) {
        print(head(clin_c2_cleaning_comparison)) # Check: Print head of changes
    }
    dt[, clin_c2_orig := NULL]

    dt[, clin_c1 := remove_lumped_icd_codes(dt$clin_c1)]
    dt[, clin_c2 := remove_lumped_icd_codes(dt$clin_c2)]

    dt[, clin_c1 := split_to_vector(clin_c1)]
    clin_c1_result <- transfer_extra_icd10s_to_clin_icd(dt$clin_icd, dt$clin_c1)
    dt[, clin_icd := clin_c1_result$clin_icd]
    dt[, clin_c1 := clin_c1_result$col_first]

    dt[, clin_c2 := split_to_vector(clin_c2)]
    clin_c2_result <- transfer_extra_icd10s_to_clin_icd(dt$clin_icd, dt$clin_c2)
    dt[, clin_icd := clin_c2_result$clin_icd]
    dt[, clin_c2 := clin_c2_result$col_first]
}

clean_data_append_remove_rvs_func <- function(dt) {
    clin_c1_rvs_results <- append_and_remove_rvs(dt$clin_rvs, dt$clin_c1, rvs_icd9)
    dt[, clin_rvs := clin_c1_rvs_results$clin_rvs]
    dt[, clin_c1 := clin_c1_rvs_results$col]

    clin_c2_rvs_results <- append_and_remove_rvs(dt$clin_rvs, dt$clin_c2, rvs_icd9)
    dt[, clin_rvs := clin_c2_rvs_results$clin_rvs]
    dt[, clin_c2 := clin_c2_rvs_results$col]

    dt[, clin_rvs := lapply(clin_rvs, unique)]
}

clean_data_dedup_icd_codes <- function(dt) {
    dedup_result <- ensure_unique_icd_codes(dt$clin_c1, dt$clin_c2, dt$clin_icd)
    dt[, clin_c1 := dedup_result$clin_c1]
    dt[, clin_c2 := dedup_result$clin_c2]
    dt[, clin_icd := dedup_result$clin_icd]
}

clean_data_replace_empty_with_na_func <- function(dt) {
    dt <- replace_empty_with_na(dt, to_view_checks)
}

clean_data_remap_columns <- function(dt) {
    warning_thrown <- FALSE

    # Remap and check for patient type
    result <- remap_patient_type(dt$pat_type)
    dt$pat_type <- result$remapped
    if (length(result$unmapped) > 0 && to_view_checks) {
        warning_thrown <- TRUE
        print("Unmapped Patient Types:")
        print(result$unmapped)
    }
    if (warning_thrown && to_view_checks) {
        print("Patient Types:")
        print(unique(dt$pat_type))
    }

    warning_thrown <- FALSE

    # Remap and check for member category parent
    result <- remap_memcat_parent_desc(dt$pat_memcat_parent)
    dt$pat_memcat_parent <- result$remapped
    if (length(result$unmapped) > 0 && to_view_checks) {
        warning_thrown <- TRUE
        print("Unmapped Memcat Parent Types:")
        print(result$unmapped)
    }
    if (warning_thrown && to_view_checks) {
        print("Memcat Parent Types:")
        print(unique(dt$pat_memcat_parent))
    }

    warning_thrown <- FALSE

    # Remap and check for member category child
    result <- remap_memcat_child_desc(dt$pat_memcat_child)
    dt$pat_memcat_child <- result$remapped
    if (length(result$unmapped) > 0 && to_view_checks) {
        warning_thrown <- TRUE
        print("Unmapped Memcat Child Types:")
        print(result$unmapped)
    }
    if (warning_thrown && to_view_checks) {
        print("Memcat Child Types:")
        print(unique(dt$pat_memcat_child))
    }

    warning_thrown <- FALSE

    # Remap and check for clinical discharge disposition
    result <- remap_disposition(dt$clin_discharge)
    dt$clin_discharge <- result$remapped
    if (length(result$unmapped) > 0 && to_view_checks) {
        warning_thrown <- TRUE
        print("Unmapped Discharge Types:")
        print(result$unmapped)
    }
    if (warning_thrown && to_view_checks) {
        print("Discharge Types:")
        print(unique(dt$clin_discharge))
    }
}
