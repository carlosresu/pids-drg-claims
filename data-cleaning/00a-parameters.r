thread_offset <- 0

sample_size_divisor <- 25

# Whether to sample each split_part by sample_size_divisor
# (useful when iterating through code runs in quick succession)
to_sample <- FALSE
# TODO: Add description here
to_write <- TRUE
# TODO: Add description here
to_flush <- FALSE
# TODO: Add description here
to_bq <- TRUE
# TODO: Add description here
to_post_cleaning_checks <- FALSE
# TODO: Add description here
to_parallel <- TRUE
cat("Parallelization:", to_parallel, "\n")
# TODO: Add description here
to_debug <- FALSE
verbose_output <- if (to_debug) TRUE else FALSE

to_generate_subset <- TRUE

to_py_prompt <- TRUE
to_python <- TRUE
to_generate_py_fwrite <- TRUE
to_generate_feather <- TRUE
to_py_bq <- TRUE

to_thai_prompt <- TRUE
to_thai <- TRUE
to_thai_bq <- TRUE
to_generate_thai_txt <- TRUE
to_thai_all_years <- FALSE

to_spc <- FALSE
