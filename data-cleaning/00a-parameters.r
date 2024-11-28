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
to_bq <- FALSE
# TODO: Add description here
to_post_cleaning_checks <- FALSE
# TODO: Add description here
to_parallel <- TRUE
cat("Parallelization:", to_parallel, "\n")
# TODO: Add description here
to_debug <- FALSE
verbose_output <- if (to_debug) TRUE else FALSE

to_generate_subset <- TRUE

to_py_prompt <- FALSE
to_python <- FALSE
to_generate_py_fwrite <- FALSE
to_generate_feather <- FALSE
to_py_bq <- FALSE

to_thai_prompt <- TRUE
to_thai <- TRUE
to_thai_bq <- FALSE
to_generate_thai_txt <- TRUE
to_thai_all_years <- TRUE

to_spc <- FALSE

if (!to_bq) {
  to_py_bq <- FALSE
  to_thai_bq <- FALSE
}
