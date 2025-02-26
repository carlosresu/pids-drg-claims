# Global parameters
thread_offset <- 0

sample_size_divisor <- 625

# Whether to sample each split_part by sample_size_divisor
# (useful when iterating through code runs in quick succession)
to_sample <- TRUE
# TODO: Add description here
to_write <- TRUE
# To flush intermediate files
to_flush <- FALSE
# To push to BQ
to_bq <- FALSE
# To parallelize using unix-style multicore lapply
to_parallel <- FALSE
cat("Parallelization:", to_parallel, "\n")
# TODO: Add description here
to_debug <- TRUE
verbose_output <- if (to_debug) TRUE else FALSE

# Grouping parameters
to_generate_subset <- TRUE

to_py_prompt <- TRUE
to_python <- TRUE
to_generate_py_fwrite <- TRUE
to_generate_feather <- TRUE
to_py_bq <- FALSE

to_thai_prompt <- TRUE
to_thai <- TRUE
to_thai_bq <- TRUE
to_generate_thai_txt <- FALSE
to_thai_all_years <- FALSE

to_spc <- FALSE

if (!to_bq) {
  to_py_bq <- FALSE
  to_thai_bq <- FALSE
}
