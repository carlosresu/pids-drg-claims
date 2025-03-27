# Global parameters
thread_offset <- 0
sample_size_divisor <- 625

# Whether to use single, standardized *_raw_master_*.rds
to_create_std <- FALSE
# Whether to sample each split_part by sample_size_divisor
# (useful when iterating through code runs in quick succession)
to_sample <- FALSE
# TODO: Add description here
to_write <- TRUE
# To flush intermediate files
to_filter <- FALSE
# To filter using run 1: is_covid = FALSE, claim_status = G,
# clin_outpatient = FALSE, and id_hci = inst_level %in% "L1", "L2", "L3", "INF"
to_flush <- FALSE
# To push to BQ
to_bq <- FALSE
# To parallelize using unix-style multicore lapply
to_parallel <- TRUE
cat("Parallelization:", to_parallel, "\n")
# TODO: Add description here
to_debug <- FALSE
verbose_output <- if (to_debug) TRUE else FALSE


# Grouping parameters
to_generate_subset <- TRUE

to_py_prompt <- FALSE
to_python <- FALSE
to_generate_py_fwrite <- FALSE
to_generate_feather <- FALSE
to_py_bq <- FALSE
to_py_all_years <- FALSE

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
