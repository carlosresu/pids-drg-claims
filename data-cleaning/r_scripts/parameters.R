source(here("data-cleaning", "r_scripts", "libraries.R"))

year_to_load <- "2018"
version <- "v2"

sample_size <- 25 * 1e3
seed <- 123
rows_to_show <- 10

drop_cols <- c(
    paste0("ICDCODE", 13:14),
    "ICCODED15",
    paste0("ICDCODE", 16:170)
)
icd_cols <- paste0("clin_icd", 1:12)
rvs_cols <- paste0("clin_rvs", 1:20)

to_read <- FALSE
to_sample <- TRUE
to_write <- TRUE
to_group <- TRUE
to_filter <- FALSE # unused
to_profvis <- FALSE
to_chunk <- TRUE # doesn't work if false; not chunking is deprecated.
to_view_checks <- TRUE
to_view_checks_parallelized <- TRUE

set.seed(seed)

options(future.globals.maxSize = 1024 * 1024^2)

global_seed <- seed # for parallelized operations
