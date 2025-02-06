# Define the directory containing the R scripts
script_dir <- "~/drg-pipeline/data-cleaning/r_scripts_v2"

# Temporary directory for script processing
system("mkdir -p ~/drg-pipeline/data-cleaning/debug/codebase/temp")

# Output file paths
helper_scripts_path <- "~/drg-pipeline/data-cleaning/debug/codebase/helper-scripts.R"
main_script_r_path <- "~/drg-pipeline/data-cleaning/debug/codebase/main-script-partial-cleaning-grouping.R"
main_script_py_path <- "~/drg-pipeline/data-cleaning/debug/codebase/main-script-grouping-python.py"

# Read the files in the desired order (sorted by filename)
script_files <- list.files(script_dir, pattern = "\\.R$", full.names = TRUE)

# Function to remove comments and useless parts from combined R scripts
clean_content <- function(lines) {
  lines <- lines[!grepl("^\\s*#", lines)] # Remove comments
  lines <- lines[lines != ""] # Remove empty lines
  lines <- lines[!grepl("^\
+$", lines)] # Remove groups of empty lines
  return(lines)
}

# Read and clean helper scripts and store in temp
helper_content <- unlist(lapply(script_files, readLines))
clean_helper_content <- clean_content(helper_content)
write(clean_helper_content, file = "~/drg-pipeline/data-cleaning/debug/codebase/temp/helper-scripts.R")

# Convert Jupyter Notebooks to R and Python scripts in temp
system("jupyter nbconvert --to script --output-dir=~/drg-pipeline/data-cleaning/debug/codebase/temp ~/drg-pipeline/data-cleaning/00b-drg-partial.ipynb")
system("jupyter nbconvert --to script --output-dir=~/drg-pipeline/data-cleaning/debug/codebase/temp ~/drg-pipeline/data-cleaning/01-drg-cleaning-v2.ipynb")
system("jupyter nbconvert --to script --output-dir=~/drg-pipeline/data-cleaning/debug/codebase/temp ~/drg-pipeline/data-cleaning/02-drg-grouping-v2.ipynb")
system("jupyter nbconvert --to script --output-dir=~/drg-pipeline/data-cleaning/debug/codebase/temp ~/drg-pipeline/data-cleaning/02b-drg-grouping-py-v2.ipynb")

# Paths to converted temp files
temp_partial <- "~/drg-pipeline/data-cleaning/debug/codebase/temp/00b-drg-partial.R"
temp_cleaning <- "~/drg-pipeline/data-cleaning/debug/codebase/temp/01-drg-cleaning-v2.R"
temp_grouping <- "~/drg-pipeline/data-cleaning/debug/codebase/temp/02-drg-grouping-v2.R"
temp_grouping_py <- "~/drg-pipeline/data-cleaning/debug/codebase/temp/02b-drg-grouping-py-v2.py"

# Read and clean main script (partial, cleaning, grouping)
main_r_content <- c(
  readLines(temp_partial), "\n",
  readLines(temp_cleaning), "\n",
  readLines(temp_grouping)
)
clean_main_r_content <- clean_content(main_r_content)
write(clean_main_r_content, file = main_script_r_path)

# Read and clean main Python script (grouping-py)
main_py_content <- readLines(temp_grouping_py)
clean_main_py_content <- clean_content(main_py_content)
write(clean_main_py_content, file = main_script_py_path)

# Move cleaned helper script to final location
file.rename("~/drg-pipeline/data-cleaning/debug/codebase/temp/helper-scripts.R", helper_scripts_path)

# Remove temp directory and its contents
system("rm -rf ~/drg-pipeline/data-cleaning/debug/codebase/temp")
