# Define the path to the year_to_load.txt file
home_dir <- Sys.getenv("HOME")
year_to_load_path <- file.path(home_dir, "drg-pipeline", "data-cleaning", "cache", "year_to_load.txt")

# Define the input notebook path and the output R script path
input_notebook <- file.path(home_dir, "drg-pipeline", "data-cleaning", "drg-cleaning.ipynb")
output_rscript <- file.path(home_dir, "drg-pipeline", "data-cleaning", "debug", "drg-cleaning.r")

# Loop over the years 2018 to 2023
for (year in c(2019, 2022)) {
  # Write the year_to_load to the year_to_load.txt file
  write(as.character(year), year_to_load_path)

  # Step 1: Convert the Jupyter notebook to an R script
  if (.Platform$OS.type == "unix") system("cd ~/drg-pipeline && jupyter nbconvert --no-prompt --to script data-cleaning/drg-cleaning.ipynb --output debug/drg-cleaning")

  # Step 2: Run the generated R script
  run_rscript_command <- paste("Rscript", output_rscript)
  system(run_rscript_command)
}
