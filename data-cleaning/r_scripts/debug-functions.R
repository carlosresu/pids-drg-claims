source(here("data-cleaning", "r_scripts", "libraries.R"))

concatenate_r_files <- function(input_path, output_file) {
  # List all .R files in the directory
  r_files <- list.files(
    path = input_path,
    pattern = "\\.R$", full.names = TRUE
  )

  # Delete the existing output file if it exists
  if (file.exists(output_file)) {
    file.remove(output_file)
  }

  # Read and concatenate contents
  file_contents <- lapply(r_files, readLines)
  concatenated_content <- unlist(file_contents)

  # Write concatenated content to the output file
  cat(concatenated_content, file = output_file, sep = "\n")
}

# Example usage within your script
input_path <- here("data-cleaning", "r_scripts")
output_file <- paste0(here("data-cleaning", "everything", "everything.R"))

concatenate_r_files(input_path, output_file)
