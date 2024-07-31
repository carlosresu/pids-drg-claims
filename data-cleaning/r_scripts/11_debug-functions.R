concatenate_r_files <- function(input_path, output_file) {
  #' @title Concatenate R Files
  #'
  #' @description This function concatenates all .R files in 
  #' a specified directory into a single output file.
  #'
  #' @param input_path character. The directory containing the 
  #' .R files to concatenate.
  #' @param output_file character. The path to the output file 
  #' where the concatenated content will be written.
  #'
  #' @return NULL.

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
