# Display unique before and after mappings for each categorical variable
display_unique_mappings <- function(mapped_data, mapping_name) {
  # Ensure the data has the correct columns
  if (
    !("Original" %in% names(mapped_data)) ||
      !("Mapped" %in% names(mapped_data))) {
    stop("The data table must contain 'Original' and 'Mapped' columns.")
  }

  # Create a data table to display unique before and after mappings
  unique_mappings <- unique(mapped_data)

  # Print the mappings using kable
  print(kable(unique_mappings,
    format = "markdown",
    caption = sprintf("Unique Before and After Mappings for %s", mapping_name)
  ))
}
