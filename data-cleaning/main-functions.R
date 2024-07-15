main_read_function <- function() {
  if (to_read) {
    if (to_view_checks) {
      print("Reading the entire file...")
    }
    dt <- read_entire_file(drop_cols)
    
    if (to_sample) {
      if (file.exists(sampled_claims)) {
        if (to_view_checks) {
          print("Sampled file exists. Reading the sampled file...")
        }
        dt <- read_sampled_file()
        # Check if the number of rows matches sample_size
        if (nrow(dt) != sample_size) {
          if (to_view_checks) {
            print(paste("Sampled file does not match sample size. Expected:", 
                        sample_size, "Found:", nrow(dt), "Re-sampling..."))
          }
          dt <- read_entire_file(drop_cols)
          dt <- sample_data(dt)
          if (to_write) {
            if (to_view_checks) {
              print(paste("to_write is TRUE. Writing the new sample data to file:", 
                          sampled_claims))
            }
            fwrite(dt, sampled_claims)
          } else {
            if (to_view_checks) {
              print("to_write is FALSE. Not writing the sample data to file.")
            }
          }
        } else {
          if (to_view_checks) {
            print("Sampled file matches sample size.")
          }
        }
      } else {
        if (to_view_checks) {
          print("Sampled file does not exist. Creating new sample...")
        }
        dt <- sample_data(dt)
        if (to_write) {
          if (to_view_checks) {
            print(paste("to_write is TRUE. Writing the new sample data to file:", 
                        sampled_claims))
          }
          fwrite(dt, sampled_claims)
        } else {
          if (to_view_checks) {
            print("to_write is FALSE. Not writing the sample data to file.")
          }
        }
      }
    }
  } else {
    if (to_sample) {
      if (file.exists(sampled_claims)) {
        if (to_view_checks) {
          print("Sampled file exists. Reading the sampled file...")
        }
        dt <- read_sampled_file()
        # Check if the number of rows matches sample_size
        if (nrow(dt) != sample_size) {
          if (to_view_checks) {
            print(paste("Sampled file does not match sample size. Expected:", 
                        sample_size, "Found:", nrow(dt), "Re-sampling..."))
          }
          dt <- read_entire_file(drop_cols)
          dt <- sample_data(dt)
          if (to_write) {
            if (to_view_checks) {
              print(paste("to_write is TRUE. Writing the new sample data to file:", 
                          sampled_claims))
            }
            fwrite(dt, sampled_claims)
          } else {
            if (to_view_checks) {
              print("to_write is FALSE. Not writing the sample data to file.")
            }
          }
        } else {
          if (to_view_checks) {
            print("Sampled file matches sample size.")
          }
        }
      } else {
        if (to_view_checks) {
          print("Sampled file does not exist.")
          print("Reading entire file and creating new sample...")
        }
        dt <- read_entire_file(drop_cols)
        dt <- sample_data(dt)
        if (to_write) {
          if (to_view_checks) {
            print(paste("to_write is TRUE. Writing the new sample data to file:", 
                        sampled_claims))
          }
          fwrite(dt, sampled_claims)
        } else {
          if (to_view_checks) {
            print("to_write is FALSE. Not writing the sample data to file.")
          }
        }
      }
    } else {
      if (!file.exists(intermediate_file)) {
        stop("Cannot proceed: to_read is FALSE and to_sample is FALSE. 
           At least one must be TRUE.")
      } else {
        if (to_view_checks) {
          print("Using existing intermediate file.")
        }
      }
    }
  }
  return(dt)
}