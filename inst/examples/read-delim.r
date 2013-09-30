read_delim <- function(file, header = TRUE, sep = ",", stringsAsFactors = TRUE) {
  # Determine number of fields by reading first line
  first <- scan(file, what = character(1), nlines = 1, sep = sep, quiet = TRUE)
  p <- length(first)
    
  # Load all fields
  all <- scan(file, what = as.list(rep("character", p)), sep = sep, 
    skip = if (header) 1 else 0, quiet = TRUE)

  # Convert from strings to appropriate types
  all[] <- lapply(all, type.convert, as.is = !stringsAsFactors)
  
  # Set column names
  if (header) {
    names(all) <- first
    rm(first)
  } else {
    names(all) <- paste0("V", seq_along(all))
  }
    
  # Convert list into data frame
  class(all) <- "data.frame"
  attr(all, "row.names") <- c(NA_integer_, -length(all[[1]]))
    
  all
}
