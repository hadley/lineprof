align <- function(prof, digits = 3) {
  path <- unique(paths(prof))
  if (length(path) > 1) {
    stop("Profile refers to multiple files: ", 
      paste(basename(path), collapse = ", "), 
      ". Use filter() to focus on only one.",
      call. = FALSE)
  }
  if (is.na(path)) {
    stop("Profile doesn't refer to any files.", call. = FALSE)
  }
  
  # Collapse summary to individual lines
  line <- vapply(prof$ref, function(x) x$line[[1]], double(1))
  collapse <- aggregate(
    prof[c("time", "alloc", "release", "dups")], 
    list(line = line), 
    sum)
  collapse$alloc <- round(collapse$alloc, digits)
  collapse$release <- round(collapse$release, digits)
  
  # Read in code and align profiling data
  contents <- readLines(path, warn = FALSE)
  lineup <- collapse[match(seq_along(contents), collapse$line), , drop = FALSE]
  
  out <- data.frame(src = contents, lineup, stringsAsFactors = FALSE)
  out$line <- NULL
  out[is.na(out)] <- 0  
  out$ref <- ifelse(is.na(lineup$line), NA, paste0(basename(path), "#", lineup$line))
  rownames(out) <- NULL
  
  out
}
