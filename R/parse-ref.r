#' @importFrom stringr str_split_fixed
parse_ref <- function(ref, paths) {
  parsed <- parseLineProfileRefs(ref)
  f <- parsed[[1]]
  ref <- parsed[[2]]
  
  # Split refs into path and line
  ref <- str_split_fixed(ref, "#", n = 2)
  path <- paths[as.numeric(ref[, 1])]  
  line <- as.numeric(ref[, 2])
  
  df <- data.frame(f = rev(f), path = rev(path), line = rev(line))
  
  # Remove entries added by lineprof, profile and force 
  # (and any functions that call lineprof)
  discard_to <- which(df$f == "lineprof") + 2
  
  df[-seq_len(discard_to), , drop = FALSE]
}
