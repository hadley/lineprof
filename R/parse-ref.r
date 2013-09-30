#' @importFrom stringr str_split_fixed
parse_ref <- function(ref, paths) {
  parsed <- parseLineProfileRefs(ref)
  f <- parsed[[1]]
  ref <- parsed[[2]]

  # Remove last duplicated refs
  # e.g. :197781:316585:19036808:0:"<GC>" 1#3 "scan" 1#3 "read_delim"
  #
  # From Duncan Murdoch:
  # What that means is that scan was active, but it was evaluating something
  # which appeared to be from this line:
  #
  # first <- scan(file, what = character(1), nlines = 1, sep = sep,
  #   quiet = TRUE)
  #
  # It could have been evaluating one of the arguments...
  ref[duplicated(ref, fromLast = TRUE)] <- ""

  # Split refs into path and line
  ref <- str_split_fixed(ref, "#", n = 2)
  path <- paths[as.numeric(ref[, 1])]
  line <- as.numeric(ref[, 2])

  df <- data.frame(f = rev(f), path = rev(path), line = rev(line),
    stringsAsFactors = FALSE)

  # Remove entries added by profile and force
  # (and any functions that call lineprof)
  discard_to <- which(df$f == "profile") + 1

  df[-seq_len(discard_to), , drop = FALSE]
}
