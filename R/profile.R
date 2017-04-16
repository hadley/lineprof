#' Profile and parse output
#'
#' These are useful if you want to see the interim output that
#' \code{\link{lineprof}} creates.
#'
#' @keywords internal
#' @inheritParams lineprof
#' @export
line_profile <- function(code, prof_path, interval = 0.001, torture = FALSE) {

  if (torture) {
    gctorture(TRUE)
    on.exit(gctorture(FALSE), add = TRUE)
  }

  gc()
  Rprof(prof_path, interval = interval, memory.profiling = TRUE,
    line.profiling = TRUE, gc.profiling = TRUE)
  on.exit(Rprof(NULL), add = TRUE)
  tryCatch(
    force(code),
    error = function(e) NULL,
    interrupt = function(e) NULL
  )

}

#' @rdname line_profile
#' @importFrom stringr str_split str_replace_all str_trim
#' @export
#' @param path path to line profiling data
parse_prof <- function(path) {
  lines <- readLines(path)
  if (length(lines) < 2) {
    stop("No parsing data available. Maybe your function was too fast?",
      call. = FALSE)
  }

  # Parse header, including interval
  opts <- str_split(lines[[1]], ": ")[[1]]
  interval <- as.numeric(str_split(opts[length(opts)], "=")[[1]][2]) / 1e6
  lines <- lines[-1]

  # Separate file labels and profiling data
  is_label <- grepl("^#", lines)

  label_lines <- lines[is_label]
  label_pieces <- str_split_fixed(label_lines, ": ", 2)
  labels <- data.frame(
    label = seq_along(label_pieces),
    path = label_pieces[, 2],
    stringsAsFactors = FALSE)

  # Parse profiling data -----------------
  prof_lines <- lines[!is_label]
  prof <- as.data.frame(str_split_fixed(prof_lines, ":", 6),
    stringsAsFactors = FALSE)
  prof$V1 <- NULL
  names(prof) <- c("small_v", "big_v", "nodes", "dups", "source")
  prof[c("small_v", "big_v", "nodes", "dups")] <- lapply(
    prof[c("small_v", "big_v", "nodes", "dups")], as.numeric)

  # Add time info, and compute total memory (rounded to meg's)
  prof$time <- interval
  prof$mem <- (prof$small_v + prof$big_v + prof$nodes) / 1024 ^ 2

  # Compute memory allocation and release
  mem_alloc <- c(diff(prof$mem), NA)
  prof$alloc <- pmax(0, mem_alloc)
  prof$release <- abs(pmin(0, mem_alloc))
  prof$mem <- NULL

  # Strip <GC>, parse refs and collapse
  prof$source <- str_replace_all(prof$source, '"<GC>" ?', "")
  prof$ref <- lapply(prof$source, parse_ref, paths = labels$path)

  auto_focus(collapse(prof))
}

#' @importFrom stringr str_split_fixed str_extract fixed
add_top_level_loc <- function(mem, paths) {
  linenum <- str_extract(mem$source, "[0-9]+#[0-9]+")
  pieces <- str_split_fixed(linenum, fixed("#"), n = 2)

  mem$file <- paths[as.numeric(pieces[, 1])]
  mem$line <- as.numeric(pieces[, 2])

  mem
}
