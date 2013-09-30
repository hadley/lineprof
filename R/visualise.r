#' Print reduced line profiling call stack.
#' 
#' By default, whenever you print a line profile, it is collapsed to two levels
#' deep.  You can override this using the \code{depth} parameter. 
#' \code{reduce_depth} is the function that implements this behaviour - you
#' may want to use it if you're processing the line profiling data in other 
#' ways.
#' 
#' @param x a line profiling object
#' @param depth depth of calls
#' @param digits number of digits to use for performance summaries
#' @param ... other arguments passed on to \code{\link{format}}
#' @export
#' @keywords internal
#' @examples
#' # Line profiling the profile parsing code
#' x <- parse_prof(find_demo("read-delim.prof"))
#'
#' x
#' print(x, depth = 3)
#' print(x, depth = Inf)
#' @S3method print lineprof
print.lineprof <- function(x, digits = 3, depth = 2, ...) {
  max_depth <- max(vapply(x$ref, nrow, integer(1)))
  if (max_depth > depth) {
    message("Reducing depth to ", depth, " (from ", max_depth, ")")
    x <- reduce_depth(x, depth)
  }
  
  path <- unique(paths(x))
  if (length(path) == 1 && !is.na(path)) {
    message("Common path: ", basename(path))  
  }
  
  print(format(x, digits = digits, depth = depth, ...))
}

#' @rdname print.lineprof
#' @export
reduce_depth <- function(x, depth = 2) {
  x$ref <- lapply(x$ref, function(x) {
    x[seq_len(min(depth, nrow(x))), , drop = FALSE]
  })
  collapse(x, ignore.path = TRUE)
}

#' @S3method format lineprof
format.lineprof <- function(x, digits = 3, ...) {
  x$alloc <- round(x$alloc, digits)
  x$release <- round(x$release, digits)
  
  ref <- vapply(x$ref, function(x) paste(x$f, collapse = "/"), character(1))
  x$src <- format(ref, align = "left")
  
  x$ref <- vapply(x$ref, FUN.VALUE = character(1), function(x) {
    first <- x[1, , drop = FALSE]
    if (is.na(first$path)) {
      paste0(deparse(x$f), collapse = "")
    } else {
      paste0(basename(first$path), "#", first$line)
    }
  })
  
  class(x) <- "data.frame"
  x
}


#' Align line profiling data with source code
#' 
#' @inheritParams print.lineprof
#' @export
align <- function(x, digits = 3) {
  path <- unique(paths(x))
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
  line <- vapply(x$ref, function(x) x$line[[1]], double(1))
  collapse <- aggregate(
    x[c("time", "alloc", "release", "dups")], 
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
