#' Line profiling.
#' 
#' @export
#' @examples
#' 
#' source(find_demo("read-delim.r"))
#' source(find_demo("read-table.r"))
#' wine <- find_demo("wine.csv")
#' 
#' lineprof(read.table2(wine, sep = ","), torture = TRUE)
#' lineprof(read_delim(wine), torture = TRUE)
#' @useDynLib lineprof
lineprof <- function(code, interval = 0.01, torture = FALSE) {
  path <- profile(code, interval, torture)
  on.exit(unlink(path))
  
  parse_prof(path)
}

is.lineprof <- function(x) inherits(x, "lineprof")

#' @S3method [ lineprof
"[.lineprof" <- function(x, ...) {
  out <- NextMethod()
  class(out) <- c("lineprof", class(out))
  out
}

#' Focus on 
#' 
#' @param f a character vector providing a sequence of calls
#' @param filename name of the file
focus <- function(prof, f = NULL, filename = NULL) {
  stopifnot(is.lineprof(x))
  
  if (!xor(is.null(f), is.null(filename))) {
    stop("Must supply one of f and filename")
  }
  
  if (!is.null(f)) {
    stopifnot(is.character(f))
    
    pos <- vapply(prof$ref, function(x) contains(x$f, f), integer(1))
    offset <- length(f) - 1
  } else {
    stopifnot(is.character(filename), length(filename) == 1)
   
    filename_pos <- function(x) firstTRUE(basename(x$path) == filename)
    pos <- vapply(prof$ref, filename_pos, integer(1))
    offset <- -1L
  }

  prof <- prof[pos > 0, , drop = FALSE]
  pos <- pos[pos > 0]
  
  prof$ref <- Map(function(ref, pos) ref[(offset + pos):nrow(ref), , drop = FALSE], 
    prof$ref, pos)
  
  prof
}

align <- function(prof) {
  path <- unique(vapply(prof$ref, function(x) x$path[[1]], character(1)))
  if (length(path) > 1) {
    stop("Profile refers to multiple files: ", 
      paste(basename(path), collapse = ", "), 
      ". Use filter() to focus on only one",
      call. = FALSE)
  }
  
  line <- vapply(prof$ref, function(x) x$line[[1]], double(1))
  profsum <- aggregate(
    prof[c("time", "alloc", "release", "dups")], 
    list(line = line), 
    sum)
  
  contents <- readLines(path)
  lineup <- profsum[match(seq_along(contents), profsum$line), , drop = FALSE]
  
  out <- data.frame(src = contents, lineup)
  out$line <- NULL
  rownames(out) <- NULL
  out[is.na(out)] <- 0
  
  
  
}