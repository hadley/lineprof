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
focus <- function(prof, f = NULL, filename = NULL, ref = NULL) {
  stopifnot(is.lineprof(prof))
  
  if (sum(!is.null(f), !is.null(filename), !is.null(ref)) != 1) {
    stop("Must supply one of f, filename or ref")
  }
  
  if (!is.null(f)) {
    stopifnot(is.character(f))
    
    find_pos <- function(x) contains(x$f, f)
    offset <- length(f) - 1
  } else if (!is.null(filename)) {
    stopifnot(is.character(filename), length(filename) == 1)
   
    find_pos <- function(x) firstTRUE(basename(x$path) == filename)
    offset <- -1L
  } else if (!is.null(ref)) {
    stopifnot(is.character(ref), length(ref) == 1)
    
    find_pos <- function(x) {
      firstTRUE(paste0(basename(x$path), "#", x$line) == ref)
    }
    offset <- 1L
  }
  
  pos <- vapply(prof$ref, find_pos, integer(1))
  
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

show <- function(prof) {
  ref <- vapply(prof$ref, function(x) paste(x$f, collapse = "/"), character(1))
  
  prof$ref <- format(ref, align = "left")
  prof
}

reduce_depth <- function(prof, i = 2) {
  prof$ref <- lapply(prof$ref, function(x) {
    x[seq_len(min(i, nrow(x))), , drop = FALSE]
  })
  collapse(prof)
}

collapse <- function(prof) {
  ind <- c(FALSE, unlist(Map(identical, prof$ref[-1], prof$ref[-nrow(prof)])))
  grp <- cumsum(!ind)
  
  col <- aggregate(
    prof[c("time", "alloc", "release", "dups")], 
    list(g = grp), 
    sum)
  col$ref <- prof$ref[!duplicated(grp)]
  col$g <- NULL
  col  
}