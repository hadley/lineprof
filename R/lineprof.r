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


#' @S3method print lineprof
print.lineprof <- function(x, digits = 3, depth = 3,...) {
  max_depth <- max(vapply(x$ref, nrow, integer(1)))
  if (max_depth > depth) {
    message("Reducing depth to ", depth, " (from ", max_depth, ")")
    x <- reduce_depth(x, depth)
  }
  
  x$alloc <- round(x$alloc, digits)
  x$release <- round(x$release, digits)
  
  ref <- vapply(x$ref, function(x) paste(x$f, collapse = "/"), character(1))
  x$call <- format(ref, align = "left")
  x$ref <- NULL
  
  NextMethod(x)
}

#' @S3method [ lineprof
"[.lineprof" <- function(x, ...) {
  out <- NextMethod()
  class(out) <- c("lineprof", "data.frame")
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
  
  prof$ref <- Map(function(ref, pos) {
    if ((offset + pos) > nrow(ref)) return(ref[0, , drop = FALSE])
    ref[seq(offset + pos, nrow(ref), by = 1), , drop = FALSE]
  }, prof$ref, pos)
  
  prof
}

align <- function(prof, digits = 3) {
  path <- unique(vapply(prof$ref, function(x) x$path[[1]], character(1)))
  if (length(path) > 1) {
    stop("Profile refers to multiple files: ", 
      paste(basename(path), collapse = ", "), 
      ". Use filter() to focus on only one",
      call. = FALSE)
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
  contents <- readLines(path)
  lineup <- collapse[match(seq_along(contents), collapse$line), , drop = FALSE]
  
  out <- data.frame(src = contents, lineup, stringsAsFactors = FALSE)
  out$line <- NULL
  out[is.na(out)] <- 0  
  out$ref <- ifelse(is.na(lineup$line), NA, paste0(basename(path), "#", lineup$line))
  rownames(out) <- NULL
  
  out
}

reduce_depth <- function(prof, i = 2) {
  prof$ref <- lapply(prof$ref, function(x) {
    x[seq_len(min(i, nrow(x))), , drop = FALSE]
  })
  collapse(prof)
}

collapse <- function(prof) {
  index <- c(FALSE, unlist(Map(identical, prof$ref[-1], prof$ref[-nrow(prof)])))
  group <- cumsum(!index)
  
  collapsed <- rowsum(prof[c("time", "alloc", "release", "dups")], group, 
    na.rm = TRUE, reorder = FALSE)
  collapsed$ref <- prof$ref[!duplicated(group)]
  
  class(collapsed) <- c("lineprof", "data.frame")
  collapsed
}