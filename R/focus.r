
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
