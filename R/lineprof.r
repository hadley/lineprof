#' Line profiling.
#' 
#' \code{lineprof} uses R built-in time and memory profiler \code{\link{Rprof}}
#' to collect line profiling data and displays it in ways that help you
#' figure out where the bottlenecks in your code R.
#' 
#' R's profiler is a sampling profiler, which means that it stops execution
#' every \code{interval} seconds and records the current call stack. This means
#' that it is somewhat random: if you run the profiler repeatedly on the same
#' code you'll see slightly different outputs depending on exactly where the 
#' profiler stopped each time.
#' 
#' @section Profiling output:
#' 
#' For each sequence of calls, \code{lineprof} calculates:
#' 
#' \itemize{
#'   \item \code{time}: the time in seconds
#'   \item \code{alloc}: the memory allocated, in megabytes
#'   \item \code{released}: the memory released, in megabytes. Unless
#'     \code{torture = TRUE} this release is somewhat random:
#'     memory will be only released if a garbage collection is triggered by
#'     an allocation.
#'   \item \code{dups}: the number of calls to the internal \code{duplicate}
#'     function which is called by C code to duplicate R vectors.
#' }
#' 
#' Note that memory allocation is only approximate due to the nature of the 
#' sampling profiler: if a large block of memory is allocated and then released
#' in between ticks of the timer, no change in memory will be recorded. Using
#' \code{torture = TRUE} helps prevent this, mostly by making R super slow.
#' 
#' @param code code to profile.
#' @param interval interval, in seconds, between profile snapshots. R's timer
#'   has a resolution of at best 1 ms, so there's no reason to make this value
#'   smaller, but if you're profiling a long running function you might want to
#'   set it to something larger.
#' @param torture if \code{TRUE}, turns on \code{\link{gctorture}} which forces
#'   \code{\link{gc}} to run after (almost) every allocation. This is useful
#'   if you want to see exactly when memory is released. It also makes R run
#'   extremely slowly (10-100x slower than usual) so it can also be useful to
#'   simulate a smaller \code{interval}.
#' @export
#' @examples
#' source(find_demo("read-delim.r"))
#' source(find_demo("read-table.r"))
#' wine <- find_demo("wine.csv")
#' 
#' x1 <- lineprof(read.table2(wine, sep = ","), torture = TRUE)
#' x2 <- lineprof(read_delim(wine), torture = TRUE)
#' 
#' shine(x1)
#' shine(x2)
#' @useDynLib lineprof
lineprof <- function(code, interval = 0.001, torture = FALSE) {
  path <- profile(code, interval, torture)
  on.exit(unlink(path))
  
  parse_prof(path)
}

is.lineprof <- function(x) inherits(x, "lineprof")

#' @S3method [ lineprof
"[.lineprof" <- function(x, ...) {
  out <- NextMethod()
  class(out) <- c("lineprof", "data.frame")
  out
}

#' @S3method print lineprof
print.lineprof <- function(x, digits = 3, depth = 2,...) {
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

paths <- function(x) {
  vapply(x$ref, FUN.VALUE = character(1), function(x) {
    if (length(x$path) == 0) NA_character_ else x$path[[1]]
  })
}

reduce_depth <- function(prof, i = 2) {
  prof$ref <- lapply(prof$ref, function(x) {
    x[seq_len(min(i, nrow(x))), , drop = FALSE]
  })
  collapse(prof, ignore.path = TRUE)
}

collapse <- function(prof, ignore.path = FALSE) {
  if (ignore.path) {
    # Only needs to compare calls
    call <- vapply(prof$ref, function(x) paste(x$f, collapse = "\u001F"), 
      character(1))
    index <- c(FALSE, call[-1] == call[-length(call)])    
  } else {
    index <- c(FALSE, unlist(Map(identical, prof$ref[-1], prof$ref[-nrow(prof)]))) 
  }
  group <- cumsum(!index)
  
  collapsed <- rowsum(prof[c("time", "alloc", "release", "dups")], group, 
    na.rm = TRUE, reorder = FALSE)
  collapsed$ref <- prof$ref[!duplicated(group)]
  class(collapsed) <- c("lineprof", "data.frame")
  
  collapsed
}

# Automatically zoom to majority file
auto_zoom <- function(x) {
  # tapply automatically drops missing x
  paths <- basename(paths(x))
  prop_time <- tapply(x$time, paths, sum) / sum(x$time)
    
  # Don't zoom if:
  # * no src refs (i.e. all paths are missing)
  # * already single file that is 100%
  # * no file uses more than 95%
  if (length(prop_time) == 0) return(x)
  if (length(prop_time) == 1 && prop_time == 1) return(x)
  if (max(prop_time) < 0.95) return(x)
  
  i <- which.max(prop_time)
  message("Zooming to ", names(prop_time)[i], " (", floor(prop_time[i] * 100), 
    "% of total time)")
  
  x[paths == names(prop_time)[i] & !is.na(paths), , drop = FALSE] 
}
