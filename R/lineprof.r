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

profile <- function(code, interval = 0.01, torture = FALSE) {
  prof_path <- tempfile(fileext = ".prof")
  
  if (torture) {
    gctorture(TRUE)
    on.exit(gctorture(FALSE), add = TRUE)
  }
  
  gc()
  Rprof(prof_path, interval = interval, memory.profiling = TRUE, 
    line.profiling = TRUE, gc.profiling = TRUE)
  on.exit(Rprof(NULL), add = TRUE)
  force(code)
  
  prof_path
}
