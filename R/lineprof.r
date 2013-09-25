#' @export
lineprof <- function(code, interval = 0.02, torture = FALSE) {
  path <- profile(code, interval, torture)
  on.exit(unlink(path))
  
  parse_mem_prof(path)
}

profile <- function(code, interval = 0.02, torture = FALSE) {
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
