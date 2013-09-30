#' Locate built in demos
#'
#' Mainly used for examples and testing.
#'
#' @param path path to file (within demo directory)
#' @export
#' @keywords internal
find_demo <- function(path) {
  found <- system.file("demo", path, package = "lineprof")

  not_found <- found == ""
  if (any(not_found)) {
    stop("Couldnt't find ", paste0(path[not_found], collapse = ", "),
      call. = FALSE)
  }

  found
}
