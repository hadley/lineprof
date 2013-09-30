#' Focus in on selected parts of the profile.
#'
#' \code{focus} allows you to navigate a complicated call stack with a flexible
#' navigation specification. \code{auto_focus} is called automatically to zoom
#' into the file that occupies the majority of the current level - this is
#' particularly useful for \code{\link{gctorture}}.
#'
#' @param x a line profile
#' @param f a character vector providing a sequence of calls
#' @param filename base name of the file
#' @param ref a reference of the form \code{"filename.r#lineno"}
#' @export
#' @examples
#' # Line profiling the profile parsing code
#' (x <- lineprof(parse_prof(find_demo("read-delim.prof"))))
#'
#' # Zoom into line 38
#' (x1 <- focus(x, ref = "parse.r#38"))
#'
#' # Zoom into the lapply
#' (x2 <- focus(x1, f = c("lapply", "FUN")))
#' align(x2)
#'
#' # Zoom into line 21
#' (x3 <- focus(x2, ref = "parse-ref.r#21"))
focus <- function(x, f = NULL, filename = NULL, ref = NULL) {
  stopifnot(is.lineprof(x))

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

  pos <- vapply(x$ref, find_pos, integer(1))

  x <- x[pos > 0, , drop = FALSE]
  pos <- pos[pos > 0]

  x$ref <- Map(function(ref, pos) {
    if ((offset + pos) > nrow(ref)) return(ref[0, , drop = FALSE])
    ref[seq(offset + pos, nrow(ref), by = 1), , drop = FALSE]
  }, x$ref, pos)

  x
}

#' @rdname focus
#' @export
auto_focus <- function(x) {
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
