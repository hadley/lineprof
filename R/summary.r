#' Summarize call stack by function
#'
#' Aggregates the calling stack bottom-up by function name.
#'
#' @param x a line profiling object
#' @param depth depth of calls
#' @examples
#' \dontrun{
#' library(jsonlite)
#' library(ggplot2)
#' l1 <- lineprof(json <- toJSON(diamonds))
#' #the usual top-down aggregation
#' print(l1)
#'
#' #bottom-up aggregation
#' summary(l1, depth = 3)
#' }
#' @method summary lineprof
#' @export
summary.lineprof <- function(x, depth = 1){
  stopifnot(is(x, "lineprof"))
  profdata <- format(x)
  profsrc <- gsub(" ", "", profdata$src)
  proffun <- regmatches(profsrc, regexpr(paste0("(/[^/]+){1,", depth, "}$"), profsrc))
  proffun <- gsub("/", " > ", sub("^/", "", proffun), fixed=TRUE)
  profagg <- aggregate(profdata[1:4], list(fun = proffun), sum)
  profagg <- profagg[order(profagg$time, decreasing = TRUE),]
  row.names(profagg) <- NULL
  profagg
}
