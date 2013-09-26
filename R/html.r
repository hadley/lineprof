html <- function(x, ...) {
  path <- unique(paths(x))
  if (length(path) == 1 && !is.na(path)) {
    html_align(x, ...)
  } else {
    html_format(x, ...)
  }
}


html_format <- function(x, ...) {
  x <- format(reduce_depth(x))
  x <- x[c("call", "time", "release", "alloc", "dups", "ref")]
  
  make_table(x)
}

html_align <- function(x, ...) {
  x <- align(x, ...)
  
  x$src <- paste0("<code>", escape(x$src), "</code>")
  x$time <- format(x$time, justify = "none", trim = TRUE)
  x$alloc <- format(x$alloc, digits = 3, justify = "none", trim = TRUE)
  x$release <- format(x$release, digits = 3, justify = "none", trim = TRUE)
  x$dups <- format(x$dups, justify = "none", trim = TRUE)
  x$ref[is.na(x$ref)] <- ""

  make_table(x)
}

make_table <- function(x, headers = colnames(x)) {
  cells <- apply(x, 1, function(x) paste0("  <td>", x, "</td>\n", collapse = ""))
  rows <- paste0("<tr>\n", cells, "</tr>\n", collapse = "")
  header <- paste0("<tr>\n", paste0("  <th>", headers, "<th>\n", collapse = ""), 
    "</tr>\n")  
  table <- paste0("<table>\n", header, rows, "</table>\n")
  
  HTML(table)
}

escape <- function(x) {
  x <- gsub("&", "&amp;", x)
  x <- gsub("<", "&lt;", x)
  x <- gsub(">", "&gt;", x)
  
  x
}
