#' @importFrom stringr str_split str_replace_all str_trim
parse_prof <- function(path) {  
  # Parse header, including interval
  header <- readLines(path, n = 1)
  opts <- str_split(header, ": ")[[1]]
  interval <- as.numeric(str_split(opts[length(opts)], "=")[[1]][2]) / 1e6
  
  raw <- read.delim(path, sep = ":", skip = 1, header = FALSE,
    stringsAsFactors = FALSE, quote = "")
  
  # Separate file labels and profiling data
  is_label <- raw$V1 != ""

  labels <- raw[is_label, 1:2]
  names(labels) <- c("label", "path")
  labels$label <- as.numeric(str_replace_all(labels$label, "[^0-9]+", ""))
  labels$path <- str_trim(labels$path)
  rownames(labels) <- NULL
  
  # Parse profiling data -----------------
  prof <- raw[!is_label, ]
  prof$V1 <- NULL
  prof$V2 <- as.numeric(prof$V2)
  names(prof) <- c("small_v", "big_v", "nodes", "dups", "source")
  
  # Add time info, and compute total memory (rounded to meg's)
  prof$time <- interval
  prof$mem <- (prof$small_v + prof$big_v + prof$nodes) / 1024 ^ 2
  
  # Compute memory allocation and release
  mem_alloc <- c(diff(prof$mem), NA)
  prof$alloc <- pmax(0, mem_alloc)
  prof$release <- abs(pmin(0, mem_alloc))
  prof$mem <- NULL
  
  # Strip <GC> and collapse to one row per ref
  prof$source <- str_replace_all(prof$source, '"<GC>" ?', "")
  grp <- cumsum(c(FALSE, prof$source[-nrow(prof)] != prof$source[-1]))
  profsum <- aggregate(
    prof[c("time", "alloc", "release", "dups")], 
    list(g = grp), 
    sum)
  profsum$g <- NULL
  profsum$source <- prof$source[!duplicated(grp)]
  
  list(mem = prof, paths = labels$path)
}


#' @importFrom stringr str_split_fixed
add_top_level_loc <- function(mem, paths) {
  linenum <- str_extract(mem$source, "[0-9]+#[0-9]+")
  pieces <- str_split_fixed(linenum, fixed("#"), n = 2)
  
  mem$file <- paths[as.numeric(pieces[, 1])]
  mem$line <- as.numeric(pieces[, 2])
  
  mem
}