\name{lineprof}
\alias{lineprof}
\title{Line profiling.}
\usage{
  lineprof(code, interval = 0.01, torture = FALSE)
}
\description{
  Line profiling.
}
\examples{
source(find_demo("read-delim.r"))
source(find_demo("read-table.r"))
wine <- find_demo("wine.csv")

lineprof(read.table2(wine, sep = ","), torture = TRUE)
lineprof(read.delim(wine), torture = TRUE)
}

