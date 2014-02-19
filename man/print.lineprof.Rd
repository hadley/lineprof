\name{print.lineprof}
\alias{print.lineprof}
\alias{reduce_depth}
\title{Print reduced line profiling call stack.}
\usage{
\method{print}{lineprof}(x, digits = 3, depth = 2, ...)

reduce_depth(x, depth = 2)
}
\arguments{
  \item{x}{a line profiling object}

  \item{depth}{depth of calls}

  \item{digits}{number of digits to use for performance
  summaries}

  \item{...}{other arguments passed on to
  \code{\link{format}}}
}
\description{
By default, whenever you print a line profile, it is
collapsed to two levels deep.  You can override this using
the \code{depth} parameter. \code{reduce_depth} is the
function that implements this behaviour - you may want to
use it if you're processing the line profiling data in
other ways.
}
\examples{
# Line profiling the profile parsing code
x <- parse_prof(find_ex("read-delim.prof"))

x
print(x, depth = 3)
print(x, depth = Inf)
}
\keyword{internal}

