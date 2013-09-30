\name{line_profile}
\alias{line_profile}
\alias{parse_prof}
\title{Profile and parse output}
\usage{
  line_profile(code, interval = 0.01, torture = FALSE)

  parse_prof(path)
}
\arguments{
  \item{path}{path to line profiling data}

  \item{code}{code to profile.}

  \item{interval}{interval, in seconds, between profile
  snapshots. R's timer has a resolution of at best 1 ms, so
  there's no reason to make this value smaller, but if
  you're profiling a long running function you might want
  to set it to something larger.}

  \item{torture}{if \code{TRUE}, turns on
  \code{\link{gctorture}} which forces \code{\link{gc}} to
  run after (almost) every allocation. This is useful if
  you want to see exactly when memory is released. It also
  makes R run extremely slowly (10-100x slower than usual)
  so it can also be useful to simulate a smaller
  \code{interval}.}
}
\description{
  These are useful if you want to see the interim output
  that \code{\link{lineprof}} creates.
}
\keyword{internal}

