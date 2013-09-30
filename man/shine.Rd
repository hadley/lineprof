\name{shine}
\alias{shine}
\title{Generate shiny GUI for line profiling output}
\usage{
  shine(x)
}
\arguments{
  \item{x}{a line profiling dataset}
}
\description{
  The shiny GUI generates two types of view depending on
  whether or not the src refs for the function are
  available. If src refs are available, it aligns profiling
  information with the original source code so that you can
  easily see time and memory behaviour in the context of
  the original code. If they are not available, it does the
  best it can do, just displaying the sequence of calls
  that it captured.
}
\section{Display}{
  The shiny app creates a table with six columns:

  \itemize{ \item the line number (or call number) \item
  the source code, or if not available, the name of the
  function \item t, the amount of time spent on that line
  (in seconds) \item r, the amount of memory released by
  that call (in megabytes) \item a, the amount of memory
  allocated by that call (in megabytes) \item d, the number
  of duplications performed by that call }

  The time and memory summaries are displayed as inline bar
  charts. This gives you a qualitative impression of how
  expensive each line of code is - to get the precise
  details, hover over the bar.
}

\section{Navigation}{
  Calls/lines with a non-trivial amount of computation will
  be linked so that you can see the breakdown of
  time/memory within that line/call. This will work well
  for your own code (since you will have all the srcrefs),
  but less well for other packages and base R code since
  you'll only be able to see the sequence of the calls.

  To go back to the previous call, click on the back
  button.
}
\examples{
source(find_demo("read-delim.r"))
wine <- find_demo("wine.csv")

x <- lineprof(read_delim(wine, sep = ","), torture = TRUE)
\dontrun{
shine(x)
}
}

