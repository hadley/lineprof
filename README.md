# lineprof

The `lineprof` package makes it easy to understand the performance (time and memory) of your code by using R's built-in line profiler and visualising the results in a shiny app.

`lineprof` is currently available only on github. You can install it with:

```r
# install.packages("devtools")
devtools::install_github("hadley/lineprof")
```

Note that `lineprof` contains a little C++ code, so you'll need a [development environment](http://www.rstudio.com/ide/docs/packages/prerequisites). 

The two most important functions are `lineprof()`, which does the line profiling, and `shine()` which displays the resulting object in an interactive shiny app:

```r
library(lineprof)
source(find_ex("read-delim.r"))
wine <- find_ex("wine.csv")

x <- lineprof(read_delim(wine, sep = ","), torture = TRUE)
shine(x)
```

## Visualisation

![shiny app screen shot](http://i.imgur.com/nSCtqsM.png)

`lineprof` displays five variables for each line of code:

* `t`: the amount of time spent on that line (in seconds)

* `r`, `a`: the amount of memory released and allocated (in megabytes). The assignment of memory release to a line of is not deterministic because it occurs only when gc is triggered. 

* `d`: the number of duplicates 

You can navigate around the call stack by clicking on the link source code. Hover over individual bars to see the exact values.

## Thanks

A big thanks go to Duncan Murdoch and Luke Tierney for creating the line profiling code questions and answering my many questions about how it works. Similar, I'd like to thank Joe Cheng and Winston Chang for answering all my questions about shiny.
