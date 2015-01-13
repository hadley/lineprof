#' Use eval(parse(text="...")) here because we want to test navigate using function
#' call stacks instead of line numbers. The following will cause Rprof to use
#' <text>#linenumber as refs which ultimately fall back on function call stacks.
path <- line_profile(eval(parse(text = "
    quick <- function() { identity(identity(pause(.1))) }
    slow <- function() { identity(identity(pause(.2))) }

    quick()
    slow()
           ")))

file.rename(path, "tests/testthat/navigate-test.prof")
