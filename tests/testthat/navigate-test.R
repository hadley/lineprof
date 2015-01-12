path <- line_profile(eval(parse(text = "
    quick <- function() { identity(identity(pause(.1))) }
    slow <- function() { identity(identity(pause(.2))) }

    quick()
    slow()
           ")))

file.rename(path, "tests/testthat/navigate-test.prof")
