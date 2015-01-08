context("shine")

# This test is related to #29. In essense, this is to make sure shine can distinguish
# two call stacks even if they share same consecutive functions. This was a problem because
# only two function names were used during navigation without keeping track of upstream.
test_that("navigate can distinguish similar call stacks", {
#   file.rename(line_profile(eval(parse(text = "
#     quick <- function() { identity(identity(pause(.1))) }
#     slow <- function() { identity(identity(pause(.2))) }
#
#     quick()
#     slow()
#     "))), "inst/examples/test-shine.prof")
  x <- parse_prof(find_ex("test-shine.prof"))

  #setup fake shine (a headless shiny would be ideal)
  lapply(body(shine), function(expr) {
    if(length(grep("runApp", expr)) == 0) eval(expr, envir = sys.frame(-2))
  })

  #initial check
  expect_equal(length(x$time), 2)
  totaltime <- sum(x$time)

  #simulate drill down to first line item
  drill_first <- function() {
    navigate(json(stack$top())$ref[1])
  }

  #goes into eval
  drill_first()
  #goes into quick
  drill_first()
  #now the time should equal to quick
  expect_equal(stack$top()$time, x[1, ]$time)

  #goes into quick/identity
  drill_first()
  #goes into identity/identity under quick
  drill_first()
  #now the time should still equal to quick
  expect_equal(stack$top()$time, x[1, ]$time)
})
