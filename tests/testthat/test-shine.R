context("shine")

# This test is related to #29. In essense, this is to make sure shine can distinguish
# two call stacks even if they share same consecutive functions. This was a problem because
# only two function names were used during navigation without keeping track of upstream.
test_that("navigate can distinguish similar call stacks", {
  x <- parse_prof("navigate-test.prof")

  #initialize stack
  stack <- new_stack(x)

  #initial check
  expect_equal(length(x$time), 2)

  #simulate drill down to first line item quietly
  drill_first <- function() {
    suppressMessages(navigate(json(stack$top())$ref[1], stack))
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

describe("stack", {
  it("can be initialized with or without element", {
    expect_equal(new_stack(1)$top(), 1)

    empty_stack <- new_stack()
    expect_error(empty_stack$top(), "less than one element")
  })
  it("can push and pop elements", {
    stack <- new_stack()

    #one element
    stack$push(1)
    expect_equal(stack$top(), 1)

    #two element
    stack$push(2)
    expect_equal(stack$top(), 2)

    #back to one element
    expect_equal(stack$pop(), 2)
    expect_equal(stack$top(), 1)
  })
  it("won't pop the last element if it's initialized with it", {
    stack <- new_stack(1)
    expect_equal(stack$top(), 1)
    #can pop but returns NULL
    expect_null(stack$pop())
    #last element is still in place
    expect_equal(stack$top(), 1)
  })
})
