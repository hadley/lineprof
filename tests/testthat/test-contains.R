context("contains.cpp")

test_that("returns starting index if found", {
  #one string
  expect_equal(contains(letters, "a"), 1)
  #two strings
  expect_equal(contains(letters, c("b", "c")), 2)
  #multi-strings with failed partial match
  expect_equal(contains(c("a", letters), letters), 2)
})

test_that("returns 0 if not found", {
  #doesn't exist
  expect_equal(contains(letters, "1"), 0)
  #failed partial match
  expect_equal(contains(letters, c("b", "a")), 0)
  #haystack is substring of needle should also fail
  expect_equal(contains(letters, c(letters, "a")), 0)
})

test_that("works for empty inputs", {
  expect_equal(contains(character(), "foo"), 0)
  expect_equal(contains("foo", character()), 1)
  expect_equal(contains(character(), character()), 1)
})
