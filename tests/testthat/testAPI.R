context("Votes Information")
# vott <- vott$new()

test_that("vott rejects errounous input", {
  expect_error(vott(name1 = 'C', startY = 2002, endY = 2020))
  expect_error(vott(name1 = 'A', startY = 1900, endY = 1999))
})

#test if our function is working fine
test_that("vott function output is a dataframe", {
  expect_error(class(vott(name1, startY, endY)) == "data.frame")
})

