library(testthat)

context("Votes Information")
# output <- vott$new()
output <- APIpkg::vott$new()
meth_test <- output$calinfo(x = '2')


#test if our method is working fine
test_that("calinfo method is working", {
  expect_true(class(meth_test) == "numeric")
})

#test that the input dates are between defined limits
test_that("Input Date are between defined API Limits", {
  expect_error(output$calinfo(startD = "2001", endD = "2021")) ##need to check arguments
})

#test that we are getting the data frames for arg1
test_that("calinfo should return Dataframe of all votes in given period", {
  mem_name = meth_test$name
  expect_true(class(mem_name)[1] == "data.frame")
})

#test that we are getting the data frames for arg2
test_that("calinfo should return Dataframe of all votes in given period", {
  mem_app = meth_test$approve
  expect_true(class(mem_app)[1] == "data.frame")
})

#test that we are getting the data frames for arg 3
test_that("calinfo should return Dataframe of all votes in given period", {
  mem_nay = meth_test$approve
  expect_true(class(mem_nay)[1] == "data.frame")
})