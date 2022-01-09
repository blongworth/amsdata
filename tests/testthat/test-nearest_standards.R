test_that("nearest_standards finds correct number of standards", {
  expect_length(nearest_standards(2, c(1,2,3), 3), 3)
})

test_that("nearest_standards finds nearest standards by run number", {
  expect_equal(nearest_standards(2, c(1,3), 2), c(1,3))
  expect_equal(nearest_standards(5, c(3,4,10,20), 2), c(4,3))
})

test_that("nearest_standards Fails with bad inputs", {
  expect_error(nearest_standards(0, c(1,3), 2))
  expect_error(nearest_standards(1, 0, 2))
  expect_error(nearest_standards(1, c(0,3), 2))
  expect_error(nearest_standards(1, c(1,3), 0))
})
