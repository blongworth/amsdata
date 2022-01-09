test_that("nearest_standards finds correct number of standards", {
  expect_length(nearest_standards(2, c(1,2,3), 3), 3)
})

test_that("nearest_standards finds nearest standards by run number", {
  expect_equal(nearest_standards(2, c(1,3), 2), c(1,3))
  expect_equal(nearest_standards(5, c(3,4,10,20), 2), c(4,3))
})

test_that("nearest_standards finds nearest standards by time", {
  times <- seq(as.POSIXct("2022-01-01"), by = "min", length.out = 10)
  expect_equal(nearest_standards(times[5], times, 1), times[5])
  expect_equal(nearest_standards(times[5], times, 5), times[c(5,4,6,3,7)])
})

test_that("nearest_standards Fails with bad inputs", {
  expect_error(nearest_standards(0, c(1,3), 2))
  expect_error(nearest_standards(1, 0, 2))
  expect_error(nearest_standards(1, c(0,3), 2))
  expect_error(nearest_standards(1, c(1,3), 0))
  expect_error(nearest_standards(1, integer(0), 0))
})

test_that("nearest_standards finds same standards as SNICSer", {
})

test_that("find_standard_runs finds standard runs", {
  expect_equal(find_standard_runs(c("B", "S", "SS")), 2)
  expect_equal(find_standard_runs(c("B", "S", "SS", "S")), c(2,4))
})

test_that("find_standard_runs handles no standard runs", {
  expect_equal(find_standard_runs(c("B", "SS")), integer(0))
})

test_that("find_standard_runs drops runs", {
  expect_equal(find_standard_runs(c("B", "S", "SS", "S"), 2), c(4))
  expect_equal(find_standard_runs(c("B", "S", "SS", "S"), c(2, 4)), integer(0))
})


test_that("nearest_standards_time returns same set as SNICSer", {
})
