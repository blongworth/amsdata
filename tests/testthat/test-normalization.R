test_that("norm_run works for toy input", {
  expect_equal(norm_run(1,1,1), 1)
})

test_that("norm_run_err works for toy input", {
  expect_equal(norm_run_err(1,1,0,0,1,0), 0)
})

test_that("norm_snicser works for toy input", {
  expect_equal(norm_snicser(1,1,1), 1)
})

test_that("norm_snicser_err works for toy input", {
  expect_equal(norm_snicser_err(1,0,1,0,1), 0)
})
