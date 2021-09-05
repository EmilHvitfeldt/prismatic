library(testthat)

test_that("extract_rgb functions work", {
  rgb_cols <- rgb(1:10, 11:20, 21:30, 31:40, maxColorValue = 255)

  expect_equal(clr_extract_red(rgb_cols),    1:10)
  expect_equal(clr_extract_green(rgb_cols), 11:20)
  expect_equal(clr_extract_blue(rgb_cols),  21:30)
  expect_equal(clr_extract_alpha(rgb_cols), 31:40)
})

test_wrong_input(clr_extract_red)
test_wrong_input(clr_extract_green)
test_wrong_input(clr_extract_blue)
test_wrong_input(clr_extract_alpha)
