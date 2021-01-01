library(testthat)

test_that("contrast_ratio works", {
  expect_equal(contrast_ratio("white", "white"), 1)
  expect_equal(contrast_ratio("red", "red"), 1)
  expect_equal(contrast_ratio("white", "black"), 21)

  expect_equal(
    contrast_ratio("red", "blue"),
    contrast_ratio("blue", "red")
  )

  expect_equal(contrast_ratio(color("white"), color("white")), 1)
})

test_that("contrast_ratio works", {
  expect_equal(
    length(contrast_ratio("blue", rainbow(10))),
    10
  )
  expect_error(
    length(contrast_ratio(rainbow(2), rainbow(10))),
    "`x` must have length 1."
  )
})
