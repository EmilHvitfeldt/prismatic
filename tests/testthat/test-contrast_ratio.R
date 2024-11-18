# contrast_ratio() -------------------------------------------------------------

test_that("contrast_ratio() works", {
  expect_equal(contrast_ratio("white", "white"), 1)
  expect_equal(contrast_ratio("red", "red"), 1)
  expect_equal(contrast_ratio("white", "black"), 21)
  expect_equal(contrast_ratio("red", "blue"), contrast_ratio("blue", "red"))
  expect_equal(contrast_ratio(color("white"), color("white")), 1)
  expect_equal(length(contrast_ratio("blue", rainbow(10))), 10)
})

test_that("contrast_ratio() errors if `x` not length 1", {
  expect_error(
    length(contrast_ratio(rainbow(2), rainbow(10))),
    "`x` must have length 1. Length was: 2."
  )
})

# best_contrast() --------------------------------------------------------------

test_that("best_contrast() works", {
  expect_equal(best_contrast("white"), "#010101")
  expect_equal(best_contrast("white", c("black", "white")), "black")
  expect_equal(best_contrast("grey80", c("#999999", "black")), "black")
  expect_equal(best_contrast("grey80", c("#999999", "white")), "#999999")
})

test_that("best_contrast() errors if `x` not length 1", {
  expect_error(
    length(contrast_ratio(rainbow(2), rainbow(10))),
    "`x` must have length 1. Length was: 2."
  )
})

test_that("best_contrast() errors if `NA` in `x` or `y`", {
  expect_error(
    best_contrast(c(NA, rainbow(10))),
    "`x` and `y` must not contain any `NA`."
  )
  expect_error(
    best_contrast(rainbow(10), c("black", NA)),
    "`x` and `y` must not contain any `NA`."
  )
})

test_that("best_contrast() errors if elements in `y` not unique", {
  expect_error(
    best_contrast(rainbow(10), c("black", "blue", "black")),
    "Elements in `y` must be unique."
  )
  expect_error(
    best_contrast(rainbow(10), c("black", "white", "black")),
    "Elements in `y` must be unique."
  )
})
