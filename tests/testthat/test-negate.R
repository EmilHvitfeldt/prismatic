test_that("Length is preserved", {
  expect_length(clr_negate(rainbow(10)), 10)
})

test_that("output has color class", {
  expect_s3_class(clr_negate(rainbow(10)), "colors")
})

test_wrong_input(clr_negate)
