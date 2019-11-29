test_length(clr_negate)

test_that("output has color class", {
  expect_s3_class(clr_negate(rainbow(10)), "colors")
})

test_wrong_input(clr_negate)
