test_that("clr_negate() perserves length", {
  expect_length(clr_negate(rainbow(0)), 0)
  expect_length(clr_negate(rainbow(1)), 1)
  expect_length(clr_negate(rainbow(10)), 10)
})

test_that("clr_negate()'s output has colors class", {
  expect_s3_class(clr_negate(rainbow(10)), "colors")
})

test_that("clr_negate() complains when col type is wrong.", {
  expect_error(clr_negate("not a color"))
  expect_error(clr_negate(list(pal = "#000000")))
})
