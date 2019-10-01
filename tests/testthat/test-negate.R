test_that("Length is preserved", {
  expect_length(clr_negate(rainbow(10)), 10)
})

test_that("output has color class", {
  expect_s3_class(clr_negate(rainbow(10)), "colors")
})

test_that("it complains when col type is wrong.", {
  expect_error(clr_negate("not a color"))

  expect_error(clr_negate(list(pal = "#000000")))

  expect_error(clr_negate(character()))
})

