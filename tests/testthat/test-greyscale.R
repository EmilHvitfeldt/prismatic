test_that("Length is preserved", {
  expect_length(clr_grayscale(rainbow(10)), 10)
})

test_that("output has color class", {
  expect_s3_class(clr_grayscale(rainbow(10)), "color")
})

test_that("result is grayscale", {
  res  <- col2rgb(clr_grayscale(rainbow(10)))

  expect_equal(res[1, ], res[2, ])
  expect_equal(res[1, ], res[3, ])
})

test_that("it complains when col type is wrong.", {
  expect_error(clr_grayscale("not a color"))

  expect_error(clr_grayscale(list(pal = "#000000")))

  expect_error(clr_grayscale(character()))
})

