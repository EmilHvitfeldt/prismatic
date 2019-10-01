test_that("Length is preserved", {
  expect_length(clr_rotate(rainbow(10), 0), 10)
})

test_that("output has color class", {
  expect_s3_class(clr_rotate(rainbow(10), 0), "colors")
})

test_that("setting shift = 0 leaves input unchanged", {
  expect_equal_color(clr_rotate(rainbow(10), degrees = 0), color(rainbow(10)), 1)
  expect_equal_color(clr_rotate(rainbow(10), degrees = 360), color(rainbow(10)), 1)
})

test_that("it complains when col type is wrong.", {
  expect_error(clr_rotate("not a color"))

  expect_error(clr_rotate(list(pal = "#000000")))

  expect_error(clr_rotate(character()))
})

test_that("it if the length  of `severity` isn't 1.", {
  expect_visible(clr_rotate(rainbow(10), rep(1, 1)))
  expect_error(clr_rotate(rainbow(10), seq(0, 1, length.out = 2)))
  expect_error(clr_rotate(rainbow(10), seq(0, 1, length.out = 3)))
  expect_visible(clr_rotate(rainbow(10), seq(0, 1, length.out = 10)))
})


