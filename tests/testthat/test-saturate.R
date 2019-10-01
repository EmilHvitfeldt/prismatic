
test_that("Length is preserved", {
  expect_length(clr_saturate(rainbow(10)), 10)
  expect_length(clr_desaturate(rainbow(10)), 10)
})

test_that("output has color class", {
  expect_s3_class(clr_saturate(rainbow(10)), "colors")
  expect_s3_class(clr_desaturate(rainbow(10)), "colors")
})

test_that("setting shift = 0 leaves input unchanged", {
  expect_equal_color(clr_saturate(rainbow(10), shift = 0), color(rainbow(10)), 1)
  expect_equal_color(clr_desaturate(rainbow(10), shift = 0), color(rainbow(10)), 1)
})

test_that("it complains when col type is wrong.", {
  expect_error(clr_saturate("not a color"))
  expect_error(clr_desaturate("not a color"))

  expect_error(clr_saturate(list(pal = "#000000")))
  expect_error(clr_desaturate(list(pal = "#000000")))

  expect_error(clr_saturate(character()))
  expect_error(clr_desaturate(character()))
})

test_that("it if the length  of `severity` isn't 1.", {
  expect_visible(clr_saturate(rainbow(10), rep(1, 1)))
  expect_error(clr_saturate(rainbow(10), seq(0, 1, length.out = 2)))
  expect_error(clr_saturate(rainbow(10), seq(0, 1, length.out = 3)))
  expect_visible(clr_saturate(rainbow(10), seq(0, 1, length.out = 10)))

  expect_visible(clr_desaturate(rainbow(10), rep(1, 1)))
  expect_error(clr_desaturate(rainbow(10), seq(0, 1, length.out = 2)))
  expect_error(clr_desaturate(rainbow(10), seq(0, 1, length.out = 3)))
  expect_visible(clr_desaturate(rainbow(10), seq(0, 1, length.out = 10)))
})


