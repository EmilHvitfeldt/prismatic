test_length(clr_saturate)
test_length(clr_desaturate)

test_color_class(clr_saturate)
test_color_class(clr_desaturate)

test_that("setting shift = 0 leaves input unchanged", {
  expect_equal_color(clr_saturate(rainbow(10), shift = 0), color(rainbow(10)), 1)
  expect_equal_color(clr_desaturate(rainbow(10), shift = 0), color(rainbow(10)), 1)
})

test_wrong_input(clr_saturate)
test_wrong_input(clr_desaturate)

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


