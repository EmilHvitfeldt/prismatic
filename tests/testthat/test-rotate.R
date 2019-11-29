test_length(clr_rotate)

test_color_class(clr_rotate)

test_that("setting shift = 0 leaves input unchanged", {
  expect_equal_color(clr_rotate(rainbow(10), degrees = 0), color(rainbow(10)), 1)
  expect_equal_color(clr_rotate(rainbow(10), degrees = 360), color(rainbow(10)), 1)
})

test_wrong_input(clr_rotate)

test_that("it if the length  of `severity` isn't 1.", {
  expect_visible(clr_rotate(rainbow(10), rep(1, 1)))
  expect_error(clr_rotate(rainbow(10), seq(0, 1, length.out = 2)))
  expect_error(clr_rotate(rainbow(10), seq(0, 1, length.out = 3)))
  expect_visible(clr_rotate(rainbow(10), seq(0, 1, length.out = 10)))
})


