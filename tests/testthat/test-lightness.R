test_length(function(x) clr_lighten(x, space = "HSL"))
test_length(function(x) clr_lighten(x, space = "HCL"))
test_length(function(x) clr_lighten(x, space = "combined"))

test_length(function(x) clr_darken(x, space = "HSL"))
test_length(function(x) clr_darken(x, space = "HCL"))
test_length(function(x) clr_darken(x, space = "combined"))

test_color_class(function(x) clr_lighten(x, space = "HSL"))
test_color_class(function(x) clr_lighten(x, space = "HCL"))
test_color_class(function(x) clr_lighten(x, space = "combined"))

test_color_class(function(x) clr_darken(x, space = "HSL"))
test_color_class(function(x) clr_darken(x, space = "HCL"))
test_color_class(function(x) clr_darken(x, space = "combined"))

test_that("setting shift = 0 leaves input unchanged", {
  expect_equal_color(clr_lighten(rainbow(10), shift = 0), color(rainbow(10)), 1)
  expect_equal_color(clr_darken(rainbow(10), shift = 0), color(rainbow(10)), 1)
})

test_that("setting shift = 1 leaves result complete black or white", {
  expect_equal(clr_lighten(rainbow(10), shift = 1), color(rep("white", 10)))
  expect_equal(clr_darken(rainbow(10), shift = 1), color(rep("black", 10)))

  expect_equal_color(clr_lighten(rainbow(10), shift = 1, space = "HCL"), color(rep("white", 10)), 1)
  expect_equal(clr_darken(rainbow(10), shift = 1, space = "HCL"), color(rep("black", 10)))

  expect_equal_color(clr_lighten(rainbow(10), shift = 1, space = "combined"), color(rep("white", 10)), 1)
  expect_equal(clr_darken(rainbow(10), shift = 1, space = "combined"), color(rep("black", 10)))
})

test_wrong_input(clr_lighten)
test_wrong_input(clr_darken)

test_that("it if the length  of `severity` isn't 1.", {
  expect_visible(clr_lighten(rainbow(10), rep(1, 1)))
  expect_error(clr_lighten(rainbow(10), seq(0, 1, length.out = 2)))
  expect_error(clr_lighten(rainbow(10), seq(0, 1, length.out = 3)))
  expect_visible(clr_lighten(rainbow(10), seq(0, 1, length.out = 10)))

  expect_visible(clr_darken(rainbow(10), rep(1, 1)))
  expect_error(clr_darken(rainbow(10), seq(0, 1, length.out = 2)))
  expect_error(clr_darken(rainbow(10), seq(0, 1, length.out = 3)))
  expect_visible(clr_darken(rainbow(10), seq(0, 1, length.out = 10)))
})


