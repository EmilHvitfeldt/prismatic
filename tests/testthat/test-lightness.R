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

test_wrong_input(function(x) clr_lighten(x, space = "HSL"))
test_wrong_input(function(x) clr_lighten(x, space = "HCL"))
test_wrong_input(function(x) clr_lighten(x, space = "combined"))

test_wrong_input(function(x) clr_darken(x, space = "HSL"))
test_wrong_input(function(x) clr_darken(x, space = "HCL"))
test_wrong_input(function(x) clr_darken(x, space = "combined"))

test_severity(function(x, y) clr_lighten(x, space = "HSL", y))
test_severity(function(x, y) clr_lighten(x, space = "HCL", y))
test_severity(function(x, y) clr_lighten(x, space = "combined", y))

test_severity(function(x, y) clr_darken(x, space = "HSL", y))
test_severity(function(x, y) clr_darken(x, space = "HCL", y))
test_severity(function(x, y) clr_darken(x, space = "combined", y))

test_severity_range(function(x, y) clr_lighten(x, space = "HSL", y))
test_severity_range(function(x, y) clr_lighten(x, space = "HCL", y))
test_severity_range(function(x, y) clr_lighten(x, space = "combined", y))

test_severity_range(function(x, y) clr_darken(x, space = "HSL", y))
test_severity_range(function(x, y) clr_darken(x, space = "HCL", y))
test_severity_range(function(x, y) clr_darken(x, space = "combined", y))

test_severity_0(function(x, y) clr_lighten(x, space = "HSL", y), tol = 1)
test_severity_0(function(x, y) clr_lighten(x, space = "HCL", y), tol = 1)
test_severity_0(function(x, y) clr_lighten(x, space = "combined", y), tol = 1)

test_severity_0(function(x, y) clr_darken(x, space = "HSL", y), tol = 1)
test_severity_0(function(x, y) clr_darken(x, space = "HCL", y), tol = 1)
test_severity_0(function(x, y) clr_darken(x, space = "combined", y), tol = 1)

test_that("setting shift = 1 leaves result complete black or white", {
  expect_equal(clr_lighten(rainbow(10), shift = 1), color(rep("white", 10)))
  expect_equal(clr_darken(rainbow(10), shift = 1), color(rep("black", 10)))

  expect_equal_color(clr_lighten(rainbow(10), shift = 1, space = "HCL"), color(rep("white", 10)), 1)
  expect_equal(clr_darken(rainbow(10), shift = 1, space = "HCL"), color(rep("black", 10)))

  expect_equal_color(clr_lighten(rainbow(10), shift = 1, space = "combined"), color(rep("white", 10)), 1)
  expect_equal(clr_darken(rainbow(10), shift = 1, space = "combined"), color(rep("black", 10)))
})
