test_length(clr_rotate)

test_color_class(clr_rotate)

test_wrong_input(clr_rotate)

test_severity(clr_rotate)

test_severity_range(clr_rotate)

test_that("setting shift = 0 leaves input unchanged", {
  expect_equal_color(clr_rotate(rainbow(10), degrees = 0), color(rainbow(10)), 1)
  expect_equal_color(clr_rotate(rainbow(10), degrees = 360), color(rainbow(10)), 1)
})
