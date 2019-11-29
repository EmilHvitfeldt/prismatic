test_length(clr_saturate)
test_length(clr_desaturate)

test_color_class(clr_saturate)
test_color_class(clr_desaturate)

test_wrong_input(clr_saturate)
test_wrong_input(clr_desaturate)

test_severity(clr_saturate)
test_severity(clr_desaturate)

test_severity_range(clr_saturate)
test_severity_range(clr_desaturate)

test_that("setting shift = 0 leaves input unchanged", {
  expect_equal_color(clr_saturate(rainbow(10), shift = 0), color(rainbow(10)), 1)
  expect_equal_color(clr_desaturate(rainbow(10), shift = 0), color(rainbow(10)), 1)
})
