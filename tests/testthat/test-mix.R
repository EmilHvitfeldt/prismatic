test_length(function(x) clr_mix(x, "blue"))

test_color_class(function(x) clr_mix(x, "blue"))

test_wrong_input(clr_mix)

test_severity(function(x, y) clr_mix(x, "blue", y))

test_severity_range(function(x, y) clr_mix(x, "blue", y))

test_severity_0(function(x, y) clr_mix(x, "blue", y))

test_that("setting shift = 1 leaves input completely changed", {
  expect_equal(clr_mix(rainbow(10), "blue", ratio = 1), color(rep("blue", 10)))
})

test_that("it complains mix_in is wrong length", {
  expect_error(clr_mix(rainbow(10), character()))
  expect_error(clr_mix(rainbow(10), rep("black", 2)))
})
