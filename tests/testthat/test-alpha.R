test_length(clr_alpha)
test_color_class(clr_alpha)
test_wrong_input(clr_alpha)
test_severity(clr_alpha)
test_severity_range(clr_alpha)

test_that("setting alpha = 1 leaves input completely unchanged", {
  expect_equal(clr_alpha(rainbow(10), alpha = 1), color(rainbow(10)))
})
