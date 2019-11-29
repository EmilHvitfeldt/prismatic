test_length(clr_deutan)
test_length(clr_protan)
test_length(clr_tritan)

test_color_class(clr_deutan)
test_color_class(clr_protan)
test_color_class(clr_tritan)

test_wrong_input(clr_deutan)
test_wrong_input(clr_protan)
test_wrong_input(clr_tritan)

test_severity_1(clr_deutan)
test_severity_1(clr_protan)
test_severity_1(clr_tritan)

test_that("setting severity outside range gives error", {
  expect_error(clr_deutan(rainbow(10), severity = -1))
  expect_error(clr_protan(rainbow(10), severity = -1))
  expect_error(clr_tritan(rainbow(10), severity = -1))

  expect_error(clr_deutan(rainbow(10), severity = 2))
  expect_error(clr_protan(rainbow(10), severity = 2))
  expect_error(clr_tritan(rainbow(10), severity = 2))
})

test_that("setting severity = 0 leaves input unchanged", {
  expect_equal(clr_deutan(rainbow(10), severity = 0), color(rainbow(10)))
  expect_equal(clr_protan(rainbow(10), severity = 0), color(rainbow(10)))
  expect_equal(clr_tritan(rainbow(10), severity = 0), color(rainbow(10)))
})
