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

test_severity_range(clr_deutan)
test_severity_range(clr_protan)
test_severity_range(clr_tritan)

test_severity_0(clr_deutan)
test_severity_0(clr_protan)
test_severity_0(clr_tritan)

test_that("plotting returns the data invisibly", {
  expect_invisible(check_color_blindness(rainbow(10)))
})
