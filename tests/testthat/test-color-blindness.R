test_that("Length is preserved", {
  expect_length(clr_deutan(rainbow(10)), 10)
  expect_length(clr_protan(rainbow(10)), 10)
  expect_length(clr_tritan(rainbow(10)), 10)
})

test_that("output has color class", {
  expect_s3_class(clr_deutan(rainbow(10)), "colors")
  expect_s3_class(clr_protan(rainbow(10)), "colors")
  expect_s3_class(clr_tritan(rainbow(10)), "colors")
})

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

test_wrong_input(clr_deutan)
test_wrong_input(clr_protan)
test_wrong_input(clr_tritan)

test_that("it if the length  of `severity` isn't 1.", {
  expect_visible(clr_deutan(rainbow(10), rep(1, 1)))
  expect_error(clr_deutan(rainbow(10), rep(1, 2)))
  expect_error(clr_deutan(rainbow(10), rep(1, 3)))
  expect_error(clr_deutan(rainbow(10), rep(1, 10)))

  expect_visible(clr_protan(rainbow(10), rep(1, 1)))
  expect_error(clr_protan(rainbow(10), rep(1, 2)))
  expect_error(clr_protan(rainbow(10), rep(1, 3)))
  expect_error(clr_protan(rainbow(10), rep(1, 10)))

  expect_visible(clr_tritan(rainbow(10), rep(1, 1)))
  expect_error(clr_tritan(rainbow(10), rep(1, 2)))
  expect_error(clr_tritan(rainbow(10), rep(1, 3)))
  expect_error(clr_tritan(rainbow(10), rep(1, 10)))
})


