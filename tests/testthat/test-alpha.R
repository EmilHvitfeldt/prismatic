test_that("Length is preserved", {
  expect_length(clr_alpha(rainbow(10), 0.5), 10)
})

test_that("output has color class", {
  expect_s3_class(clr_alpha(rainbow(10), 0.4), "colors")
})

test_that("setting alpha = 1 leaves input completely unchanged", {
  expect_equal(clr_alpha(rainbow(10), alpha = 1), color(rainbow(10)))
})

test_wrong_input(clr_alpha)

test_that("it complains mix_in is wrong length", {
  expect_error(clr_alpha(rainbow(10), double()))
  expect_error(clr_alpha(rainbow(10), rep(0.4, 2)))
})
