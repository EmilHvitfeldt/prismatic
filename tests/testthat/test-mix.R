test_that("Length is preserved", {
  expect_length(clr_mix(rainbow(10), "blue"), 10)
})

test_that("output has color class", {
  expect_s3_class(clr_mix(rainbow(10), "blue"), "colors")
})

test_that("setting shift = 0 leaves input unchanged", {
  expect_equal(clr_mix(rainbow(10), "blue", ratio = 0), color(rainbow(10)), 1)
})

test_that("setting shift = 1 leaves input completely changed", {
  expect_equal(clr_mix(rainbow(10), "blue", ratio = 1), color(rep("blue", 10)))
})

test_that("it complains when col type is wrong.", {
  expect_error(clr_mix("not a color"))

  expect_error(clr_mix(list(pal = "#000000")))

  expect_error(clr_mix(character()))
})

test_that("it complains mix_in is wrong length", {
  expect_error(clr_mix(rainbow(10), character()))
  expect_error(clr_mix(rainbow(10), rep("black", 2)))
})

test_that("it if the length  of `severity` isn't 1.", {
  expect_visible(clr_mix(rainbow(10), rep(1, 1)))
  expect_error(clr_mix(rainbow(10), seq(0, 1, length.out = 2)))
  expect_error(clr_mix(rainbow(10), seq(0, 1, length.out = 3)))
  expect_error(clr_mix(rainbow(10), seq(0, 1, length.out = 10)))
})


