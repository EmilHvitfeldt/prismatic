test_that("clr_mix() perserves length", {
  expect_length(clr_mix(rainbow(0), "blue"), 0)
  expect_length(clr_mix(rainbow(1), "blue"), 1)
  expect_length(clr_mix(rainbow(10), "blue"), 10)
})

test_that("clr_mix()'s output has colors class", {
  expect_s3_class(clr_mix(rainbow(10), "blue"), "colors")
})

test_that("clr_mix() complains when col type is wrong.", {
  expect_error(clr_mix("not a color"))
  expect_error(clr_mix(list(pal = "#000000")))
})

test_that("clr_mix()' if the length  of `mix_in` isn't 1", {
  expect_visible(clr_mix(rainbow(10), "blue", rep(0, 1)))
  expect_error(clr_mix(rainbow(10), "blue", seq(0, 1, length.out = 2)))
  expect_error(clr_mix(rainbow(10), "blue", seq(0, 1, length.out = 3)))
  expect_visible(clr_mix(rainbow(10), "blue", seq(0, 1, length.out = 10)))
})

test_that("clr_mix() setting severity outside range gives error", {
  expect_error(clr_mix(rainbow(10), "blue", severity = -1))
  expect_error(clr_mix(rainbow(10), "blue", severity = 2))
})

test_that("clr_mix() setting severity = 0 leaves input unchanged", {
  expect_equal_color(clr_mix(rainbow(10), "blue", 0), color(rainbow(10)), 0)
})

test_that("setting shift = 1 leaves input completely changed", {
  expect_equal(clr_mix(rainbow(10), "blue", ratio = 1), color(rep("blue", 10)))
})

test_that("it complains mix_in is wrong length", {
  expect_error(clr_mix(rainbow(10), character()))
  expect_error(clr_mix(rainbow(10), rep("black", 2)))
})
