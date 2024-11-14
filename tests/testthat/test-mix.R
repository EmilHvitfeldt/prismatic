test_that("clr_mix() preserves length", {
  expect_length(clr_mix(rainbow(0), "blue"), 0)
  expect_length(clr_mix(rainbow(1), "blue"), 1)
  expect_length(clr_mix(rainbow(10), "blue"), 10)
})

test_that("clr_mix()'s output has colors class", {
  expect_s3_class(clr_mix(rainbow(10), "blue"), "colors")
})

test_that("clr_mix() complains when `col` is wrong", {
  expect_error(clr_mix("not a color"))
  expect_error(clr_mix(list(pal = "#000000")))
})

test_that("complains if `mix_in` is wrong length", {
  expect_error(
    clr_mix(rainbow(10), character()), "`mix_in` must be of length 1."
  )
  expect_error(
    clr_mix(rainbow(10), rep("black", 2)), "`mix_in` must be of length 1."
  )
})

test_that("clr_mix() if the length of `ratio` isn't 1", {
  expect_visible(clr_mix(rainbow(10), "blue", rep(0, 1)))
  expect_visible(clr_mix(rainbow(10), "blue", seq(0, 1, length.out = 10)))
  expect_error(
    clr_mix(rainbow(10), "blue", seq(0, 1, length.out = 2)),
    "`ratio` must be of length 1 or the same length as `col`."
  )
  expect_error(
    clr_mix(rainbow(10), "blue", seq(0, 1, length.out = 3)),
    "`ratio` must be of length 1 or the same length as `col`."
  )
})

test_that("clr_mix() setting `ratio` outside range gives error", {
  expect_error(
    clr_mix(rainbow(10), "blue", ratio = -1),
    "`ratio` must be between 0 and 1."
  )
  expect_error(
    clr_mix(rainbow(10), "blue", ratio = 2),
    "`ratio` must be between 0 and 1."
  )
})

test_that("clr_mix() setting `ratio = 0` leaves input unchanged", {
  expect_equal_color(clr_mix(rainbow(10), "blue", 0), color(rainbow(10)), 0)
})

test_that("setting `ratio = 1` turns all col to `mix_in`", {
  expect_equal(clr_mix(rainbow(10), "blue", ratio = 1), color(rep("blue", 10)))
})
