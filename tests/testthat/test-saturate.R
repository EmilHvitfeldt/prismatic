test_that("clr_saturate() preserves length", {
  expect_length(clr_saturate(rainbow(0)), 0)
  expect_length(clr_saturate(rainbow(1)), 1)
  expect_length(clr_saturate(rainbow(10)), 10)
})

test_that("clr_desaturate() preserves length", {
  expect_length(clr_desaturate(rainbow(0)), 0)
  expect_length(clr_desaturate(rainbow(1)), 1)
  expect_length(clr_desaturate(rainbow(10)), 10)
})

test_that("clr_saturate()'s output has colors class", {
  expect_s3_class(clr_saturate(rainbow(10)), "colors")
})

test_that("clr_desaturate()'s output has colors class", {
  expect_s3_class(clr_desaturate(rainbow(10)), "colors")
})

test_that("clr_saturate() complains when `col` is wrong", {
  expect_error(clr_saturate("not a color"))
  expect_error(clr_saturate(list(pal = "#000000")))
})

test_that("clr_desaturate() complains when `col` is wrong", {
  expect_error(clr_desaturate("not a color"))
  expect_error(clr_desaturate(list(pal = "#000000")))
})

test_that("clr_saturate() if the length of `shift` isn't 1", {
  expect_visible(clr_saturate(rainbow(10), rep(1, 1)))
  expect_visible(clr_saturate(rainbow(10), seq(0, 1, length.out = 10)))
  expect_error(
    clr_saturate(rainbow(10), seq(0, 1, length.out = 2)),
    "`shift` must be of length 1 or the same length as `col`."
  )
  expect_error(
    clr_saturate(rainbow(10), seq(0, 1, length.out = 3)),
    "`shift` must be of length 1 or the same length as `col`."
  )
})

test_that("clr_desaturate() if the length of `shift` isn't 1", {
  expect_visible(clr_desaturate(rainbow(10), rep(1, 1)))
  expect_visible(clr_desaturate(rainbow(10), seq(0, 1, length.out = 10)))
  expect_error(
    clr_desaturate(rainbow(10), seq(0, 1, length.out = 2)),
    "`shift` must be of length 1 or the same length as `col`."
  )
  expect_error(
    clr_desaturate(rainbow(10), seq(0, 1, length.out = 3)),
    "`shift` must be of length 1 or the same length as `col`."
  )
})

test_that("clr_saturate() setting `shift` outside range gives error", {
  expect_error(
    clr_saturate(rainbow(10), shift = -1), "`shift` must be between 0 and 1."
  )
  expect_error(
    clr_saturate(rainbow(10), shift = 2), "`shift` must be between 0 and 1."
  )
})

test_that("clr_desaturate() setting `shift` outside range gives error", {
  expect_error(
    clr_desaturate(rainbow(10), shift = -1), "`shift` must be between 0 and 1."
  )
  expect_error(
    clr_desaturate(rainbow(10), shift = 2), "`shift` must be between 0 and 1."
  )
})

test_that("clr_saturate() setting `shift = 0` leaves input unchanged", {
  expect_equal_color(clr_saturate(rainbow(10), 0), color(rainbow(10)), 0)
})

test_that("clr_desaturate() setting `shift = 0` leaves input unchanged", {
  expect_equal_color(clr_desaturate(rainbow(10), 0), color(rainbow(10)), 0)
})
