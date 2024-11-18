test_that("clr_rotate() preserves length", {
  expect_length(clr_rotate(rainbow(0)), 0)
  expect_length(clr_rotate(rainbow(1)), 1)
  expect_length(clr_rotate(rainbow(10)), 10)
})

test_that("clr_rotate()'s output has colors class", {
  expect_s3_class(clr_rotate(rainbow(10)), "colors")
})

test_that("clr_rotate() complains when `col` is wrong", {
  expect_error(clr_rotate("not a color"))
  expect_error(clr_rotate(list(pal = "#000000")))
})

test_that("clr_rotate() if the length of `degrees` isn't 1", {
  expect_visible(clr_rotate(rainbow(10), rep(1, 1)))
  expect_visible(clr_rotate(rainbow(10), seq(0, 1, length.out = 10)))
  expect_error(
    clr_rotate(rainbow(10), seq(0, 1, length.out = 2)),
    "`degrees` must be of length 1 or the same length as `col`."
  )
  expect_error(
    clr_rotate(rainbow(10), seq(0, 1, length.out = 3)),
    "`degrees` must be of length 1 or the same length as `col`."
  )
})

test_that("clr_rotate() setting `degrees` outside range gives error", {
  expect_error(
    clr_rotate(rainbow(10), degrees = -1),
    "`degrees` must be numeric between 0 and 360."
  )
  expect_error(
    clr_rotate(rainbow(10), degrees = 720),
    "`degrees` must be numeric between 0 and 360."
  )
})

test_that("clr_rotate() setting `degrees = 0` leaves input unchanged", {
  expect_equal_color(clr_rotate(rainbow(10), 0), color(rainbow(10)), 0)
})
