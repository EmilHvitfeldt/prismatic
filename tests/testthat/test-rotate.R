test_that("clr_rotate() perserves length", {
  expect_length(clr_rotate(rainbow(0)), 0)
  expect_length(clr_rotate(rainbow(1)), 1)
  expect_length(clr_rotate(rainbow(10)), 10)
})

test_that("clr_rotate()'s output has colors class", {
  expect_s3_class(clr_rotate(rainbow(10)), "colors")
})

test_that("clr_rotate() complains when col type is wrong.", {
  expect_error(clr_rotate("not a color"))
  expect_error(clr_rotate(list(pal = "#000000")))
})

test_that("clr_rotate()' if the length  of `degrees` isn't 1", {
  expect_visible(clr_rotate(rainbow(10), rep(1, 1)))
  expect_error(clr_rotate(rainbow(10), seq(0, 1, length.out = 2)))
  expect_error(clr_rotate(rainbow(10), seq(0, 1, length.out = 3)))
  expect_visible(clr_rotate(rainbow(10), seq(0, 1, length.out = 10)))
})

test_that("clr_rotate() setting severity outside range gives error", {
  expect_error(clr_rotate(rainbow(10), severity = -1))
  expect_error(clr_rotate(rainbow(10), severity = 2))
})

test_that("clr_rotate() setting severity = 0 leaves input unchanged", {
  expect_equal_color(clr_rotate(rainbow(10), 0), color(rainbow(10)), 0)
})
