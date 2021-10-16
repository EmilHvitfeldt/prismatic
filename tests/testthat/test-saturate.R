test_that("clr_saturate() perserves length", {
  expect_length(clr_saturate(rainbow(0)), 0)
  expect_length(clr_saturate(rainbow(1)), 1)
  expect_length(clr_saturate(rainbow(10)), 10)
})

test_that("clr_desaturate() perserves length", {
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

test_that("clr_saturate() complains when col type is wrong.", {
  expect_error(clr_saturate("not a color"))
  expect_error(clr_saturate(list(pal = "#000000")))
})

test_that("clr_desaturate() complains when col type is wrong.", {
  expect_error(clr_desaturate("not a color"))
  expect_error(clr_desaturate(list(pal = "#000000")))
})

test_that("clr_saturate()' if the length  of `severity` isn't 1", {
  expect_visible(clr_saturate(rainbow(10), rep(1, 1)))
  expect_error(clr_saturate(rainbow(10), seq(0, 1, length.out = 2)))
  expect_error(clr_saturate(rainbow(10), seq(0, 1, length.out = 3)))
  expect_visible(clr_saturate(rainbow(10), seq(0, 1, length.out = 10)))
})

test_that("clr_desaturate()' if the length  of `severity` isn't 1", {
  expect_visible(clr_desaturate(rainbow(10), rep(1, 1)))
  expect_error(clr_desaturate(rainbow(10), seq(0, 1, length.out = 2)))
  expect_error(clr_desaturate(rainbow(10), seq(0, 1, length.out = 3)))
  expect_visible(clr_desaturate(rainbow(10), seq(0, 1, length.out = 10)))
})

test_that("clr_saturate() setting severity outside range gives error", {
  expect_error(clr_saturate(rainbow(10), severity = -1))
  expect_error(clr_saturate(rainbow(10), severity = 2))
})

test_that("clr_desaturate() setting severity outside range gives error", {
  expect_error(clr_desaturate(rainbow(10), severity = -1))
  expect_error(clr_desaturate(rainbow(10), severity = 2))
})

test_that("clr_saturate() setting severity = 0 leaves input unchanged", {
  expect_equal_color(clr_saturate(rainbow(10), 0), color(rainbow(10)), 0)
})

test_that("clr_desaturate() setting severity = 0 leaves input unchanged", {
  expect_equal_color(clr_desaturate(rainbow(10), 0), color(rainbow(10)), 0)
})
