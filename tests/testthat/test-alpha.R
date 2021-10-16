test_that(paste0("clr_alpha() perserves length"), {
  expect_length(clr_alpha(rainbow(0)), 0)
  expect_length(clr_alpha(rainbow(1)), 1)
  expect_length(clr_alpha(rainbow(10)), 10)
})

test_that("clr_alpha()'s output has colors class", {
  expect_s3_class(clr_alpha(rainbow(10)), "colors")
})

test_that("clr_alpha() complains when col type is wrong.", {
  expect_error(clr_alpha("not a color"))
  expect_error(clr_alpha(list(pal = "#000000")))
})

test_that("clr_alpha()' if the length  of `severity` isn't 1", {
  expect_visible(clr_alpha(rainbow(10), rep(1, 1)))
  expect_error(clr_alpha(rainbow(10), seq(0, 1, length.out = 2)))
  expect_error(clr_alpha(rainbow(10), seq(0, 1, length.out = 3)))
  expect_visible(clr_alpha(rainbow(10), seq(0, 1, length.out = 10)))
})

test_that("clr_alpha() setting severity outside range gives error", {
  expect_error(clr_alpha(rainbow(10), severity = -1))
  expect_error(clr_alpha(rainbow(10), severity = 2))
})

test_that("setting alpha = 1 leaves input completely unchanged", {
  expect_equal(clr_alpha(rainbow(10), alpha = 1), color(rainbow(10)))
})
