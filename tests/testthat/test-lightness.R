test_that("clr_lighten() perserves length", {
  expect_length(clr_lighten(rainbow(0), space = "HSL"), 0)
  expect_length(clr_lighten(rainbow(1), space = "HSL"), 1)
  expect_length(clr_lighten(rainbow(10), space = "HSL"), 10)
  expect_length(clr_lighten(rainbow(0), space = "HCL"), 0)
  expect_length(clr_lighten(rainbow(1), space = "HCL"), 1)
  expect_length(clr_lighten(rainbow(10), space = "HCL"), 10)
  expect_length(clr_lighten(rainbow(0), space = "combined"), 0)
  expect_length(clr_lighten(rainbow(1), space = "combined"), 1)
  expect_length(clr_lighten(rainbow(10), space = "combined"), 10)
})

test_that("clr_darken() perserves length", {
  expect_length(clr_darken(rainbow(0), space = "HSL"), 0)
  expect_length(clr_darken(rainbow(1), space = "HSL"), 1)
  expect_length(clr_darken(rainbow(10), space = "HSL"), 10)
  expect_length(clr_darken(rainbow(0), space = "HCL"), 0)
  expect_length(clr_darken(rainbow(1), space = "HCL"), 1)
  expect_length(clr_darken(rainbow(10), space = "HCL"), 10)
  expect_length(clr_darken(rainbow(0), space = "combined"), 0)
  expect_length(clr_darken(rainbow(1), space = "combined"), 1)
  expect_length(clr_darken(rainbow(10), space = "combined"), 10)
})

test_that("clr_lighten()'s output has colors class", {
  expect_s3_class(clr_lighten(rainbow(10), space = "HSL"), "colors")
  expect_s3_class(clr_lighten(rainbow(10), space = "HCL"), "colors")
  expect_s3_class(clr_lighten(rainbow(10), space = "combined"), "colors")
})

test_that("clr_darken()'s output has colors class", {
  expect_s3_class(clr_darken(rainbow(10), space = "HSL"), "colors")
  expect_s3_class(clr_darken(rainbow(10), space = "HCL"), "colors")
  expect_s3_class(clr_darken(rainbow(10), space = "combined"), "colors")
})

test_that("clr_lighten()'s output has colors class", {
  expect_s3_class(clr_lighten(rainbow(10), space = "HSL"), "colors")
  expect_s3_class(clr_lighten(rainbow(10), space = "HCL"), "colors")
  expect_s3_class(clr_lighten(rainbow(10), space = "combined"), "colors")
})

test_that("clr_darken()'s output has colors class", {
  expect_s3_class(clr_darken(rainbow(10), space = "HSL"), "colors")
  expect_s3_class(clr_darken(rainbow(10), space = "HCL"), "colors")
  expect_s3_class(clr_darken(rainbow(10), space = "combined"), "colors")
})

test_that("clr_lighten()'s output has colors class", {
  expect_s3_class(clr_lighten(rainbow(10), space = "HSL"), "colors")
  expect_s3_class(clr_lighten(rainbow(10), space = "HCL"), "colors")
  expect_s3_class(clr_lighten(rainbow(10), space = "combined"), "colors")
})

test_that("clr_darken()'s output has colors class", {
  expect_s3_class(clr_darken(rainbow(10), space = "HSL"), "colors")
  expect_s3_class(clr_darken(rainbow(10), space = "HCL"), "colors")
  expect_s3_class(clr_darken(rainbow(10), space = "combined"), "colors")
})

test_that("clr_lighten() complains when col type is wrong.", {
  expect_error(clr_lighten("not a color"))
  expect_error(clr_lighten(list(pal = "#000000")))
})

test_that("clr_darken() complains when col type is wrong.", {
  expect_error(clr_darken("not a color"))
  expect_error(clr_darken(list(pal = "#000000")))
})

test_that("clr_lighten()' if the length  of `shift` isn't 1", {
  expect_visible(clr_lighten(rainbow(10), rep(1, 1)))
  expect_error(clr_lighten(rainbow(10), seq(0, 1, length.out = 2)))
  expect_error(clr_lighten(rainbow(10), seq(0, 1, length.out = 3)))
  expect_visible(clr_lighten(rainbow(10), seq(0, 1, length.out = 10)))
})

test_that("clr_darken()' if the length  of `shift` isn't 1", {
  expect_visible(clr_darken(rainbow(10), rep(1, 1)))
  expect_error(clr_darken(rainbow(10), seq(0, 1, length.out = 2)))
  expect_error(clr_darken(rainbow(10), seq(0, 1, length.out = 3)))
  expect_visible(clr_darken(rainbow(10), seq(0, 1, length.out = 10)))
})

test_that("clr_lighten() setting severity = 0 leaves input unchanged", {
  expect_equal_color(clr_lighten(rainbow(10), 0, space = "HSL"), color(rainbow(10)), 0)
  expect_equal_color(clr_lighten(rainbow(10), 0, space = "HCL"), color(rainbow(10)), 0)
  expect_equal_color(clr_lighten(rainbow(10), 0, space = "combined"), color(rainbow(10)), 0)
})

test_that("clr_darken() setting severity = 0 leaves input unchanged", {
  expect_equal_color(clr_darken(rainbow(10), 0, space = "HSL"), color(rainbow(10)), 0)
  expect_equal_color(clr_darken(rainbow(10), 0, space = "HCL"), color(rainbow(10)), 0)
  expect_equal_color(clr_darken(rainbow(10), 0, space = "combined"), color(rainbow(10)), 0)
})

test_that("setting shift = 1 leaves result complete black or white", {
  expect_equal(clr_lighten(rainbow(10), shift = 1), color(rep("white", 10)))
  expect_equal(clr_darken(rainbow(10), shift = 1), color(rep("black", 10)))

  expect_equal_color(clr_lighten(rainbow(10), shift = 1, space = "HCL"), color(rep("white", 10)), 1)
  expect_equal(clr_darken(rainbow(10), shift = 1, space = "HCL"), color(rep("black", 10)))

  expect_equal_color(clr_lighten(rainbow(10), shift = 1, space = "combined"), color(rep("white", 10)), 1)
  expect_equal(clr_darken(rainbow(10), shift = 1, space = "combined"), color(rep("black", 10)))
})
