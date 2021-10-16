library(testthat)
library(farver)

test_that("extract_rgb functions work", {
  rgb_cols <- rgb(1:10, 11:20, 21:30, 31:40, maxColorValue = 255)

  expect_equal(clr_extract_red(rgb_cols), 1:10)
  expect_equal(clr_extract_green(rgb_cols), 11:20)
  expect_equal(clr_extract_blue(rgb_cols), 21:30)
  expect_equal(clr_extract_alpha(rgb_cols), 31:40)
})

test_that("clr_extract_red() complains when col type is wrong.", {
  expect_error(clr_extract_red("not a color"))
  expect_error(clr_extract_red(list(pal = "#000000")))
})

test_that("clr_extract_green() complains when col type is wrong.", {
  expect_error(clr_extract_green("not a color"))
  expect_error(clr_extract_green(list(pal = "#000000")))
})

test_that("clr_extract_blue() complains when col type is wrong.", {
  expect_error(clr_extract_blue("not a color"))
  expect_error(clr_extract_blue(list(pal = "#000000")))
})

test_that("clr_extract_alpha() complains when col type is wrong.", {
  expect_error(clr_extract_alpha("not a color"))
  expect_error(clr_extract_alpha(list(pal = "#000000")))
})

test_that("extract_rgb functions work", {
  colors <- topo.colors(10)

  hsl_cols <- decode_colour(colors, to = "hsl")

  expect_equal(clr_extract_hue(colors), hsl_cols[, "h"])
  expect_equal(clr_extract_saturation(colors), hsl_cols[, "s"])
  expect_equal(clr_extract_lightness(colors), hsl_cols[, "l"])
})

test_that("clr_extract_hue() complains when col type is wrong.", {
  expect_error(clr_extract_hue("not a color"))
  expect_error(clr_extract_hue(list(pal = "#000000")))
})

test_that("clr_extract_saturation() complains when col type is wrong.", {
  expect_error(clr_extract_saturation("not a color"))
  expect_error(clr_extract_saturation(list(pal = "#000000")))
})

test_that("clr_extract_lightness() complains when col type is wrong.", {
  expect_error(clr_extract_lightness("not a color"))
  expect_error(clr_extract_lightness(list(pal = "#000000")))
})

test_that("extract_rgb functions work", {
  colors <- topo.colors(10)

  hcl_cols <- decode_colour(colors, to = "hcl")

  expect_equal(clr_extract_hue(colors, space = "HCL"), hcl_cols[, "h"])
  expect_equal(clr_extract_chroma(colors), hcl_cols[, "c"])
  expect_equal(clr_extract_luminance(colors), hcl_cols[, "l"])
})

test_that("clr_extract_chroma() complains when col type is wrong.", {
  expect_error(clr_extract_chroma("not a color"))
  expect_error(clr_extract_chroma(list(pal = "#000000")))
})

test_that("clr_extract_luminance() complains when col type is wrong.", {
  expect_error(clr_extract_luminance("not a color"))
  expect_error(clr_extract_luminance(list(pal = "#000000")))
})
