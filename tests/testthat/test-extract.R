library(testthat)
library(farver)

test_that("extract_rgb functions work", {
  rgb_cols <- rgb(1:10, 11:20, 21:30, 31:40, maxColorValue = 255)

  expect_equal(clr_extract_red(rgb_cols),    1:10)
  expect_equal(clr_extract_green(rgb_cols), 11:20)
  expect_equal(clr_extract_blue(rgb_cols),  21:30)
  expect_equal(clr_extract_alpha(rgb_cols), 31:40)
})

test_wrong_input(clr_extract_red)
test_wrong_input(clr_extract_green)
test_wrong_input(clr_extract_blue)
test_wrong_input(clr_extract_alpha)

test_that("extract_rgb functions work", {
  colors <- topo.colors(10)

  hsl_cols <- decode_colour(colors, to = "hsl")

  expect_equal(clr_extract_hue(colors), hsl_cols[,"h"])
  expect_equal(clr_extract_saturation(colors), hsl_cols[,"s"])
  expect_equal(clr_extract_lightness(colors), hsl_cols[,"l"])
})

test_wrong_input(clr_extract_hue)
test_wrong_input(clr_extract_saturation)
test_wrong_input(clr_extract_lightness)

test_that("extract_rgb functions work", {
  colors <- topo.colors(10)

  hcl_cols <- decode_colour(colors, to = "hcl")

  expect_equal(clr_extract_hue(colors, space = "HCL"), hcl_cols[,"h"])
  expect_equal(clr_extract_chroma(colors), hcl_cols[,"c"])
  expect_equal(clr_extract_luminance(colors), hcl_cols[,"l"])
})

test_wrong_input(clr_extract_chroma)
test_wrong_input(clr_extract_luminance)
