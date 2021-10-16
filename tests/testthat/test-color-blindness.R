test_that("clr_deutan() perserves length", {
  expect_length(clr_deutan(rainbow(0)), 0)
  expect_length(clr_deutan(rainbow(1)), 1)
  expect_length(clr_deutan(rainbow(10)), 10)
})

test_that("clr_protan() perserves length", {
  expect_length(clr_protan(rainbow(0)), 0)
  expect_length(clr_protan(rainbow(1)), 1)
  expect_length(clr_protan(rainbow(10)), 10)
})

test_that("clr_tritan() perserves length", {
  expect_length(clr_tritan(rainbow(0)), 0)
  expect_length(clr_tritan(rainbow(1)), 1)
  expect_length(clr_tritan(rainbow(10)), 10)
})

test_that("clr_deutan()'s output has colors class", {
  expect_s3_class(clr_deutan(rainbow(10)), "colors")
})

test_that("clr_protan()'s output has colors class", {
  expect_s3_class(clr_protan(rainbow(10)), "colors")
})

test_that("clr_tritan()'s output has colors class", {
  expect_s3_class(clr_tritan(rainbow(10)), "colors")
})

test_that("clr_deutan() complains when col type is wrong.", {
  expect_error(clr_deutan("not a color"))
  expect_error(clr_deutan(list(pal = "#000000")))
})

test_that("clr_protan() complains when col type is wrong.", {
  expect_error(clr_protan("not a color"))
  expect_error(clr_protan(list(pal = "#000000")))
})

test_that("clr_tritan() complains when col type is wrong.", {
  expect_error(clr_tritan("not a color"))
  expect_error(clr_tritan(list(pal = "#000000")))
})

test_that("clr_deutan()' if the length  of `severity` isn't 1", {
  expect_visible(clr_deutan(rainbow(10), rep(1, 1)))
  expect_error(clr_deutan(rainbow(10), seq(0, 1, length.out = 2)))
  expect_error(clr_deutan(rainbow(10), seq(0, 1, length.out = 3)))
  expect_error(clr_deutan(rainbow(10), seq(0, 1, length.out = 10)))
})

test_that("clr_protan()' if the length  of `severity` isn't 1", {
  expect_visible(clr_protan(rainbow(10), rep(1, 1)))
  expect_error(clr_protan(rainbow(10), seq(0, 1, length.out = 2)))
  expect_error(clr_protan(rainbow(10), seq(0, 1, length.out = 3)))
  expect_error(clr_protan(rainbow(10), seq(0, 1, length.out = 10)))
})

test_that("clr_tritan()' if the length  of `severity` isn't 1", {
  expect_visible(clr_tritan(rainbow(10), rep(1, 1)))
  expect_error(clr_tritan(rainbow(10), seq(0, 1, length.out = 2)))
  expect_error(clr_tritan(rainbow(10), seq(0, 1, length.out = 3)))
  expect_error(clr_tritan(rainbow(10), seq(0, 1, length.out = 10)))
})

test_that("clr_deutan() setting severity outside range gives error", {
  expect_error(clr_deutan(rainbow(10), severity = -1))
  expect_error(clr_deutan(rainbow(10), severity = 2))
})

test_that("clr_protan() setting severity outside range gives error", {
  expect_error(clr_protan(rainbow(10), severity = -1))
  expect_error(clr_protan(rainbow(10), severity = 2))
})

test_that("clr_tritan() setting severity outside range gives error", {
  expect_error(clr_tritan(rainbow(10), severity = -1))
  expect_error(clr_tritan(rainbow(10), severity = 2))
})

test_that("clr_deutan() setting severity = 0 leaves input unchanged", {
  expect_equal_color(clr_deutan(rainbow(10), 0), color(rainbow(10)), 0)
})

test_that("clr_protan() setting severity = 0 leaves input unchanged", {
  expect_equal_color(clr_protan(rainbow(10), 0), color(rainbow(10)), 0)
})

test_that("clr_tritan() setting severity = 0 leaves input unchanged", {
  expect_equal_color(clr_tritan(rainbow(10), 0), color(rainbow(10)), 0)
})

test_that("plotting returns the data invisibly", {
  expect_invisible(check_color_blindness(rainbow(10)))
})
