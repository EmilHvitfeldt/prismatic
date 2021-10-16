test_that("color()'s output has colors class", {
  expect_s3_class(color(rainbow(10)), "colors")
})

test_that("colour()'s output has colors class", {
  expect_s3_class(colour(rainbow(10)), "colors")
})

test_that("color() perserves length", {
  expect_length(color(rainbow(0)), 0)
  expect_length(color(rainbow(1)), 1)
  expect_length(color(rainbow(10)), 10)
})

test_that("colour() perserves length", {
  expect_length(colour(rainbow(0)), 0)
  expect_length(colour(rainbow(1)), 1)
  expect_length(colour(rainbow(10)), 10)
})

test_that("is_color is working", {
  expect_true(is_color(color(rainbow(10))))
  expect_false(is_color(rainbow(10)))
})

test_that("plotting returns the data invisibly", {
  expect_invisible(plot(color(rainbow(10))))
  expect_invisible(plot(colour(rainbow(10))))
})

test_that("printing works", {
  expect_output(print(color(rainbow(10))), "<colors>")
  expect_output(print(colour(rainbow(10))), "<colors>")
})

test_that("subsetting works", {
  colors <- color(rainbow(10))

  expect_length(colors[1:4], 4)
  expect_s3_class(colors[6:8], "colors")
})

test_that("color() complains when col type is wrong.", {
  expect_error(color("not a color"))
  expect_error(color(list(pal = "#000000")))
})

test_that("colour() complains when col type is wrong.", {
  expect_error(colour("not a color"))
  expect_error(colour(list(pal = "#000000")))
})
