test_color_class(color)
test_color_class(colour)

test_length(color)
test_length(colour)

test_zero_length_input(color)
test_zero_length_input(colour)

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

test_wrong_input(color)
test_wrong_input(colour)
