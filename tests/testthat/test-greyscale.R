test_that("clr_grayscale() perserves length", {
  expect_length(clr_grayscale(rainbow(0)), 0)
  expect_length(clr_grayscale(rainbow(1)), 1)
  expect_length(clr_grayscale(rainbow(10)), 10)
})

test_that("clr_greyscale() perserves length", {
  expect_length(clr_greyscale(rainbow(0)), 0)
  expect_length(clr_greyscale(rainbow(1)), 1)
  expect_length(clr_greyscale(rainbow(10)), 10)
})

test_that("clr_grayscale()'s output has colors class", {
  expect_s3_class(clr_grayscale(rainbow(10)), "colors")
})

test_that("clr_greyscale()'s output has colors class", {
  expect_s3_class(clr_greyscale(rainbow(10)), "colors")
})

test_that("clr_grayscale() complains when col type is wrong.", {
  expect_error(clr_grayscale("not a color"))
  expect_error(clr_grayscale(list(pal = "#000000")))
})

test_that("clr_greyscale() complains when col type is wrong.", {
  expect_error(clr_greyscale("not a color"))
  expect_error(clr_greyscale(list(pal = "#000000")))
})

test_that("result is grayscale", {
  methods <- c(
    "luma", "averaging", "min_decomp",
    "max_decomp", "red_channel",
    "green_channel", "blue_channel"
  )

  for (method in methods) {
    res <- col2rgb(clr_grayscale(rainbow(10), method))

    expect_equal(res[1, ], res[2, ])
    expect_equal(res[1, ], res[3, ])
  }
})

test_that("errors when method is wrongly specified", {
  expect_error(clr_grayscale(rainbow(10), "111"))
})
