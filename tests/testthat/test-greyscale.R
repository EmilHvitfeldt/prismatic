test_that("Length is preserved", {
  expect_length(clr_grayscale(rainbow(10)), 10)
  expect_length(clr_greyscale(rainbow(10)), 10)
})

test_that("output has color class", {
  expect_s3_class(clr_grayscale(rainbow(10)), "colors")
  expect_s3_class(clr_greyscale(rainbow(10)), "colors")
})

test_that("result is grayscale", {
  methods <- c("luma", "averaging", "min_decomp",
               "max_decomp", "red_channel",
               "green_channel", "blue_channel")

  for (method in methods) {
    res  <- col2rgb(clr_grayscale(rainbow(10), method))

    expect_equal(res[1, ], res[2, ])
    expect_equal(res[1, ], res[3, ])
  }
})

test_that("errors when method is  wrongly specified", {

  expect_error(clr_grayscale(rainbow(10), "111"))
})

test_that("it complains when col type is wrong.", {
  expect_error(clr_grayscale("not a color"))

  expect_error(clr_grayscale(list(pal = "#000000")))

  expect_error(clr_grayscale(character()))
})


