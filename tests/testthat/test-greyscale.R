test_length(clr_grayscale)
test_length(clr_greyscale)

test_color_class(clr_grayscale)
test_color_class(clr_greyscale)

test_wrong_input(clr_grayscale)
test_wrong_input(clr_greyscale)

test_zero_length_input(clr_grayscale)
test_zero_length_input(clr_greyscale)

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
