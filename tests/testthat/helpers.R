expect_equal_color <- function(object, expected, tol = 0) {
  res <- all(abs(col2rgb(object) - col2rgb(expected)) <= tol)
  expect_true(res)
}

test_wrong_input <- function(clr_) {
  test_that(paste0("test_wrong_input: ",
                   deparse(substitute(clr_)),
                   "() complains when col type is wrong."), {
  expect_error(clr_("not a color"))

  expect_error(clr_(list(pal = "#000000")))

  expect_error(clr_("pinkly"))
  })
}
