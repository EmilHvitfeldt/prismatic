
expect_equal_color <- function(object, expected, tol = 0) {
  res <- all(abs(col2rgb(object) - col2rgb(expected)) <= tol)
  expect_true(res)
}
