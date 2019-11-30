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

  expect_error(clr_(rainbow(0)))
  })
}

test_length <- function(clr_) {
  test_that(paste0("test_length: ",
                   deparse(substitute(clr_)),
                   "() perserves length"), {
    expect_length(clr_(rainbow(1)), 1)
    expect_length(clr_(rainbow(10)), 10)
  })
}

test_color_class <- function(clr_) {
  test_that(paste0("test_color_class: ",
                   deparse(substitute(clr_)),
                   "()'s output has colors class"), {
    expect_s3_class(clr_(rainbow(10)), "colors")
  })
}

test_severity <- function(clr_) {
  test_that(paste0("test_severity: ",
                   deparse(substitute(clr_)),
                   "()' if the length  of `severity` isn't 1"), {
    expect_visible(clr_(rainbow(10), rep(1, 1)))
    expect_error(clr_(rainbow(10), seq(0, 1, length.out = 2)))
    expect_error(clr_(rainbow(10), seq(0, 1, length.out = 3)))
    expect_visible(clr_(rainbow(10), seq(0, 1, length.out = 10)))
  })
}

test_severity_1 <- function(clr_) {
  test_that(paste0("test_severity_1: ",
                   deparse(substitute(clr_)),
                   "()' if the length  of `severity` isn't 1"), {
    expect_visible(clr_(rainbow(10), rep(1, 1)))
    expect_error(clr_(rainbow(10), seq(0, 1, length.out = 2)))
    expect_error(clr_(rainbow(10), seq(0, 1, length.out = 3)))
    expect_error(clr_(rainbow(10), seq(0, 1, length.out = 10)))
  })
}

test_severity_range <- function(clr_) {
  test_that(paste0("test_severity_range: ",
                   deparse(substitute(clr_)),
                   "()' setting severity outside range gives error"), {
    expect_error(clr_(rainbow(10), severity = -1))
    expect_error(clr_(rainbow(10), severity = 2))
  })
}

test_severity_0 <- function(clr_, tol = 0) {
  test_that(paste0("test_severity_0: ",
                   deparse(substitute(clr_)),
                   "() setting severity = 0 leaves input unchanged"), {
   expect_equal_color(clr_(rainbow(10), 0), color(rainbow(10)), tol)
 })
}
