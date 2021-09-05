#' Extract RGB components
#'
#' @inheritParams color
#'
#' @rdname extract_rgb
#'
#' @details
#' The values of the output will range between 0 and 255.
#'
#' @return Numeric vector of values.
#' @export
#'
#' @examples
#' clr_extract_red(rainbow(100))
#' clr_extract_green(rainbow(100))
#' clr_extract_blue(rainbow(100))
#' clr_extract_alpha(rainbow(100))
clr_extract_red <- function(col) {
  col <- color(col)
  extract_rgb(col)[["red"]]
}

#' @rdname clr_grayscale
#' @export
clr_extract_green <- function(col) {
  col <- color(col)
  extract_rgb(col)[["green"]]
}

#' @rdname clr_grayscale
#' @export
clr_extract_blue <- function(col) {
  col <- color(col)
  extract_rgb(col)[["blue"]]
}

#' @rdname clr_grayscale
#' @export
clr_extract_alpha <- function(col) {
  col <- color(col)
  extract_rgb(col)[["alpha"]]
}

extract_rgb <- function(col) {
  as.data.frame(t(col2rgb(col, alpha = TRUE)))
}
