#' Rotate the colors around the hue wheel
#'
#' @details The colors will be trainsformed to HSL color space (hue, saturation,
#'  lightness) where the hue of the color will be rotation.
#'
#' @source \url{https://en.wikipedia.org/wiki/HSL_and_HSV}
#'
#' @inheritParams color
#' @param degrees Numeric between 0 and 360, denotion the about of degrees the
#' colors should be rotated.
#'
#' @return a colors object of same length as col.
#' @export
#'
#' @examples
#'
#' plot(clr_rotate(terrain.colors(10), degrees = 90))
#'
#' plot(clr_rotate(terrain.colors(10), degrees = 180))
#'
#' plot(clr_rotate(rep("magenta", 11), degrees = seq(0, 360, length.out =  11)))
clr_rotate <- function(col, degrees) {
  col <- color(col)
  if (!(length(degrees) == 1 || (length(degrees) == length(col)))) {
    stop("`degrees` must be of length 1 or the same length as `col`.")
  }
  hsl <- farver::convert_colour(t(col2rgb(col)), "rgb", "hsl")
  hsl[, 1] <- (hsl[, 1] + degrees) %% 360

  rgb <- t(farver::convert_colour(hsl, "hsl", "rgb"))
  color(rgb2col(rgb_norn(rgb)))
}
