#' Make a color more light
#'
#' @details The colors will be trainsformed to HSL color space (hue, saturation,
#'  lightness) where the lightness of the color will  be modified. The lightness
#'  of a color takes a value between 0 and 1, with 0 being black and 1 being
#'  white. The \code{shift} argument takes a value between 0 and 1, where 0
#'  means that the lightness stays unchanged and 1 means completely white. As an
#'  example, if the lightness of the color is 0.6 and shift is 0.5, then the
#'  lightness be set to the halfway point between 0.6 and 1 which is 0.8.
#'
#' @source \url{https://en.wikipedia.org/wiki/HSL_and_HSV}
#'
#' @inheritParams color
#' @param shift Numeric between 0 and 1, 0 will do zero lightening, 1 will do
#'    complete lightening turning the color to white. Defaults to 0.5.
#'
#' @return a color object of same length as col.
#' @export
#'
#' @seealso clr_darken
#' @examples
#' # Using linear shift
#' plot(clr_lighten(rep("red", 11), shift = seq(0, 1, 0.1)))
#'
#' # Using exponential shifts
#' plot(clr_lighten(rep("red", 11), shift = log(seq(1, exp(1), length.out = 11))))
clr_lighten <- function(col, shift = 0.5) {
  hsl <- farver::convert_colour(t(col2rgb(col)), "rgb", "hsl")
  hsl[, 3] <- pro_transform(hsl[, 3], 100, shift)

  rgb <- t(farver::convert_colour(hsl, "hsl", "rgb"))
  color(rgb2col(rgb_norn(rgb)))
}

#' Make a color more dark
#'
#' @details The colors will be trainsformed to HSL color space (hue, saturation,
#'  lightness) where the lightness of the color will  be modified. The lightness
#'  of a color takes a value between 0 and 1, with 0 being black and 1 being
#'  white. The \code{shift} argument takes a value between 0 and 1, where 0
#'  means that the lightness stays unchanged and 1 means completely black. As an
#'  example, if the lightness of the color is 0.6 and shift is 0.5, then the
#'  lightness be set to the halfway point between 0.6 and 0, which is 0.3.
#'
#' @source \url{https://en.wikipedia.org/wiki/HSL_and_HSV}
#'
#' @inheritParams color
#' @param shift Numeric between 0 and 1, 0 will do zero darkening, 1 will do
#'    complete darkening turning the color to black. Defaults to 0.5.
#'
#' @return a color object of same length as col.
#' @export
#'
#' @seealso clr_lighten
#' @examples
#' # Using linear shift
#' plot(clr_darken(rep("red", 11), shift = seq(0, 1, 0.1)))
#'
#' # Using exponential shifts
#' plot(clr_darken(rep("red", 11), shift = log(seq(1, exp(1), length.out = 11))))
clr_darken <- function(col, shift = 0.5) {
  hsl <- farver::convert_colour(t(col2rgb(col)), "rgb", "hsl")
  hsl[, 3] <- pro_transform(hsl[, 3], 0, shift)

  rgb <- t(farver::convert_colour(hsl, "hsl", "rgb"))
  color(rgb2col(rgb_norn(rgb)))
}
