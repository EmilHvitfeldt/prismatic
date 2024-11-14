#' Negate colors in RGB space
#'
#' @inheritParams color
#'
#' @details The negation of color is happening in the red-green-blue colorspace
#'  RGB. This means if we take the specification for orange which is
#'  rgb(255, 165, 0), then we negate by taking the opposite number on the scale
#'  from 0 to 255, leaving us with rgb(0, 90, 255), which is a shade of blue.
#'
#' @return A `colors` object of the same length as `col`.
#' @export
#'
#' @examples
#' clr_negate("orange")
#'
#' terr <- color(terrain.colors(10))
#'
#' terr
#' clr_negate(terr)
#'
#' plot(terr)
#' plot(clr_negate(terr))
clr_negate <- function(col) {
  col <- color(col)
  color(encode_colour(abs(decode_colour(col) - 255)))
}
