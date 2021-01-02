#' Make a color more saturated
#'
#' @details The colors will be transformed to HSL color space (hue, saturation,
#'  lightness) where the saturation of the color will  be modified.
#'  The saturation of a color takes a value between 0 and 1, with 0 being black
#'  and 1 being white. The \code{shift} argument takes a value between 0 and 1,
#'  where 0 means that the saturation stays unchanged and 1 means completely
#'  saturated. As an example, if the saturation of the color is 0.6 and shift is
#'  0.5, then the saturation be set to the halfway point between 0.6 and 1 which
#'  is 0.8.
#'
#' @source \url{https://en.wikipedia.org/wiki/HSL_and_HSV}
#'
#' @inheritParams color
#' @param shift Numeric between 0 and 1, 0 will do zero saturation, 1 will do
#'    complete saturation. Defaults to 0.5.
#'
#' @return a color object of same length as col.
#' @export
#'
#' @seealso clr_desaturate
#' @examples
#'
#' plot(clr_saturate(terrain.colors(10), shift = 0.5))
#'
#' plot(clr_saturate(terrain.colors(10), shift = 1))
#'
#' plot(clr_saturate(rep("firebrick", 11), shift = seq(0, 1, 0.1)))
clr_saturate <- function(col, shift = 0.5) {
  col <- color(col)
  if (!(length(shift) == 1 || (length(shift) == length(col)))) {
    stop("`shift` must be of length 1 or the same length as `col`.")
  }

  hsl <- decode_colour(col, to = "hsl")
  hsl[, 2] <- pro_transform(hsl[, 2], 100, shift)

  rgb <- convert_colour(hsl, "hsl", "rgb")
  color(encode_colour(rgb_norm(rgb)))
}

#' Make a color more desaturated
#'
#' @details The colors will be transformed to HSL color space (hue, saturation,
#'  lightness) where the saturation of the color will  be modified. The
#'  saturation of a color takes a value between 0 and 1, with 0 being black and
#'  1 being white. The \code{shift} argument takes a value between 0 and 1,
#'  where 0 means that the saturation stays unchanged and 1 means completely
#'  desaturated. As an example, if the saturation of the color is 0.6 and shift
#'  is 0.5, then the saturation be set to the halfway point between 0.6 and 0
#'  which is 0.3.
#'
#' @source \url{https://en.wikipedia.org/wiki/HSL_and_HSV}
#'
#' @inheritParams color
#' @param shift Numeric between 0 and 1, 0 will do zero desaturation, 1 will do
#'    complete desaturation. Defaults to 0.5.
#'
#' @return a colors object of same length as col.
#' @export
#'
#' @seealso clr_saturate
#' @examples
#'
#' plot(clr_desaturate(terrain.colors(10), shift = 0.5))
#'
#' plot(clr_desaturate(terrain.colors(10), shift = 0.9))
#'
#' plot(clr_desaturate(rep("firebrick", 11), shift = seq(0, 1, 0.1)))
clr_desaturate <- function(col, shift = 0.5) {
  col <- color(col)
  if (!(length(shift) == 1 || (length(shift) == length(col)))) {
    stop("`shift` must be of length 1 or the same length as `col`.")
  }

  hsl <- decode_colour(col, to = "hsl")
  hsl[, 2] <- pro_transform(hsl[, 2], 0, shift)

  rgb <- convert_colour(hsl, "hsl", "rgb")
  color(encode_colour(rgb_norm(rgb)))
}
