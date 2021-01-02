#' Rotate the colors around the hue wheel
#'
#' @details The colors will be transformed to HCL color space
#' (Hue-Chroma-Luminance) where the hue of the color will be rotation.
#'
#' @source \url{https://en.wikipedia.org/wiki/HCL_color_space}
#'
#' @inheritParams color
#' @param degrees Numeric between 0 and 360, denoting the amount of degrees the
#' colors should be rotated. Defaults to 0.
#'
#' @return a colors object of same length as col.
#' @export
#'
#' @examples
#' plot(clr_rotate(terrain.colors(10)))
#'
#' plot(clr_rotate(terrain.colors(10), degrees = 90))
#'
#' plot(clr_rotate(terrain.colors(10), degrees = 180))
#'
#' plot(clr_rotate(rep("magenta", 11), degrees = seq(0, 360, length.out = 11)))
clr_rotate <- function(col, degrees = 0) {
  col <- color(col)
  if (!(length(degrees) == 1 || (length(degrees) == length(col)))) {
    stop("`degrees` must be of length 1 or the same length as `col`.")
  }
  hcl <- decode_colour(col, to = "hcl")
  hcl[, 1] <- (hcl[, 1] + degrees) %% 360

  rgb <- convert_colour(hcl, "hcl", "rgb")
  color(encode_colour(rgb_norm(rgb)))
}
