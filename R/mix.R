#' Mixes a color into
#'
#' @param mix_in A single color any of the three kinds of R color
#'     specifications, i.e., either a color name (as listed by colors()), a
#'     hexadecimal string of the form "#rrggbb" or "#rrggbbaa" (see rgb), or a
#'     positive integer i meaning palette()[i].
#' @param ratio Numeric between 0 and 1. 0 will result on no mixing. 1 results
#'     in all the colors turning to mix_in.
#' @inheritParams color
#'
#' @return a colors object
#' @export
#'
#' @examples
#' plot(clr_mix(rainbow(10), "blue"))
#'
#' plot(clr_mix(rainbow(10), "red"))
#'
#' plot(clr_mix(rainbow(10), "#5500EE"))
clr_mix <- function(col, mix_in, ratio = 0.5) {
  col <- color(col)
  mix_in <- color(mix_in)
  if (length(mix_in) != 1) {
    stop("`mix_in` must be of length 1.")
  }
  rgb <- pro_transform(col2rgb(col), rowSums(col2rgb(mix_in)), ratio)

  color(rgb2col(rgb))
}
