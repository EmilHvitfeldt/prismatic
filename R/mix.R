#' Mixes a color into
#'
#' @param mix_in A single color any of the three kinds of R color
#'     specifications, i.e., either a color name (as listed by colors()), a
#'     hexadecimal string of the form "#rrggbb" or "#rrggbbaa" (see rgb), or a
#'     positive integer i meaning palette()[i].
#' @param ratio Numeric between 0 and 1. 0 will result on no mixing. 1 results
#'     in all the colors turning to mix_in. Must be of length 1 or same length
#'     as col.
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
#'
#' plot(clr_mix(rainbow(10), "black", seq(1, 0, length.out = 10)))
clr_mix <- function(col, mix_in, ratio = 0.5) {
  col <- color(col)
  mix_in <- color(mix_in)
  if (length(mix_in) != 1) {
    stop("`mix_in` must be of length 1.")
  }

  if (!(length(ratio) == 1 || (length(ratio) == length(col)))) {
    stop("`ratio` must be of length 1 or the same length as `col`.")
  }

  ratio_mat <- matrix(ratio, nrow = 3, ncol = length(col), byrow = TRUE)
  rgb <- pro_transform(t(decode_colour(col)), rowSums(col2rgb(mix_in)), ratio_mat)

  color(encode_colour(t(rgb)))
}
