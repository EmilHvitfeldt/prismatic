#' Mix color into color(s)
#'
#'
#' @inheritParams color
#' @param mix_in Same as `col`.
#' @param ratio Numeric between 0 and 1. 0 will result in `col`. 1 results in
#'  all the colors turning to `mix_in`. Must be of length 1 or the same length
#'  as `col`.
#'
#' @return A `colors` object of the same length as `col`.
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
  if (length(mix_in) != 1) {
    stop("`mix_in` must be of length 1.")
  }

  if (!(length(ratio) == 1 || (length(ratio) == length(col)))) {
    stop("`ratio` must be of length 1 or the same length as `col`.")
  }

  if (!all(ratio >= 0 & ratio <= 1)) {
    stop("`ratio` must be between 0 and 1.")
  }

  col <- color(col)
  mix_in <- color(mix_in)

  ratio_mat <- matrix(ratio, nrow = 3, ncol = length(col), byrow = TRUE)
  rgb <- pro_transform(t(decode_colour(col)), rowSums(col2rgb(mix_in)), ratio_mat)

  color(encode_colour(t(rgb)))
}

