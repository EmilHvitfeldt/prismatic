#' Set alpha in color
#'
#' @inheritParams color
#' @param alpha Numeric between 0 and 1. 0 will result in full transparency and
#'  1 in no transparency.
#'
#' @return A `colors` object of the same length as `col`.
#' @export
#'
#' @examples
#' plot(clr_alpha(rainbow(10), 0.5))
#'
#' plot(clr_alpha(rainbow(10), 0.2))
#'
#' plot(clr_alpha(rainbow(10), seq(0, 1, length.out = 10)))
clr_alpha <- function(col, alpha = 0.5) {
  if (!(length(alpha) == 1 || (length(alpha) == length(col)))) {
    stop("`alpha` must be of length 1 or the same length as `col`.")
  }

  if (!all(alpha >= 0 & alpha <= 1)) {
    stop("`alpha` must be between 0 and 1.")
  }

  col <- color(col)
  rgba <- decode_colour(col)
  color(encode_colour(rgba, alpha = alpha))
}
