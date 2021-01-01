#' Contrast Ratio Between Colors
#'
#' Calculates the contrast ratio between `x` and the colors `y`. Contrast ratios
#' can range from 1 to 21 with 1 being no contrast (same color) and 21 being
#' highest contrast.
#'
#' The formula for calculating contract ratio is
#'
#' \deqn{(L1 + 0.05) / (L2 + 0.05)}
#'
#' where
#'
#' \itemize{
#' \item L1 is the relative luminance of the lighter of the colors, and
#' \item L2 is the relative luminance of the darker of the colors.
#' }
#'
#' Relative luminance is calculated according to
#' \url{https://www.w3.org/TR/2008/REC-WCAG20-20081211/#relativeluminancedef}.
#'
#' @param x A color object or vector of length 1 of any of the three kinds of R
#' color specifications, i.e., either a color name (as listed by colors()), a
#' hexadecimal string of the form "#rrggbb" or "#rrggbbaa" (see rgb), or a
#' positive integer i meaning palette()[i].
#' @param y A color object or vector of any of the three kinds of R color
#' specifications, i.e., either a color name (as listed by colors()), a
#' hexadecimal string of the form "#rrggbb" or "#rrggbbaa" (see rgb), or a
#' positive integer i meaning palette()[i].
#'
#' @return The elements of `y` with highest contrast to `x`.
#' @export
#'
#' @source \url{https://www.w3.org/TR/UNDERSTANDING-WCAG20/visual-audio-contrast-contrast.html}
#' @examples
#' contrast_ratio("red", "blue")
#' contrast_ratio("grey20", grey.colors(10))
#' contrast_ratio("white", c("white", "black"))
contrast_ratio <- function(x, y) {
  if (length(x) != 1) {
    stop(paste0("`x` must have length 1. Length was: ", length(x)))
  }
  x_l <- rel_l(x)
  y_l <- rel_l(y)

  res <- (pmax(x_l, y_l) + 0.05) / (pmin(x_l, y_l) + 0.05)
  unname(res)
}
