#' Contrast ratio between colors
#'
#' `contrast_ratio()` calculates the contrast ratio between the color `x` and
#' the color(s) `y`. Contrast ratios can range from 1 to 21 with 1 being no
#' contrast (i.e., same color) and 21 being highest contrast.
#'
#' @details
#' The formula used for calculating a contrast ratio between two colors is
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
#' \url{https://www.w3.org/TR/WCAG21/#dfn-relative-luminance}.
#'
#' @param x A length 1 color object (see [color()]) or a length 1 vector of any
#'  of the three kinds of R color specifications, i.e., either a color name (as
#'  listed by [grDevices::colors()]), a hexadecimal string (see [col2rgb()]), or
#'  a positive integer `i` meaning [grDevices::palette()]`[i]`.
#' @param y A color object (see [color()]) or a vector of any of the three kinds
#'  of R color specifications, i.e., either a color name (as listed by
#'  [grDevices::colors()]), a hexadecimal string (see [col2rgb()]), or a
#'  positive integer `i` meaning [grDevices::palette()]`[i]`.
#'
#' @return A numerical vector of the same length as `y` of the calculated
#'  contrast ratios.
#' @export
#'
#' @source \url{https://www.w3.org/TR/UNDERSTANDING-WCAG20/visual-audio-contrast-contrast.html}
#' @examples
#' contrast_ratio("red", "blue")
#' contrast_ratio("grey20", grey.colors(10))
#' contrast_ratio("white", c("white", "black"))
contrast_ratio <- function(x, y) {
  if (length(x) != 1) {
    stop(paste0("`x` must have length 1. Length was: ", length(x), "."))
  }
  x_l <- rel_l(x)
  y_l <- rel_l(y)

  res <- (pmax(x_l, y_l) + 0.05) / (pmin(x_l, y_l) + 0.05)
  unname(res)
}

#' Find highest contrast color
#'
#' `best_contrast()` finds the color in `y` with the highest contrast to the
#' color `x`.
#'
#' @param x A vector of colors as described in `col` of [color()]. Must not
#'  contain any `NA`.
#' @param y A vector of colors as described in `col` of [color()]. Must not
#'  contain any `NA`.
#'
#' @return A vector of the same length as `x` with, for each element of `x`, the
#'  element of `y` that has the highest contrast to `x`.
#' @export
#'
#' @examples
#' best_contrast("red")
#' best_contrast("grey20")
#' best_contrast("white")
#'
#' best_contrast(rainbow(10), rainbow(3))
best_contrast <- function(x, y = c("#010101", "#FFFFFF")) {
  if (any(is.na(x)) || any(is.na(y))) {
    stop("`x` and `y` must not contain any `NA`.")
  }

  if (length(unique(y)) != length(y)) {
    stop("Elements in `y` must be unique.")
  }

  constracts <- sapply(x, contrast_ratio, y)
  y[apply(constracts, 2, function(x) which(max(x) == x))]
}

# Source: https://www.w3.org/TR/WCAG21/#dfn-relative-luminance
rel_l <- function(x) {
  scale <- function(x) {
    ifelse(x <= 0.04045, x / 12.92, ((x + 0.055) / 1.055)^2.4)
  }
  rgb <- decode_colour(x) / 255
  0.2126 * scale(rgb[, 1]) + 0.7152 * scale(rgb[, 2]) + 0.0722 * scale(rgb[, 3])
}
