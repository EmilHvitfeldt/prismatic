#' Modify Individual HCL Axes
#'
#' This function lets you modify individual axes of a color in HCL color space.
#'
#' The expression used in `h`, `c`, and `l` is evaluated in the `hcl` space and
#' and you have access to `h`, `c`, and `l` as vectors along with vectors in the
#' calling environment.
#'
#' `h` ranges from 0 to 360, `l` ranges from 0 to 100, and `c` while depended on
#' `h` and `l` will roughly be within 0 and 180, but often on a narrower range.
#' Colors after modification will be adjusted to fit within the color space.
#'
#' @inheritParams color
#' @param h Expression to modify the hue of `col`
#' @param c Expression to modify the chroma of `col`
#' @param l Expression to modify the luminance of `col`
#'
#' @source \url{https://en.wikipedia.org/wiki/HCL_color_space}
#'
#' @return a colors object.
#' @export
#'
#' @examples
#' plot(modify_hcl("red", h = 160))
#' plot(modify_hcl("red", h = h + 50))
#'
#' plot(modify_hcl("red", h = h + 1:100))
#' plot(modify_hcl("red", c = c - 1:200))
#' plot(modify_hcl("red", l = l + 1:50))
#'
#' plot(modify_hcl(rainbow(10), l = 25))
#'
#' plot(modify_hcl(rainbow(10), h + h / 2, l = 70))
modify_hcl <- function(col, h, c, l) {
  hcl <- as.data.frame(decode_colour(col, to = "hcl"))

  if (!missing(h)) {
    h <- eval(substitute(h), envir = hcl)
    h <- h %% 360
  } else {
    h <- hcl$h
  }
  if (!missing(l)) {
    l <- eval(substitute(l), envir = hcl)
    l <- pmin(100, pmax(0, l))
  } else {
    l <- hcl$l
  }
  if (!missing(c)) {
    c <- eval(substitute(c), envir = hcl)
  } else {
    c <- hcl$c
  }

  c <- pmin(max_chroma(h, l, floor = TRUE), pmax(0, c))

  color(encode_colour(cbind(h, c, l), from = "hcl"))
}
