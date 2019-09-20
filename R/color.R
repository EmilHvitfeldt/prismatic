#' Turn vector to color vector
#'
#' @details Alpha values will be automatically  added to hexcodes. If none at
#' present it will default to no alpha (FF).
#'
#' @param col a color object or vector of any of the three kinds of R color
#' specifications, i.e., either a color name (as listed by colors()), a
#' hexadecimal string of the form "#rrggbb" or "#rrggbbaa" (see rgb), or a
#' positive integer i meaning palette()[i].
#'
#' @return a color object.
#' @export
#'
#' @examples
#' terrain_10 <- color(terrain.colors(10))
#'
#' terrain_10[1:4]
#'
#' plot(terrain_10)
color <- function(col) {
  if (is.list(col)) stop("`col` must not be a list.")
  if (length(col) < 1) stop("The length of `col` must be positive.")
  col <- rgb2col(col2rgb(col, alpha = TRUE), alpha = TRUE)
  attr(col, "class") <- "color"
  col
}

#' Test if the object is a color
#'
#' @param x An object
#'
#' @return TRUE if the object inherits from the color class.
#' @export
is_color <- function(x) {
  inherits(x, "color")
}

#' @export
`[.color` <- function(x, i) {
  x <- unclass(x)
  color(x[i])
}

#' @export
print.color <- function(x, ...) {
  cat("<color>\n")
  print(unclass(x))
}

#' @export
#' @importFrom graphics plot rect
plot.color <- function(x, ...) {
  plot(0,type='n',axes=FALSE,ann=FALSE, xlim = c(0, length(x) + 1), ylim = c(-0.1, 1.1))
  rect(xleft = seq_along(x) - 0.5, ybottom = 0, xright = seq_along(x) + 0.5,
       ytop = 1, col = x, border = NA)
  rect(xleft = 0.5, ybottom = 0, xright = length(x) + 0.5, ytop = 1)
}
