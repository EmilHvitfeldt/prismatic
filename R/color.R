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
#' @return a colors object.
#' @export
#'
#' @rdname color
#'
#' @examples
#' terrain_10 <- color(terrain.colors(10))
#'
#' terrain_10[1:4]
#'
#' plot(terrain_10)
#'
#' plot(terrain_10, labels = TRUE)
#'
#' plot(color(gray.colors(10)), labels = TRUE)
color <- function(col) {
  if (is.list(col)) stop("`col` must not be a list.")
  if (length(col) < 1) stop("The length of `col` must be positive.")
  col <- rgb2col(col2rgb(col, alpha = TRUE), alpha = TRUE)
  attr(col, "class") <- "colors"
  col
}

#' @rdname color
#' @export
colour <- function(col) {
  color(col)
}

#' Test if the object is a color
#'
#' @param x An object
#'
#' @return TRUE if the object inherits from the color class.
#' @export
is_color <- function(x) {
  inherits(x, "colors")
}

#' @export
`[.colors` <- function(x, i) {
  x <- unclass(x)
  color(x[i])
}

#' @export
#' @importFrom graphics plot rect text
plot.colors <- function(x, labels = FALSE, ...) {
  plot(0, type = "n", axes = FALSE, ann = FALSE, xlim = c(0, length(x) + 1),
       ylim = c(-0.1, 1.1), mar = rep(0, 4))
  rect(xleft = seq_along(x) - 0.5, ybottom = 0, xright = seq_along(x) + 0.5,
       ytop = 1, col = x, border = NA)
  if (labels) {
    color_light <- farver::convert_colour(t(col2rgb(x)), "rgb", "hsl")[, "l"]
    label_col <- ifelse(color_light > 31,
                        "#010101",
                        "#FFFFFF")
    text(x = seq_along(x), y = 0.5, labels = x, srt = 90, col = label_col)
  }
  rect(xleft = 0.5, ybottom = 0, xright = length(x) + 0.5, ytop = 1)
}

color_styler <- function(x) {
  color_lightness <- farver::convert_colour(t(col2rgb(x)), "rgb", "hsl")[, "l"]

  text <- crayon::make_style(
    ifelse(color_lightness > 31,
           "#010101",
           "#FFFFFF"),
    bg = FALSE)

  background <- crayon::make_style(x, bg = TRUE, colors = 256, grey = FALSE)

  crayon::combine_styles(text, background)(x)

}

pretty_print <- function(x) {
  cols <- vapply(x, color_styler, FUN.VALUE = character(1), USE.NAMES = FALSE)
  cat(paste(c(cols, "\n"), collapse = " "))
}

#' @export
print.colors <- function(x, ...) {
  cat("<colors>\n")
  if (requireNamespace("crayon", quietly = TRUE)) {
    pretty_print(x)
  } else {
    print(unclass(x))
  }
}
