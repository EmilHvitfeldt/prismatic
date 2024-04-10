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
#' grey_10 <- color(gray.colors(10, start = 0, end = 1))
#'
#' grey_10
#'
#' plot(grey_10, labels = TRUE)
color <- function(col) {
  if (is.list(col)) stop("`col` must not be a list.")
  if (length(col) < 0) stop("The length of `col` must be positive.")
  colors <- rgb2col(col2rgb(col, alpha = TRUE), alpha = TRUE)
  if (has_names(col)) {
    names(colors) <- names(col)
  }
  attr(colors, "class") <- "colors"
  colors
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
plot.colors <- function(x, labels = FALSE, ...) {
  plot(0,
    type = "n", axes = FALSE, ann = FALSE, xlim = c(0, length(x) + 1),
    ylim = c(-0.1, 1.1), mar = rep(0, 4)
  )
  rect(
    xleft = seq_along(x) - 0.5, ybottom = 0, xright = seq_along(x) + 0.5,
    ytop = 1, col = x, border = NA
  )
  if (is.logical(labels)) {
    color_labels <- if (has_names(x)) names(x) else x
    show_labels <- isTRUE(labels)
  } else {
    stopifnot(
      "`labels` must be a character" = is.character(labels),
      "`labels` must be the same length as `x`" = length(x) == length(labels)
    )
    color_labels <- labels
    show_labels <- TRUE
  }
  if (show_labels) {
    # Fill missing color labels with the color hex value
    color_labels[!nzchar(color_labels)] <- x[!nzchar(color_labels)]
    label_col <- vapply(x, best_contrast, FUN.VALUE = character(1))
    text(x = seq_along(x), y = 0.5, labels = color_labels, srt = 90, col = label_col)
  }
  rect(xleft = 0.5, ybottom = 0, xright = length(x) + 0.5, ytop = 1)
}

color_styler <- function(x) {
  text <- cli::make_ansi_style(best_contrast(x), bg = FALSE)
  background <- cli::make_ansi_style(x, bg = TRUE, colors = 256, grey = FALSE)

  cli::combine_ansi_styles(text, background)(x)
}

pretty_print <- function(x) {
  cols <- vapply(x, color_styler, FUN.VALUE = character(1), USE.NAMES = FALSE)
  cat(paste(c(cols, "\n"), collapse = " "))
}

#' @export
print.colors <- function(x, ...) {
  cat("<colors>\n")
  if (requireNamespace("cli", quietly = TRUE)) {
    pretty_print(x)
  } else {
    print(unclass(x))
  }
}
