#' Transform colors to grayscale
#'
#' `clr_grayscale()` has a selection of different methods to turn colors into
#' grayscale.
#'
#' @inheritParams color
#' @param method A character string specifying the grayscaling method. Can be
#'  one of "luma", "averaging", "min_decomp", "max_decomp", "red_channel",
#'  "green_channel" and "blue_channel". Defaults to "luma".
#'
#' @details If `method = "averaging"` then the red, green and blue will be
#'  averaged together to create the grey value. This method does a poor job of
#'  representing the way the human eye sees color. If `method = "luma"` (the
#'  default), a weighted average is used to calculate the grayscale values. The
#'  BT. 709 method from the ITU Radiocommunication Sector have determined the
#'  weights. If `method` is `"min_decomp"` or `"max_decomp"`, then a decomposition
#'  method is used where the minimum or maximum color value have been selected
#'  for the color value. So the color rgb(60, 120, 40) would have the "min_decomp"
#'  value of 40 and "max_decomp" value of 120. If method is `"red_channel"`,
#'  `"green_channel"` or `"blue_channel"`, then the corresponding color channel
#'  will be selected for the values of grayscale.
#'
#' @source \url{https://tannerhelland.com/3643/grayscale-image-algorithm-vb6/}
#' @source \url{https://en.wikipedia.org/wiki/Luma}
#'
#' @rdname clr_grayscale
#'
#' @return A `colors` object of the same length as `col`.
#' @export
#'
#' @examples
#'
#' plot(clr_grayscale(rainbow(10)))
#'
#' plot(clr_grayscale(terrain.colors(10)))
#'
#' viridis_colors <- c(
#'   "#4B0055FF", "#422C70FF", "#185086FF", "#007094FF",
#'   "#008E98FF", "#00A890FF", "#00BE7DFF", "#6CD05EFF",
#'   "#BBDD38FF", "#FDE333FF"
#' )
#'
#' plot(clr_grayscale(viridis_colors, method = "luma"))
#' plot(clr_grayscale(viridis_colors, method = "averaging"))
#' plot(clr_grayscale(viridis_colors, method = "min_decomp"))
#' plot(clr_grayscale(viridis_colors, method = "max_decomp"))
#' plot(clr_grayscale(viridis_colors, method = "red_channel"))
#' plot(clr_grayscale(viridis_colors, method = "green_channel"))
#' plot(clr_grayscale(viridis_colors, method = "blue_channel"))
clr_grayscale <- function(col,
                          method = c("luma", "averaging", "min_decomp",
                                     "max_decomp", "red_channel",
                                     "green_channel", "blue_channel")) {
  method <- match.arg(method)

  col <- color(col)

  colors <- switch(method,
                   luma = grayscale_luma(col),
                   averaging = grayscale_averaging(col),
                   min_decomp = grayscale_decomp(col, min),
                   max_decomp = grayscale_decomp(col, max),
                   red_channel = grayscale_channel(col, "red"),
                   green_channel = grayscale_channel(col, "green"),
                   blue_channel = grayscale_channel(col, "blue")
                   )

  color(colors)
}

#' @rdname clr_grayscale
#' @export
clr_greyscale <- function(col,
                          method = c("luma", "averaging", "min_decomp",
                                     "max_decomp", "red_channel",
                                     "green_channel", "blue_channel")) {
  method <- match.arg(method)
  col <- color(col)
  clr_grayscale(col, method)
}

grayscale_averaging <- function(col) {
  value <- matrix(c(1 / 3, 1 / 3, 1 / 3), nrow = 1) %*% col2rgb(col) / 256
  rgb(value, value, value)
}

grayscale_luma <- function(col) {
  value <- matrix(c(0.2126, 0.7152, 0.0722), nrow = 1) %*% col2rgb(col) / 256
  rgb(value, value, value)
}

grayscale_decomp <- function(col, fun) {
  value <- apply(col2rgb(col) / 256, 2, fun)
  rgb(value, value, value)
}

grayscale_channel <- function(col, channel) {
  value <- (col2rgb(col) / 256)[channel, ]
  rgb(value, value, value)
}
