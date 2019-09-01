#' Transform colors to greyscale
#'
#' @inheritParams color
#'
#' @return a color object of same length as col.
#' @export
#'
#' @examples
#'
#' plot(clr_grayscale(rainbow(10)))
#'
#' plot(clr_grayscale(terrain.colors(10)))
#'
#' plot(clr_grayscale(hcl.colors(10, palette = "viridis")))
clr_grayscale <- function(col) {
  value <- matrix(c(0.2989, 0.5870, 0.1140), nrow = 1) %*% col2rgb(col) / 256
  color(rgb(value, value, value))
}
