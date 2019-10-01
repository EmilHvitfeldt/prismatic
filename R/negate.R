#' Negates colors in RGB space
#'
#' @inheritParams color
#'
#' @details The negation of color is happening in the red-green-blue colorspace
#' RGB. Meaning that if we take the specification for Orange which is
#' rgb(255, 165, 0), then we negate by taking the oppesite number on the scale
#' from 0 to 255, leaving us wih rgb(0, 90, 255) which is a shade of blue.
#'
#' @return a colors object of same length as col.
#' @export
#'
#' @importFrom grDevices col2rgb rgb
#'
#' @examples
#' terr <- color(terrain.colors(10))
#'
#' terr
#' clr_negate(terr)
#'
#' plot(terr)
#' plot(clr_negate(terr))
clr_negate <- function(col) {
  col <- color(col)
  value <- abs(col2rgb(col) - 255) / 255
  color(rgb(value[1, ], value[2, ], value[3, ]))
}
