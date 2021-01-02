#' Simulate color vision deficiency
#'
#' @details The matrices uses to perform transformations have been taken as the
#'  1.0 value in table 1 in \url{http://www.inf.ufrgs.br/~oliveira/pubs_files/CVD_Simulation/CVD_Simulation.html}.
#'
#' @rdname colorblindness
#'
#' @inheritParams color
#' @param severity A numeric, Severity of the color vision defect, a number
#' between 0 and 1. 0 means no deficiency, 1 means complete deficiency. Defaults
#'  to 1.
#'
#' @return a colors object of same length as col.
#' @export
#'
#' @source \url{http://www.inf.ufrgs.br/~oliveira/pubs_files/CVD_Simulation/CVD_Simulation.html}
#'
#' @references
#'  Gustavo M. Machado, Manuel M. Oliveira, and Leandro A. F. Fernandes "A
#'  Physiologically-based Model for Simulation of Color Vision Deficiency". IEEE
#'  Transactions on Visualization and Computer Graphics. Volume 15 (2009),
#'  Number 6, November/December 2009. pp. 1291-1298.
#'
#' @examples
#' rainbow_colors <- color(rainbow(10))
#'
#' plot(clr_protan(rainbow_colors))
#' plot(clr_deutan(rainbow_colors))
#' plot(clr_tritan(rainbow_colors))
#'
#' viridis_colors <- c(
#'   "#4B0055FF", "#422C70FF", "#185086FF", "#007094FF",
#'   "#008E98FF", "#00A890FF", "#00BE7DFF", "#6CD05EFF",
#'   "#BBDD38FF", "#FDE333FF"
#' )
#'
#' plot(clr_protan(viridis_colors))
#' plot(clr_deutan(viridis_colors))
#' plot(clr_tritan(viridis_colors))
clr_protan <- function(col, severity = 1) {
  col <- color(col)
  range_check(severity)
  if (!(length(severity) == 1)) {
    stop("`severity` must be of length 1.")
  }

  rgb <- decode_colour(col) %*%
    t((diag(3) * (1 - severity) + protan_matrix * (severity)))
  color(encode_colour(rgb_norm(rgb)))
}

#' @rdname colorblindness
#' @export
clr_deutan <- function(col, severity = 1) {
  col <- color(col)
  range_check(severity)
  if (!(length(severity) == 1)) {
    stop("`severity` must be of length 1.")
  }

  rgb <- decode_colour(col) %*%
    t((diag(3) * (1 - severity) + deutan_matrix * (severity)))
  color(encode_colour(rgb_norm(rgb)))
}

#' @rdname colorblindness
#' @export
clr_tritan <- function(col, severity = 1) {
  col <- color(col)
  range_check(severity)
  if (!(length(severity) == 1)) {
    stop("`severity` must be of length 1.")
  }

  rgb <- decode_colour(col) %*%
    t((diag(3) * (1 - severity) + tritan_matrix * (severity)))
  color(encode_colour(rgb_norm(rgb)))
}

range_check <- function(x) {
  if (!all(x >= 0 & x <= 1)) {
    stop("`severity` must be between 0 and 1.")
  }
}

protan_matrix <- matrix(
  nrow = 3, byrow = TRUE,
  c(
    0.152286, 1.052583, -0.204868,
    0.114503, 0.786281, 0.099216,
    -0.003882, -0.048116, 1.051998
  )
)

deutan_matrix <- matrix(
  nrow = 3, byrow = TRUE,
  c(
    0.367322, 0.860646, -0.227968,
    0.280085, 0.672501, 0.047413,
    -0.011820, 0.042940, 0.968881
  )
)

tritan_matrix <- matrix(
  nrow = 3, byrow = TRUE,
  c(
    1.255528, -0.076749, -0.178779,
    -0.078411, 0.930809, 0.147602,
    0.004733, 0.691367, 0.303900
  )
)

#' Visualize color vision deficiency
#'
#' @param col a color object or vector of any of the three kinds of R color
#' specifications, i.e., either a color name (as listed by colors()), a
#' hexadecimal string of the form "#rrggbb" or "#rrggbbaa" (see rgb), or a
#' positive integer i meaning palette()[i].
#'
#' This function will showcase the effect of all 3 kinds of color vision
#' deficiency at the same time side by side.
#'
#' @return Nothing
#' @export
#'
#' @examples
#' check_color_blindness(rainbow(10))
#'
#' check_color_blindness(terrain.colors(10))
check_color_blindness <- function(col) {
  plot(NULL,
    xlim = c(-0.1, 4.1), ylim = c(0, length(col) + 2),
    xaxs = "i", yaxs = "i", mar = rep(0, 4), axes = FALSE, ann = FALSE
  )

  rect(
    ybottom = seq_along(col) - 0.5, ytop = seq_along(col) + 0.5,
    xleft = 3.1, xright = 3.9, col = clr_tritan(col), border = NA
  )
  rect(
    ybottom = seq_along(col) - 0.5, ytop = seq_along(col) + 0.5,
    xleft = 2.1, xright = 2.9, col = clr_protan(col), border = NA
  )
  rect(
    ybottom = seq_along(col) - 0.5, ytop = seq_along(col) + 0.5,
    xleft = 1.1, xright = 1.9, col = clr_deutan(col), border = NA
  )
  rect(
    ybottom = seq_along(col) - 0.5, ytop = seq_along(col) + 0.5,
    xleft = 0.1, xright = 0.9, col = col, border = NA
  )

  rect(ybottom = 0.5, xleft = 0.1, ytop = length(col) + 0.5, xright = 0.9)
  rect(ybottom = 0.5, xleft = 1.1, ytop = length(col) + 0.5, xright = 1.9)
  rect(ybottom = 0.5, xleft = 2.1, ytop = length(col) + 0.5, xright = 2.9)
  rect(ybottom = 0.5, xleft = 3.1, ytop = length(col) + 0.5, xright = 3.9)

  text(
    x = 1:4 - 0.5, y = length(col) + 1,
    labels = c("Normal", "Deuteranopia", "Protanopia", "Tritanopia")
  )
}
