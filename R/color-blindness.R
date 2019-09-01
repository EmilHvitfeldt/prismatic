#' Simulate color vision deficiency
#'
#' @rdname colorblindness
#'
#' @inheritParams color
#' @param severity A numeric, Severity of the color vision defect, a number
#' between 0 and 1. 0 means no deficiency, 1 means complete deficiency. Defaults
#'  to 1.
#'
#' @return a color object of same length as col.
#' @export
#'
#' @examples
#' rainbow_colors <- color(rainbow(10))
#'
#' plot(clr_protan(rainbow_colors))
#' plot(clr_deutan(rainbow_colors))
#' plot(clr_tritan(rainbow_colors))
#'
#' viridis_colors <- color(hcl.colors(10, palette = "viridis"))
#'
#' plot(clr_protan(viridis_colors))
#' plot(clr_deutan(viridis_colors))
#' plot(clr_tritan(viridis_colors))
clr_protan <- function(col, severity = 1) {
  rgb <- (diag(3)*(1-severity) + protan_matrix * (severity)) %*% col2rgb(col)
  color(rgb2col(rgb_norn(rgb)))
}

#' @rdname colorblindness
#' @export
clr_deutan <- function(col, severity = 1) {
  rgb <- (diag(3)*(1-severity) + deutan_matrix * (severity)) %*% col2rgb(col)
  color(rgb2col(rgb_norn(rgb)))
}

#' @rdname colorblindness
#' @export
clr_tritan <- function(col, severity = 1) {
  rgb <- (diag(3)*(1-severity) + tritan_matrix * (severity)) %*% col2rgb(col)
  color(rgb2col(rgb_norn(rgb)))
}

protan_matrix <- matrix(nrow = 3, byrow = TRUE,
                        c(0.152286,	 1.052583, -0.204868,
                          0.114503,	 0.786281,	0.099216,
                         -0.003882,	-0.048116,	1.051998))

deutan_matrix <- matrix(nrow = 3, byrow = TRUE,
                        c(0.367322,	0.860646,	-0.227968,
                          0.280085,	0.672501,	 0.047413,
                         -0.011820,	0.042940,	 0.968881))

tritan_matrix <- matrix(nrow = 3, byrow = TRUE,
                        c(1.255528,	-0.076749, -0.178779,
                         -0.078411,	 0.930809,	0.147602,
                          0.004733,	 0.691367,	0.303900))
