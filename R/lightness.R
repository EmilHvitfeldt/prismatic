#' Make a color more light
#'
#' @details The colors will be transformed to HSL color space (hue, saturation,
#'  lightness) where the lightness of the color will  be modified. The lightness
#'  of a color takes a value between 0 and 1, with 0 being black and 1 being
#'  white. The \code{shift} argument takes a value between 0 and 1, where 0
#'  means that the lightness stays unchanged and 1 means completely white. As an
#'  example, if the lightness of the color is 0.6 and shift is 0.5, then the
#'  lightness be set to the halfway point between 0.6 and 1 which is 0.8.
#'
#' @details If space = "HSL" then the colors are transformed to HSL space where
#'   the lightness value L is adjusted. If space = "HCL" then the colors are
#'   transformed to Cylindrical HCL space where the luminance value L is
#'   adjusted. If space = "combined" then the colors are transformed into HSL and
#'   Cylindrical HCL space. Where the color adjusting is happening HLS is copied
#'   to the values in the HCL transformation. Thus the "combined" transformation
#'   adjusts the luminance in HCL space and chroma in HSL space. For more
#'   information regarding use of color spaces, please refer to the {colorspace}
#'   paper \url{https://arxiv.org/abs/1903.06490}.
#'
#' @source \url{https://en.wikipedia.org/wiki/HSL_and_HSV}
#' @source \url{https://en.wikipedia.org/wiki/CIELUV}
#' @source \url{https://arxiv.org/abs/1903.06490}
#'
#' @inheritParams color
#' @param shift Numeric between 0 and 1, 0 will do zero lightening, 1 will do
#'    complete lightening turning the color to white. Defaults to 0.5.
#' @param space character string specifying the color space in which adjustment
#'    happens. Can be either "HCL", "HSL" or "combined". Defaults to "HCL".
#'
#' @return a colors object of same length as col.
#' @export
#'
#' @seealso clr_darken
#' @examples
#' # Using linear shift
#' plot(clr_lighten(rep("red", 11), shift = seq(0, 1, 0.1)))
#' plot(clr_lighten(rep("red", 11), shift = seq(0, 1, 0.1), space = "HSL"))
#' plot(clr_lighten(rep("red", 11), shift = seq(0, 1, 0.1), space = "combined"))
#'
#' plot(clr_lighten(terrain.colors(10)))
#'
#' # Using exponential shifts
#' plot(clr_lighten(rep("red", 11), shift = log(seq(1, exp(1), length.out = 11))))
clr_lighten <- function(col, shift = 0.5, space = c("HCL", "HSL", "combined")) {
  col <- color(col)
  if (!(length(shift) == 1 || (length(shift) == length(col)))) {
    stop("`shift` must be of length 1 or the same length as `col`.")
  }

  if (all(shift == 0)) {
    return(col)
  }

  space <- match.arg(space)

  if (space == "HSL") {
    hsl <- decode_colour(col, to = "hsl")

    hsl[, "l"] <- pro_transform(hsl[, "l"], (shift >= 0) * 100, abs(shift))

    rgb <- convert_colour(hsl, "hsl", "rgb")
  } else if (space == "HCL") {
    hcl <- decode_colour(col, to = "hcl")

    hcl[, "l"] <- pmin(100, pmax(0, hcl[, "l"]))
    hcl[, "l"] <- (shift >= 0) * (100 - (100 - hcl[, "l"]) * (1 - shift)) +
      (shift < 0) * hcl[, "l"] * (1 + shift)
    hcl[, "l"] <- pmin(100, pmax(0, hcl[, "l"]))
    hcl[, "c"] <- pmin(
      max_chroma(hcl[, "h"], hcl[, "l"], floor = TRUE),
      pmax(0, hcl[, "c"])
    )

    rgb <- convert_colour(hcl, "hcl", "rgb")
  } else {
    hsl <- decode_colour(col, to = "hsl")
    hsl[, "l"] <- (shift >= 0) * (1 - (1 - hsl[, "l"]) *
      (1 - shift)) + (shift < 0) * hsl[, "l"] * (1 + shift)
    hsl[, "l"] <- pmin(100, pmax(0, hsl[, "l"]))

    hcl <- decode_colour(col, to = "hcl")
    hcl[, "l"] <- pmin(100, pmax(0, hcl[, "l"]))
    hcl[, "l"] <- (shift >= 0) * (100 - (100 - hcl[, "l"]) * (1 - shift)) +
      (shift < 0) * hcl[, "l"] * (1 + shift)
    hcl[, "l"] <- pmin(100, pmax(0, hcl[, "l"]))
    hcl[, "c"] <- convert_colour(hsl, "hsl", "hcl")[, "c"]
    hcl[, "c"] <- pmin(
      max_chroma(hcl[, "h"], hcl[, "l"], floor = TRUE),
      hcl[, "c"]
    )

    rgb <- convert_colour(hcl, "hcl", "rgb")
  }
  color(encode_colour(rgb_norm(rgb)))
}


#' Make a color more dark
#'
#' @details The colors will be transformed to HSL color space (hue, saturation,
#'  lightness) where the lightness of the color will  be modified. The lightness
#'  of a color takes a value between 0 and 1, with 0 being black and 1 being
#'  white. The \code{shift} argument takes a value between 0 and 1, where 0
#'  means that the lightness stays unchanged and 1 means completely black. As an
#'  example, if the lightness of the color is 0.6 and shift is 0.5, then the
#'  lightness be set to the halfway point between 0.6 and 0, which is 0.3.
#'
#' @details If space = "HSL" then the colors are transformed to HSL space where
#'   the lightness value L is adjusted. If space = "HCL" then the colors are
#'   transformed to Cylindrical HCL space where the luminance value L is
#'   adjusted. If space = "combined" then the colors are transformed into HSL and
#'   Cylindrical HCL space. Where the color adjusting is happening HLS is copied
#'   to the values in the HCL transformation. Thus the "combined" transformation
#'   adjusts the luminance in HCL space and chroma in HSL space. For more
#'   information regarding use of color spaces, please refer to the {colorspace}
#'   paper \url{https://arxiv.org/abs/1903.06490}.
#'
#' @source \url{https://en.wikipedia.org/wiki/HSL_and_HSV}
#' @source \url{https://en.wikipedia.org/wiki/CIELUV}
#' @source \url{https://arxiv.org/abs/1903.06490}
#'
#' @inheritParams color
#' @inheritParams clr_lighten
#' @param shift Numeric between 0 and 1, 0 will do zero darkening, 1 will do
#'    complete darkening turning the color to black. Defaults to 0.5.
#'
#' @return a color object of same length as col.
#' @export
#'
#' @seealso clr_lighten
#' @examples
#' # Using linear shift
#' plot(clr_darken(rep("red", 11), shift = seq(0, 1, 0.1)))
#' plot(clr_darken(rep("red", 11), shift = seq(0, 1, 0.1), space = "HSL"))
#' plot(clr_darken(rep("red", 11), shift = seq(0, 1, 0.1), space = "combined"))
#'
#' plot(clr_darken(terrain.colors(10)))
#'
#' # Using exponential shifts
#' plot(clr_darken(rep("red", 11), shift = log(seq(1, exp(1), length.out = 11))))
clr_darken <- function(col, shift = 0.5, space = c("HCL", "HSL", "combined")) {
  clr_lighten(col, -1 * shift, space)
}

max_chroma <- function(h, l, floor = FALSE) {
  n <- max(c(length(h), length(l)))
  h <- rep_len(h, n)
  l <- rep_len(l, n)
  while (any(h < 0)) h[h < 0] <- h[h < 0] + 360
  while (any(h >= 360)) h[h >= 360] <- h[h >= 360] - 360
  l <- pmin(100, pmax(0, l))
  hmin <- floor(h + 1e-08)
  hmax <- ceiling(h + 1e-08)
  lmin <- floor(l + 1e-08)
  lmax <- ceiling(l + 1e-08)
  c <- (hmax - h) * (lmax - l) *
    max_chroma_table[paste(hmin, lmin, sep = "-")] + (hmax - h) * (l - lmin) *
      max_chroma_table[paste(hmin, lmax, sep = "-")] + (h - hmin) * (lmax - l) *
      max_chroma_table[paste(hmax, lmin, sep = "-")] + (h - hmin) * (l - lmin) *
      max_chroma_table[paste(hmax, lmax, sep = "-")]
  c <- as.numeric(c)
  c[l <= 0 | l >= 100] <- 0
  if (floor) {
    c <- floor(c)
  }
  return(c)
}
