#' Extract RGB components
#'
#' Extract the red, green, or blue color components from a vector of colors.
#'
#' @inheritParams color
#'
#' @rdname extract_rgb
#'
#' @details
#' The values of the output will range between 0 and 255.
#'
#' Use [clr_extract()] if you are planning to extractionn multiple components.
#'
#' @family Extraction
#'
#' @return Numeric vector of values.
#' @export
#'
#' @examples
#' clr_extract_red(rainbow(100))
#' clr_extract_green(rainbow(100))
#' clr_extract_blue(rainbow(100))
#' clr_extract_alpha(rainbow(100))
clr_extract_red <- function(col) {
  col <- color(col)
  extract_rgb(col)[["red"]]
}

#' @rdname extract_rgb
#' @export
clr_extract_green <- function(col) {
  col <- color(col)
  extract_rgb(col)[["green"]]
}

#' @rdname extract_rgb
#' @export
clr_extract_blue <- function(col) {
  col <- color(col)
  extract_rgb(col)[["blue"]]
}

#' @rdname extract_rgb
#' @export
clr_extract_alpha <- function(col) {
  col <- color(col)
  extract_rgb(col)[["alpha"]]
}

extract_rgb <- function(col) {
  as.data.frame(t(col2rgb(col, alpha = TRUE)))
}

#' Extract HSL components
#'
#' Extract the hue, saturation, or lightness color components from a vector of
#' colors.
#'
#' @inheritParams color
#'
#' @rdname extract_hsl
#'
#' @details
#' The range of the value are
#'
#' - hue ranges from 0 to 360. in a circular fashion such that 0 and 360 are
#'   near identical. 0 is red
#' - saturation ranges from 0 to 100. 100 is full saturation, 0 is no saturation
#' - lightness ranges from 0 to 100. 100 is full lightness, 0 is no lightness
#'
#' Use [clr_extract()] if you are planning to extractionn multiple components.
#'
#' @family Extraction
#'
#' @return Numeric vector of values.
#' @export
#'
#' @examples
#' clr_extract_hue(rainbow(100))
#' clr_extract_saturation(rainbow(100))
#' clr_extract_lightness(rainbow(100))
clr_extract_hue <- function(col) {
  col <- color(col)
  extract_hsl(col)[["hue"]]
}

#' @rdname extract_hsl
#' @export
clr_extract_saturation <- function(col) {
  col <- color(col)
  extract_hsl(col)[["saturation"]]
}

#' @rdname extract_hsl
#' @export
clr_extract_lightness <- function(col) {
  col <- color(col)
  extract_hsl(col)[["lightness"]]
}

extract_hsl <- function(col) {
  hsl <- decode_colour(col, to = "hsl")
  new_names <- c("h" = "hue", "s" = "saturation", "l" = "lightness")
  hsl <- as.data.frame(hsl)
  names(hsl) <- new_names[names(hsl)]
  hsl
}

#' Extract Multiple Components
#'
#' Extract multiple color components at the same time.
#'
#' @inheritParams color
#' @param components character, components that should be extracted. See details
#'   for allowed components.
#'
#' @details
#' The allowed values for `components` are
#'
#' - red
#' - green
#' - blue
#' - hue
#' - saturation
#' - lightness
#'
#' This function is to be preferred if you need to extract multiple components
#' at the same time, since it doesn't need repeat transformations.
#'
#' @family Extraction
#'
#' @return data.frame of components
#' @export
#'
#' @examples
#' clr_extract(rainbow(10))
#'
#' clr_extract(rainbow(10), c("hue", "saturation"))
clr_extract <- function(col, components = c("red", "green", "blue", "hue",
                                            "saturation", "lightness")) {
  components <- match.arg(components, several.ok = TRUE)
  col <- color(col)
  cbind(
    extract_rgb(col),
    extract_hsl(col)
  )[components]
}
