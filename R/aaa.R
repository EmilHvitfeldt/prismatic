rgb2col <- function(x) {
  rgb(x[1, ], x[2, ], x[3, ], maxColorValue = 255)
}

rgb_norn <- function(x) {
  apply(apply(x, 2, pmin, 255), 2, pmax, 0)
}

hsv2hwb <- function(x) {
  out <- x
  out[, 2] <- (1 - x[, 2]) * x[, 3]
  out[, 3] <- (1 - x[, 3])
  out
}

hwb2hsv <- function(x) {
  out <- x
  out[, 2] <- 1 - (x[, 2] / (1 - x[, 3]))
  out[, 3] <- (1 - x[, 3])
  out
}

pro_transform <- function(data, value, ratio) {
  value * ratio + data * (1 - ratio)
}

col2hex <- function(x) {
    cols <- col2rgb(x)
    rgb(red = cols[1, ], green = cols[2, ], blue = cols[3, ], maxColorValue = 255)
}
