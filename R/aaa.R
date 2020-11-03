rgb2col <- function(x, alpha = FALSE) {
  if (alpha) {
    rgb(x[1, ], x[2, ], x[3, ], alpha = x[4, ], maxColorValue = 255)
  } else {
    rgb(x[1, ], x[2, ], x[3, ], maxColorValue = 255)
  }
}

rgb_norm <- function(x) {
  x[x > 255] <- 255
  x[x < 0] <- 0
  x
}

pro_transform <- function(data, value, ratio) {
  value * ratio + data * (1 - ratio)
}
