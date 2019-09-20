rgb2col <- function(x, alpha = FALSE) {
  if(alpha) {
    rgb(x[1, ], x[2, ], x[3, ], alpha = x[4, ], maxColorValue = 255)
  } else {
    rgb(x[1, ], x[2, ], x[3, ], maxColorValue = 255)
  }
}

rgb_norn <- function(x) {
  apply(apply(x, 2, pmin, 255), 2, pmax, 0)
}

pro_transform <- function(data, value, ratio) {
  value * ratio + data * (1 - ratio)
}
