
<!-- README.md is generated from README.Rmd. Please edit that file -->

# prismatic <img src='man/figures/logo.png' align="right" height="139" />

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/EmilHvitfeldt/prismatic.svg?branch=master)](https://travis-ci.org/EmilHvitfeldt/prismatic)
[![Codecov test
coverage](https://codecov.io/gh/EmilHvitfeldt/prismatic/branch/master/graph/badge.svg)](https://codecov.io/gh/EmilHvitfeldt/prismatic?branch=master)
<!-- badges: end -->

The goal of prismatic is to provide color manipulation tools in R, in a
intuitive, low-dependency and functional way.

  - **intuitive** All the working functions are prefixed with `clr_`
    (**c**o**l**o**r**) allowing for easy autocompletion.
  - **low-dependency** Only depends on
    [farver](https://github.com/thomasp85/farver).
  - **functional** All functions have consistant inputs and outputes and
    are thus fully pipeable.

## Installation

~~You can install the released version of prismatic from
[CRAN](https://CRAN.R-project.org) with:~~

``` r
install.packages("prismatic")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("EmilHvitfeldt/prismatic")
```

## Examples

All **prismatic** functions returns a `colors` object, which includes a
new printing method and plotting method for quickly visualizing the
colors.

``` r
library(prismatic)
library(magrittr)

terrain10 <- terrain.colors(10)

terrain10
#>  [1] "#00A600FF" "#2DB600FF" "#63C600FF" "#A0D600FF" "#E6E600FF"
#>  [6] "#E8C32EFF" "#EBB25EFF" "#EDB48EFF" "#F0C9C0FF" "#F2F2F2FF"

terrain_color <- color(terrain10)

terrain_color
#> <colors>
#> #00A600FF #2DB600FF #63C600FF #A0D600FF #E6E600FF #E8C32EFF #EBB25EFF #EDB48EFF #F0C9C0FF #F2F2F2FF

plot(terrain_color)
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

If [crayon](https://github.com/r-lib/crayon) is available the print
method will do its best to represent the colors.

![](man/figures/sceenshot.png)

``` r
library(prismatic)
library(magrittr)
ddd <- color(terrain.colors(10))

ddd
#> <colors>
#> #00A600FF #2DB600FF #63C600FF #A0D600FF #E6E600FF #E8C32EFF #EBB25EFF #EDB48EFF #F0C9C0FF #F2F2F2FF

plot(ddd)
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

``` r
clr_grayscale(ddd) %>% plot()
```

<img src="man/figures/README-unnamed-chunk-3-2.png" width="100%" />

``` r
clr_lighten(ddd, 0.7) %>% plot()
```

<img src="man/figures/README-unnamed-chunk-3-3.png" width="100%" />

``` r
clr_darken(ddd, 0.5) %>% plot()
```

<img src="man/figures/README-unnamed-chunk-3-4.png" width="100%" />

``` r
clr_saturate(ddd, 0.5) %>% plot()
```

<img src="man/figures/README-unnamed-chunk-3-5.png" width="100%" />

``` r
clr_desaturate(ddd, 0.5) %>% plot()
```

<img src="man/figures/README-unnamed-chunk-3-6.png" width="100%" />

``` r
clr_negate(ddd) %>% plot()
```

<img src="man/figures/README-unnamed-chunk-3-7.png" width="100%" />

``` r
clr_rotate(ddd, 180) %>% plot()
```

<img src="man/figures/README-unnamed-chunk-3-8.png" width="100%" />

``` r

clr_protan(ddd) %>% plot()
```

<img src="man/figures/README-unnamed-chunk-3-9.png" width="100%" />

``` r
clr_tritan(ddd) %>% plot()
```

<img src="man/figures/README-unnamed-chunk-3-10.png" width="100%" />

``` r
clr_deutan(ddd) %>% plot()
```

<img src="man/figures/README-unnamed-chunk-3-11.png" width="100%" />

## Related work

This package is hugely inspired by the JavaScript library
[Qix-/color](https://github.com/Qix-/color).

## Code of Conduct

Please note that the ‘prismatic’ project is released with a [Contributor
Code of
Conduct](https://github.com/EmilHvitfeldt/prismatic/blob/master/CODE_OF_CONDUCT.md).
By contributing to this project, you agree to abide by its terms.
