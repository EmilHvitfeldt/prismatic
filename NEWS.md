# prismatic 1.1.1

* Fixed documentation to be HTML5 friendly.

# prismatic 1.1.0

* Add extraction functions.
* `best_contrast()` has been added, and can be used to find the best contrasted colors.

# prismatic 1.0.0

* All functions now accepts zero length input.
* Added `contrast_ratio()` function to calculate contrast ratios between colors. (#18)
* Added `modify_hcl()` function to modify individual HCL axes. (#20)

## Breaking changes

* `clr_rotate()` now uses HCL instead of HSL as its color space for rotation. (#19)
* `clr_lightness()` and `clr_darken()` now uses HCL instead of HSL as its default color space. (#19)

# prismatic 0.2.0

* Added `clr_alpha()` function to specify transparency. (#9)
* Added `labels` argument to `plot.colors()` to show hexcode in plot. (#8)
* `clr_rotate()`'s argument `degrees` now default to 0.
* `clr_mix()`'s argument `ratio` now correctly takes varied lengths as the argument.
* added `check_color_blindness()` to allow quick visual color deficiency examination. (#11)

# prismatic 0.1.0

* Release on CRAN
