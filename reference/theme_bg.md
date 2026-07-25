# Background colors used by the `gtUtils` themes

A lookup of the background color each `gt_theme_*` function sets, used
to match a saved image's canvas to the table sitting on it.
[`gt_save_crop()`](https://andreweatherman.github.io/gtUtils/reference/gt_save_crop.md)
and
[`gt_social_crop()`](https://andreweatherman.github.io/gtUtils/reference/gt_social_crop.md)
pad the image using their own `bg` argument, and a mismatch shows up as
a border around the table.

## Usage

``` r
theme_bg
```

## Format

A tibble with three columns:

- theme:

  The theme function name.

- has_style:

  Whether the theme sets additional styling beyond the background.

- bg:

  The background color the theme applies, as a hex code or a color name.

## See also

[`gt_save_crop()`](https://andreweatherman.github.io/gtUtils/reference/gt_save_crop.md),
[`gt_social_crop()`](https://andreweatherman.github.io/gtUtils/reference/gt_social_crop.md).

## Examples

``` r
if (FALSE) { # \dontrun{
# look up the background a theme uses, then match the canvas to it
bg <- theme_bg$bg[theme_bg$theme == "gt_theme_gtutils"]

gt::gt(head(mtcars)) %>%
  gt_theme_gtutils() %>%
  gt_save_crop("table.png", bg = bg)
} # }
```
