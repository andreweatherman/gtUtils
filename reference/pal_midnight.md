# A rank palette for dark backgrounds

A five-color green-to-red ramp with its luminance range lifted so it
still reads on a near-black background. The default palette in
[`gt_color_ranks()`](https://andreweatherman.github.io/gtUtils/reference/gt_color_ranks.md)
is built for white paper, and its mid-tones collapse into a dark ground.
Pass this instead when using
[`gt_theme_midnight()`](https://andreweatherman.github.io/gtUtils/reference/gt_theme_midnight.md)
or
[`gt_theme_terminal()`](https://andreweatherman.github.io/gtUtils/reference/gt_theme_terminal.md).

## Usage

``` r
pal_midnight
```

## Format

A character vector of five hex colors, running best to worst.

## See also

[`gt_theme_midnight()`](https://andreweatherman.github.io/gtUtils/reference/gt_theme_midnight.md).

## Examples

``` r
if (FALSE) { # \dontrun{
gt::gt(head(airquality, 10)) %>%
  gt_theme_midnight() %>%
  gt_color_ranks(Temp, palette = pal_midnight)
} # }
```
