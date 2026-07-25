# Color ranked columns of a `gt` table

A shorthand around
[`gt::data_color()`](https://gt.rstudio.com/reference/data_color.html)
for filling columns that already hold rank values, where 1 is best. It
takes either a plain vector of hex colors or a `paletteer` palette, and
either an explicit domain or one derived from the data. Values are
colored as they are; no ranking is computed for you.

## Usage

``` r
gt_color_ranks(
  gt_object,
  columns,
  rows = NULL,
  palette = c("#3D8B6E", "#9DC5A7", "#EDE0CC", "#DB9070", "#BE4D3A"),
  domain = NULL,
  reverse = FALSE,
  na_color = "white",
  autocolor_text = TRUE,
  pal_type = "discrete",
  ...
)
```

## Arguments

- gt_object:

  A `gt` table object to modify.

- columns:

  The column or columns to color.

- rows:

  The rows to color. Either an expression evaluated against the table's
  data, such as `mpg > 20`, or a numeric vector of row indices. Rows
  left out keep their default background. If `NULL`, every row is
  colored. Defaults to `NULL`.

- palette:

  A color palette to use. If you want a palette from `paletteer`,
  specify it as `package::palette`. Defaults to a five-color
  green-to-red ramp.

- domain:

  A length-2 numeric vector giving the value range mapped onto the
  palette. If `NULL`, the range is taken from the selected columns and
  shared across all of them, so rank 1 and the largest rank present
  anchor the two ends of the palette. Defaults to `NULL`.

- reverse:

  Logical. Should the palette be reversed? Defaults to `FALSE`.

- na_color:

  Character. The color used for `NA` values. Defaults to `"white"`.

- autocolor_text:

  Logical. Should the text color be set automatically for contrast
  against the fill? Defaults to `TRUE`.

- pal_type:

  Character. Either `"discrete"` or `"continuous"`, used when applying
  `paletteer` palettes. Defaults to `"discrete"`.

- ...:

  Additional arguments passed to
  [`gt::data_color`](https://gt.rstudio.com/reference/data_color.html).

## Value

Returns a modified `gt` table with the selected columns filled by value.

## Details

The whole cell background is filled.
[`gt_color_pills()`](https://andreweatherman.github.io/gtUtils/reference/gt_color_pills.md)
does the same job with rounded pill spans instead.

## See also

[`gt_legend_continuous()`](https://andreweatherman.github.io/gtUtils/reference/gt_legend_continuous.md)
for explaining the scale, and
[`gt_color_pills()`](https://andreweatherman.github.io/gtUtils/reference/gt_color_pills.md)
for a pill treatment instead of a full-cell fill.

## Examples

``` r
if (FALSE) { # \dontrun{
library(gt)

ranked <- data.frame(
  model = rownames(head(mtcars, 6)),
  mpg_rank = rank(-head(mtcars, 6)$mpg),
  hp_rank = rank(head(mtcars, 6)$hp)
)

gt(ranked) %>% gt_color_ranks(c(mpg_rank, hp_rank))

# a paletteer palette, reversed
gt(ranked) %>%
  gt_color_ranks(mpg_rank, palette = "viridis::mako", pal_type = "continuous",
                 reverse = TRUE)
} # }
```
