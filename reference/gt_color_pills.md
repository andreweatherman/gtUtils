# Add color pills to `gt` table columns

Renders values as rounded color pills, filled from a palette by either
the raw value or the ordinal rank. Accepts a plain vector of hex colors
or a `paletteer` palette, and an explicit domain or one taken from the
data.
[`gt_color_ranks()`](https://andreweatherman.github.io/gtUtils/reference/gt_color_ranks.md)
fills the whole cell background instead of a pill.

## Usage

``` r
gt_color_pills(
  gt_object,
  columns,
  rows = NULL,
  palette = c("#C84630", "#5DA271"),
  fill_type = "continuous",
  rank_order = "desc",
  digits = NULL,
  domain = NULL,
  format_type = "number",
  scale_percent = TRUE,
  suffix = "",
  reverse = FALSE,
  outline_color = NULL,
  outline_width = 0.25,
  pal_type = "discrete",
  pill_height = 25,
  text_color = NULL,
  na_color = NULL,
  ...
)
```

## Arguments

- gt_object:

  A `gt` table object to modify.

- columns:

  The columns to fill with pills, using tidyselect.

- rows:

  The rows to fill. Either an expression evaluated against the table's
  data, such as `mpg > 20`, or a numeric vector of row indices. Rows
  left out keep their raw value. If `NULL`, every row is filled.
  Defaults to `NULL`.

- palette:

  A vector of hex colors, or a `paletteer` palette given as
  `package::palette`. Defaults to `c("#C84630", "#5DA271")`.

- fill_type:

  Character. Either `"rank"` or `"continuous"`, choosing whether color
  follows ordinal rank or the raw values. Defaults to `"continuous"`.

- rank_order:

  Character. Either `"asc"` or `"desc"`, used when `fill_type` is
  `"rank"`. Defaults to `"desc"`.

- digits:

  Integer. Decimal places to round the printed value to. Defaults to
  `NULL`.

- domain:

  Numeric. A length-2 vector giving the value range mapped onto the
  palette. If `NULL`, the observed range of the column is used and a
  warning is issued. Defaults to `NULL`.

- format_type:

  Character. How to format the printed value. One of `"number"`,
  `"comma"`, `"currency"`, or `"percent"`. Defaults to `"number"`.

- scale_percent:

  Logical. When `format_type` is `"percent"`, should values be
  multiplied by 100? Defaults to `TRUE`.

- suffix:

  Character. A string appended to each formatted value, such as `"M"`,
  `"K"`, or `"lbs"`. Defaults to `""`.

- reverse:

  Logical. Should the palette be reversed? Defaults to `FALSE`.

- outline_color:

  Optional. A hex color for a border around each pill. Defaults to
  `NULL`, no border.

- outline_width:

  Numeric. The border width in pixels. Defaults to `0.25`.

- pal_type:

  Character. Which `paletteer` registry to look a `package::palette`
  string up in, `"discrete"` or `"continuous"`. The other registry is
  tried as a fallback, so this rarely needs setting. Defaults to
  `"discrete"`.

- pill_height:

  Numeric. The height of each pill in pixels. Defaults to `25`.

- text_color:

  Optional. A hex color for the pill text. When `NULL`, the text color
  is chosen for contrast against each pill's fill. Defaults to `NULL`.

- na_color:

  Optional. A hex color for the pill drawn over a missing value. When
  `NULL`, a missing value is left blank with no pill. Defaults to
  `NULL`.

- ...:

  Additional arguments passed to
  [`scales::col_numeric`](https://scales.r-lib.org/reference/col_numeric.html).

## Value

Returns a modified `gt` table with color pills in the selected columns.

## Details

Pills are drawn as HTML spans through
[`gt::text_transform()`](https://gt.rstudio.com/reference/text_transform.html),
so they survive
[`gtsave()`](https://gt.rstudio.com/reference/gtsave.html). The fill is
mapped with
[`scales::col_numeric()`](https://scales.r-lib.org/reference/col_numeric.html)
over `domain`, and the text is set to black or white, whichever measures
higher contrast against that fill, unless `text_color` is set. A missing
value takes an `na_color` pill, or is left blank when `na_color` is
`NULL`.

Selecting several columns maps them all onto **one** `domain`, taken
from the selection as a whole when `domain` is unset, so their colors
stay comparable. Pill width is worked out **per column**, in `ch` units,
so each column's pills line up with each other rather than with the
widest value in the selection. With `fill_type = "rank"`, each column is
ranked against itself.

When `fill_type` is `"rank"`, ranks are computed with
[`rank()`](https://rdrr.io/r/base/rank.html) using averaged ties, then
flipped when `rank_order` is `"desc"` so the top value anchors the high
end of the palette. Leaving `domain` unset falls back to the observed
range and warns, since the color mapping then depends on the data
present.

## See also

[`gt_color_ranks()`](https://andreweatherman.github.io/gtUtils/reference/gt_color_ranks.md)
for filling the whole cell, and
[`gt_legend_continuous()`](https://andreweatherman.github.io/gtUtils/reference/gt_legend_continuous.md)
for a legend explaining the scale.

## Examples

``` r
if (FALSE) { # \dontrun{
library(gt)

gt(head(mtcars)) %>% gt_color_pills(mpg, domain = c(10, 35))

# several columns on one shared domain
gt(head(mtcars)) %>% gt_color_pills(c(disp, hp), domain = c(50, 500))

# tidyselect works too
gt(head(mtcars)) %>% gt_color_pills(where(is.numeric), domain = c(0, 500))

# only the rows that clear a threshold; the rest keep their raw value
gt(head(mtcars)) %>% gt_color_pills(mpg, rows = mpg > 20, domain = c(10, 35))

# color by rank rather than value, with a paletteer palette
gt(head(mtcars)) %>%
  gt_color_pills(hp, fill_type = "rank", palette = "viridis::mako",
                 digits = 0)
} # }
```
