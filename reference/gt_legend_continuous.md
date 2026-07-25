# Add a color legend to a `gt` table

Draws a legend for a column colored with
[`gt_color_ranks()`](https://andreweatherman.github.io/gtUtils/reference/gt_color_ranks.md)
or
[`gt::data_color()`](https://gt.rstudio.com/reference/data_color.html).
Pass it the same `columns` and `palette` you colored with and the legend
will match the table, including the domain it takes from the data. For a
hand-built key of labeled swatches, use
[`gt_legend_discrete()`](https://andreweatherman.github.io/gtUtils/reference/gt_legend_discrete.md).

## Usage

``` r
gt_legend_continuous(
  gt_object,
  columns = NULL,
  palette = c("#3D8B6E", "#9DC5A7", "#EDE0CC", "#DB9070", "#BE4D3A"),
  domain = NULL,
  reverse = FALSE,
  pal_type = "discrete",
  type = c("continuous", "steps", "blocks"),
  n_bins = 5,
  labels = NULL,
  digits = 0,
  title = NULL,
  title_position = c("top", "bottom", "left", "right"),
  title_style = list(),
  labels_style = list(),
  labels_position = c("bottom", "top", "none"),
  location = c("bottom", "top"),
  align = c("center", "left", "right"),
  width = 200,
  height = 10,
  border_color = NULL,
  border_width = 1,
  radius = 2,
  gap = 3,
  title_gap = 4,
  block_gap = 2
)
```

## Arguments

- gt_object:

  A `gt` table object to modify.

- columns:

  The column or columns the legend describes, used to derive `domain`.
  Defaults to `NULL`, which takes the columns from an earlier coloring
  call, or requires `domain` to be given instead. See Picking up the
  scale.

- palette:

  A color palette to use. If you want a palette from `paletteer`,
  specify it as `package::palette`. Defaults to the same five-color
  green-to-red ramp as
  [`gt_color_ranks()`](https://andreweatherman.github.io/gtUtils/reference/gt_color_ranks.md).

- domain:

  A length-2 numeric vector giving the value range the palette spans. If
  `NULL`, the range is taken from `columns` the same way
  [`gt_color_ranks()`](https://andreweatherman.github.io/gtUtils/reference/gt_color_ranks.md)
  takes it, so the two agree. Defaults to `NULL`.

- reverse:

  Logical. Should the palette be reversed? Set this to match the
  `reverse` used when coloring. Defaults to `FALSE`.

- pal_type:

  Character. Either `"discrete"` or `"continuous"`, used when applying
  `paletteer` palettes. Defaults to `"discrete"`.

- type:

  Character. The appearance of the bar. One of `"continuous"` for a
  smooth ramp, `"steps"` for `n_bins` flat steps that touch, or
  `"blocks"` for `n_bins` separated swatches. Defaults to
  `"continuous"`.

- n_bins:

  Integer. The number of bins when `type` is `"steps"` or `"blocks"`.
  Defaults to `5`.

- labels:

  The tick labels. If `NULL`, the two ends of `domain` are used. The
  string `"edges"` uses the `n_bins + 1` bin boundaries. Otherwise pass
  any character vector, which is spread evenly. Defaults to `NULL`.

- digits:

  Integer. The number of decimal places used when deriving labels from
  `domain`. Defaults to `0`.

- title:

  Optional. A caption for the legend. Defaults to `NULL`.

- title_position:

  Character. Which side of the bar the title sits on. One of `"top"`,
  `"bottom"`, `"left"`, or `"right"`. Defaults to `"top"`.

- title_style:

  A named list styling the title. See Styling. Defaults to an empty
  list.

- labels_style:

  A named list styling the bound labels. See Styling. Defaults to an
  empty list.

- labels_position:

  Character. One of `"bottom"`, `"top"`, or `"none"` to omit the labels.
  Defaults to `"bottom"`.

- location:

  Character. Where to place the legend. `"bottom"` adds it as a source
  note; `"top"` puts it in the header, keeping any existing title and
  subtitle. Defaults to `"bottom"`.

- align:

  Character. The horizontal alignment of the whole legend. One of
  `"center"`, `"left"`, or `"right"`. Defaults to `"center"`.

- width:

  Numeric. The width of the bar in pixels. Defaults to `200`.

- height:

  Numeric. The height of the bar in pixels. Defaults to `10`.

- border_color:

  Optional. A hex color for a border around the bar, or around each
  block when `type` is `"blocks"`. Defaults to `NULL`.

- border_width:

  Numeric. The border width in pixels. Defaults to `1`.

- radius:

  Numeric. The corner radius of the bar in pixels. Defaults to `2`.

- gap:

  Numeric. The gap between the bar and its labels, in pixels. Defaults
  to `3`.

- title_gap:

  Numeric. The gap between the title and the bar, in pixels. Defaults to
  `4`.

- block_gap:

  Numeric. The gap between blocks when `type` is `"blocks"`, in pixels.
  Defaults to `2`.

## Value

Returns a modified `gt` table with the legend added.

## Details

The bar can be a smooth gradient, a stepped ramp, or separated blocks.
The title can sit on any side of it, and the title and bound labels are
styled independently through `title_style` and `labels_style`.

The bar is drawn as a series of solid color segments instead of a CSS
gradient. `gt`'s inline-CSS path, used by the RStudio Viewer, Quarto and
R Markdown, strips gradient backgrounds, and the legend would disappear.
Segment colors come from
[`scales::col_numeric`](https://scales.r-lib.org/reference/col_numeric.html),
the same function
[`gt::data_color()`](https://gt.rstudio.com/reference/data_color.html)
uses, so the ramp matches the cells exactly.

[`gt_title_header()`](https://andreweatherman.github.io/gtUtils/reference/gt_title_header.md)
calls
[`gt::tab_header()`](https://gt.rstudio.com/reference/tab_header.html),
which replaces the whole header. With `location = "top"`, call
[`gt_title_header()`](https://andreweatherman.github.io/gtUtils/reference/gt_title_header.md)
first or it will overwrite the legend.

## Styling

`title_style` and `labels_style` are named lists following the same
convention as
[`gt_grid()`](https://andreweatherman.github.io/gtUtils/reference/gt_grid.md)
and
[`gt_title_header()`](https://andreweatherman.github.io/gtUtils/reference/gt_title_header.md).
Recognized keys are `font` (a Google font name), `size`, `color`,
`weight`, `italic`, `spacing` (letter spacing), `transform` (such as
`"uppercase"`), and `align`, plus `line_height`, `margin_top`,
`margin_bottom`, `padding_top`, and `padding_bottom`. Any length takes a
number, read as pixels, or a CSS string.

## Picking up the scale

[`gt_color_ranks()`](https://andreweatherman.github.io/gtUtils/reference/gt_color_ranks.md),
[`gt_color_pills()`](https://andreweatherman.github.io/gtUtils/reference/gt_color_pills.md)
and
[`gt_percentile_bar()`](https://andreweatherman.github.io/gtUtils/reference/gt_percentile_bar.md)
record the `columns`, `palette`, `domain`, `reverse` and `pal_type` they
used on the table. This function reads any of those the caller did not
supply, so a legend can be added with no arguments and cannot disagree
with the cells it explains:

    gt(df) %>%
      gt_color_ranks(net, palette = "viridis::mako", domain = c(-10, 12)) %>%
      gt_legend_continuous()

Anything passed explicitly wins over the recorded value. With several
coloring calls, the most recent one is the one recorded. The record is
an attribute on the table and survives the rest of a pipeline, apart
from
[`gt_snake()`](https://andreweatherman.github.io/gtUtils/reference/gt_snake.md),
which rebuilds the table from its data; add the legend before snaking,
or pass the arguments directly.

## See also

[`gt_color_ranks()`](https://andreweatherman.github.io/gtUtils/reference/gt_color_ranks.md),
and
[`gt_legend_discrete()`](https://andreweatherman.github.io/gtUtils/reference/gt_legend_discrete.md)
for a discrete key.

## Examples

``` r
if (FALSE) { # \dontrun{
library(gt)

# the legend matches whatever gt_color_ranks() drew
gt(head(mtcars[c("mpg", "hp", "wt")], 8)) %>%
  gt_color_ranks(columns = mpg) %>%
  gt_legend_continuous(columns = mpg, title = "Miles per gallon")

# discrete blocks, labeled at every bin edge, title to the left
gt(head(airquality, 10)) %>%
  gt_color_ranks(columns = Temp) %>%
  gt_legend_continuous(
    columns = Temp, type = "blocks", n_bins = 5, labels = "edges",
    title = "Temp (F)", title_position = "left",
    title_style = list(weight = 600, transform = "uppercase",
                       spacing = "0.08em", size = "10px"),
    labels_style = list(size = "9px", color = "#999999")
  )
} # }
```
