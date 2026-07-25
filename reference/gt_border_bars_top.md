# Add horizontal bars to the top of a `gt` table

Adds a row of horizontal color bars above a `gt` table from a vector of
hex colors, optionally carrying an image and a line of text. The bars
are attached as the table caption, so they sit above everything else.

## Usage

``` r
gt_border_bars_top(
  gt_object,
  colors,
  bar_height = 10,
  bar_width = "100%",
  bar_align = "center",
  img = NULL,
  img_width = 30,
  img_height = 30,
  img_padding = 10,
  img_align = "right",
  text = NULL,
  text_weight = "bold",
  text_color = "#FFFFFF",
  text_size = 18,
  text_align = "left",
  text_padding = 10
)
```

## Arguments

- gt_object:

  A `gt` table object to modify.

- colors:

  Character. Hex color codes, one per bar. When `img` or `text` is
  supplied only the first color is used.

- bar_height:

  Numeric. The height of the bars in pixels. Defaults to `10`.

- bar_width:

  Character. The width of the bar block, as a CSS width. Defaults to
  `"100%"`.

- bar_align:

  Character. Alignment of the block when `bar_width` is under `"100%"`.
  One of `"left"`, `"center"`, `"right"`. Defaults to `"center"`.

- img:

  Optional. A URL for an image to render in the bar. Defaults to `NULL`.

- img_width:

  Numeric. The image width in pixels. Defaults to `30`.

- img_height:

  Numeric. The image height in pixels. Defaults to `30`.

- img_padding:

  Numeric. Padding around the image in pixels, so it does not touch the
  edge. Defaults to `10`.

- img_align:

  Character. The side the image padding is applied to, one of `"left"`,
  `"center"`, `"right"`. Defaults to `"right"`.

- text:

  Optional. Text to display in the bar. Defaults to `NULL`.

- text_weight:

  Character. The font weight of the text. Defaults to `"bold"`.

- text_color:

  Character. The text color. Defaults to `"#FFFFFF"`.

- text_size:

  Numeric. The font size in pixels. Defaults to `18`.

- text_align:

  Character. The side the text padding is applied to, one of `"left"`,
  `"center"`, `"right"`. Defaults to `"left"`.

- text_padding:

  Numeric. Padding around the text in pixels. Defaults to `10`.

## Value

Returns a modified `gt` table with a row of bars above it.

## Details

The bars are added with
[`gt::tab_caption()`](https://gt.rstudio.com/reference/tab_caption.html),
and a table id is resolved or generated so scoped CSS can zero the
caption padding. With neither `img` nor `text`, each entry in `colors`
becomes its own full-width bar stacked in a block. When `img` or `text`
is supplied, a single bar is drawn in the first color as a flex row with
the text at one end and the image at the other. The text font is read
from the table's title styling and imported as a Google Font, falling
back to the inherited font when none is set, so it renders in an
exported table.

## Examples

``` r
if (FALSE) { # \dontrun{
library(gt)

gt(head(mtcars)) %>%
  gt_border_bars_top(c("#1B7837", "#FFFFFF", "#B2182B"))

# a single bar carrying a title
gt(head(iris)) %>%
  gt_border_bars_top("#22223B", text = "Iris measurements", bar_height = 34)
} # }
```
