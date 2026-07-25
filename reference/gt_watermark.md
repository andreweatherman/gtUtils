# Put a watermark behind a `gt` table

Sets a wordmark or a logo behind the table body, faint enough to read
through.

## Usage

``` r
gt_watermark(
  gt_object,
  text = NULL,
  image = NULL,
  opacity = 0.06,
  size = "60%",
  position = "center",
  color = "#000000",
  angle = 0,
  font = "Helvetica, Arial, sans-serif"
)
```

## Arguments

- gt_object:

  A `gt` table object to modify.

- text:

  Optional. Text to render as the watermark. Defaults to `NULL`.

- image:

  Optional. A path to a PNG, JPEG, or SVG file to use instead of `text`.
  Defaults to `NULL`.

- opacity:

  Numeric. How faint the watermark is, from 0 to 1. Defaults to `0.06`.

- size:

  Character. The width of the watermark relative to the table, as a CSS
  background size such as `"60%"`. Defaults to `"60%"`.

- position:

  Character. Where it sits, as a CSS background position such as
  `"center"` or `"right bottom"`. Defaults to `"center"`.

- color:

  Character. The text color, used only with `text`. Defaults to
  `"#000000"`.

- angle:

  Numeric. Rotation in degrees, used only with `text`. Defaults to `0`.

- font:

  Character. The font family for `text`. Defaults to a system
  sans-serif.

## Value

Returns a modified `gt` table with a watermark behind the body.

## Details

A text watermark is drawn as an inline SVG rather than as HTML, because
a background image is the only way to sit behind the table body without
taking up a row. An SVG background cannot load the page's webfonts, so
`font` has to name a face installed on the machine rendering the table.

The watermark sits behind the body cells, so it will be hidden wherever
a cell has its own fill, from row striping or from
[`gt::data_color()`](https://gt.rstudio.com/reference/data_color.html).

## See also

[`gt_social_tag()`](https://andreweatherman.github.io/gtUtils/reference/gt_social_tag.md)
for visible attribution in the source note.

## Examples

``` r
if (FALSE) { # \dontrun{
library(gt)

gt(head(mtcars)) %>% gt_watermark(text = "DRAFT", angle = -30, opacity = 0.08)

gt(head(mtcars)) %>%
  gt_watermark(image = "logo.png", size = "40%", position = "right bottom")
} # }
```
