# Draw percentile bars in a `gt` table

Draws a percentile as a filled track with a circular marker at the tip
of the fill, the value printed inside it. The bar, the marker, and the
number are all CSS, so the whole thing stays vector at any export scale.

## Usage

``` r
gt_percentile_bar(
  gt_object,
  columns,
  rows = NULL,
  domain = c(0, 100),
  scale = "auto",
  palette = c("#3661AD", "#C9C9C9", "#D22D49"),
  reverse = FALSE,
  pal_type = "discrete",
  track_color = "#E9E9E9",
  track_height = 6,
  marker_size = 22,
  text_color = "#FFFFFF",
  font_size = NULL,
  ring_color = NULL,
  ring_width = 2,
  full_track = TRUE,
  na_label = "—",
  na_track_color = NULL,
  na_text_color = "#9A9A9A",
  decimals = 0,
  width = 220
)
```

## Arguments

- gt_object:

  A `gt` table object to modify.

- columns:

  The columns holding percentiles.

- rows:

  The rows to draw bars in. Either an expression evaluated against the
  table's data, such as `pct > 50`, or a numeric vector of row indices.
  Rows left out keep their raw value. If `NULL`, every row is drawn.
  Defaults to `NULL`.

- domain:

  A length-2 numeric vector giving the percentile range. Defaults to
  `c(0, 100)`.

- scale:

  How to handle a column stored as 0-1 proportions rather than 0-100
  percentiles. `"auto"` maps a column whose values all fall in `[0, 1]`
  onto `domain`, so `0.72` is treated and printed as `72` against the
  default range. `"none"` leaves the values untouched. A number
  multiplies every value by it, for any other conversion. Defaults to
  `"auto"`.

- palette:

  Colors mapped across `domain` and used for both the fill and the
  marker. Defaults to a blue-grey-red ramp.

- reverse:

  Logical. Should the palette be reversed? Defaults to `FALSE`.

- pal_type:

  Character. Which `paletteer` registry to look a `package::palette`
  string up in, `"discrete"` or `"continuous"`. The other registry is
  tried as a fallback, so this rarely needs setting. Defaults to
  `"discrete"`.

- track_color:

  Character. The color of the unfilled track. Defaults to `"#E9E9E9"`.

- track_height:

  Numeric. The track thickness in pixels. Defaults to `6`.

- marker_size:

  Numeric. The marker diameter in pixels. Defaults to `22`.

- text_color:

  Character. The color of the number inside the marker. Defaults to
  `"#FFFFFF"`.

- font_size:

  Numeric. The size of the number in pixels. If `NULL`, it is set from
  `marker_size`. Defaults to `NULL`.

- ring_color:

  Character. A ring drawn around the marker, which separates it from the
  fill. `NULL` for none. Defaults to `NULL`.

- ring_width:

  Numeric. The ring thickness in pixels. Defaults to `2`.

- full_track:

  Logical. Should the track run the full width, or stop at the marker?
  Defaults to `TRUE`.

- na_label:

  Character. What to show for a missing percentile, such as
  `"Not qualified"` or `"NR"`. The track is drawn in two segments with
  the label centered between them, so the row keeps its rhythm and
  cannot be read as a percentile of zero. `NULL` draws an unbroken empty
  track with no text. Defaults to an em dash.

- na_track_color:

  Character. The track color for missing rows. If `NULL`, `track_color`
  is used. Defaults to `NULL`.

- na_text_color:

  Character. The color of `na_label`. Defaults to `"#9A9A9A"`.

- decimals:

  Integer. Decimal places on the number. Defaults to `0`.

- width:

  Numeric. The column width in pixels. Defaults to `220`.

## Value

Returns a modified `gt` table with percentile bars.

## Details

[`gtExtras::gt_plt_percentile()`](https://jthomasmock.github.io/gtExtras/reference/gt_plt_percentile.html)
covers percentiles as a ggplot dot plot rendered to a raster image. This
is the filled-bar-and-marker treatment instead.

The track is inset by half the marker so that a value at either extreme
keeps the marker inside the cell rather than hanging over the edge. The
fill and the marker take the same color, mapped from the value.

## Examples

``` r
if (FALSE) { # \dontrun{
library(gt)

metrics <- data.frame(
  metric = c("Barrel %", "Exit velocity", "Chase rate", "Whiff %"),
  pct = c(94, 41, 72, 18)
)

gt(metrics) %>% gt_percentile_bar(pct)

# the same values stored 0-1 render identically under the default scale = "auto"
metrics$pct <- metrics$pct / 100
gt(metrics) %>% gt_percentile_bar(pct)

# a row that did not qualify
metrics$pct[2] <- NA
gt(metrics) %>% gt_percentile_bar(pct, na_label = "Not qualified")
} # }
```
