# Add stacked column headers with subtitles to a `gt` table

Replaces each column label with a two-line header: a main heading
stacked over a smaller subtitle. The heading and subtitle colors and
font weights are set through arguments, and the per-column heading and
subtitle text is supplied through `...`.

## Usage

``` r
gt_column_subheaders(
  gt_object,
  heading_color = "black",
  subtitle_color = "#808080",
  heading_weight = "bold",
  subtitle_weight = "normal",
  heading_size = 14,
  subtitle_size = 10,
  font = NULL,
  ...,
  gt_table = NULL
)
```

## Arguments

- gt_object:

  A `gt` table object to modify.

- heading_color:

  Character. Color for the main heading text. Defaults to `"black"`.

- subtitle_color:

  Character. Color for the subtitle text. Defaults to `"#808080"`.

- heading_weight:

  Character. Font weight for the main heading. Defaults to `"bold"`.

- subtitle_weight:

  Character. Font weight for the subtitle. Defaults to `"normal"`.

- heading_size:

  Numeric. Font size of the main heading in pixels. Defaults to `14`.

- subtitle_size:

  Numeric. Font size of the subtitle in pixels. Defaults to `10`.

- font:

  Optional. A font family applied to both lines. It is set as a CSS
  `font-family` and is not imported, so it must be available on the
  machine rendering the table or loaded by the theme. Defaults to
  `NULL`.

- ...:

  Named arguments where each name is a column in the `gt` table and each
  value is a list with two elements: `heading` (the main heading) and
  `subtitle` (the subtitle text). A column left out of `...` uses its
  column name as the heading and a non-breaking space (`&nbsp;`) as the
  subtitle.

- gt_table:

  Deprecated. Use `gt_object`.

## Value

Returns a modified `gt` table with stacked headers and subtitles.

## Details

Every column in the table is relabeled, not only the ones named in
`...`. The loop walks the full set of column names, looks each one up in
`...`, and builds an HTML label holding the heading at `heading_size`
over the subtitle at `subtitle_size`, joined by a line break. A column
with no entry in `...` falls back to its own name as the heading and a
non-breaking space as the subtitle, so the second line still takes
vertical space and the headers stay aligned. Labels are applied with
[`gt::cols_label()`](https://gt.rstudio.com/reference/cols_label.html),
so call this after any other label changes or they will be overwritten.

## Examples

``` r
if (FALSE) { # \dontrun{
library(gt)

mtcars %>%
  head() %>%
  gt() %>%
  gt_column_subheaders(
    mpg = list(heading = "Top", subtitle = "Bottom"),
    hp = list(heading = "Horsepower", subtitle = "HP"),
    heading_color = "blue", subtitle_color = "gray"
  )
} # }
```
