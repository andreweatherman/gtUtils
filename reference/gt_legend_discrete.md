# Add a discrete color key to a `gt` table

Draws a legend of labeled color swatches, the categorical counterpart to
[`gt_legend_continuous()`](https://andreweatherman.github.io/gtUtils/reference/gt_legend_continuous.md),
which draws a continuous ramp. Give it the colors and labels you used
elsewhere in the table, such as the fills from
[`gt_highlight_cells()`](https://andreweatherman.github.io/gtUtils/reference/gt_highlight_cells.md)
or
[`gt_indicator_boxes()`](https://andreweatherman.github.io/gtUtils/reference/gt_indicator_boxes.md),
and it renders a color key. After
[`gt_tiers()`](https://andreweatherman.github.io/gtUtils/reference/gt_tiers.md),
which records its mapping on the table, `key_info` can be left out
entirely.

## Usage

``` r
gt_legend_discrete(
  gt_object,
  key_info = NULL,
  heading = NULL,
  subtitle = NULL,
  label_placement = c("outside", "inside"),
  location = c("top", "bottom"),
  shape = c("square", "rounded", "circle"),
  swatch_size = 14,
  border = TRUE,
  border_color = NULL,
  border_width = 1,
  gap = 14,
  direction = c("horizontal", "vertical"),
  align = c("center", "left", "right"),
  heading_style = list(),
  subtitle_style = list(),
  label_style = list()
)
```

## Arguments

- gt_object:

  A `gt` table object to modify.

- key_info:

  The key. Either a named character vector of `label = color`, or a data
  frame with a `color` column and a `label` column. Defaults to `NULL`,
  which takes the key
  [`gt_tiers()`](https://andreweatherman.github.io/gtUtils/reference/gt_tiers.md)
  recorded on the table.

- heading:

  Optional. A heading shown above the key. Defaults to `NULL`, no
  heading.

- subtitle:

  Optional. A subtitle shown under the heading. Defaults to `NULL`, no
  subtitle.

- label_placement:

  Character. Either `"outside"` to set each label beside its swatch, or
  `"inside"` to print it on the swatch. Defaults to `"outside"`.

- location:

  Character. Where the key goes. `"top"` places it in the header;
  `"bottom"` adds it as a source note. When `location` is `"top"` and no
  `heading` or `subtitle` is given, any existing title and subtitle are
  kept and the key is appended. Defaults to `"top"`.

- shape:

  Character. The swatch shape, one of `"square"`, `"rounded"`, or
  `"circle"`. Defaults to `"square"`.

- swatch_size:

  Numeric. The swatch size in pixels. Defaults to `14`.

- border:

  Logical. Should a hairline be drawn around each swatch? This keeps a
  pale swatch visible against the page. Defaults to `TRUE`.

- border_color:

  Optional. A hex color for the hairline, applied to every swatch. When
  `NULL`, each swatch takes its own edge, a soft darker shade of its
  fill, so pale swatches stay defined without a harsh uniform outline.
  Defaults to `NULL`.

- border_width:

  Numeric. The hairline width in pixels. Defaults to `1`.

- gap:

  Numeric. The space between keys in pixels. Defaults to `14`.

- direction:

  Character. Whether the keys run `"horizontal"` in a row or
  `"vertical"` in a column. Defaults to `"horizontal"`.

- align:

  Character. The alignment of the whole key, one of `"center"`,
  `"left"`, or `"right"`. Defaults to `"center"`.

- heading_style:

  A named list styling the heading. See Styling. Defaults to an empty
  list.

- subtitle_style:

  A named list styling the subtitle. See Styling. Defaults to an empty
  list.

- label_style:

  A named list styling the swatch labels. See Styling. For `"inside"`
  labels the color is chosen per swatch for contrast and any `color`
  here is ignored. Defaults to an empty list.

## Value

Returns a modified `gt` table with the key added.

## Details

The key can sit above the table or below it, run in a row or a column,
and take square, rounded, or circular swatches. An optional heading and
subtitle can ride along above the swatches, each styled through its own
list.

The key is passed as either a named vector,
`c("Home" = "#cce7f5", "Bye" = "#eeeeee")`, where the names are the
labels, or a data frame with a `color` column and a `label` column.

When `label_placement` is `"inside"`, each label takes black or white by
whichever contrasts better with its swatch, so a `color` in
`label_style` is ignored in that mode.

[`gt_title_header()`](https://andreweatherman.github.io/gtUtils/reference/gt_title_header.md)
and anything else calling
[`gt::tab_header()`](https://gt.rstudio.com/reference/tab_header.html)
replaces the whole header. With `location = "top"`, add those first or
they will overwrite the key.

## Styling

`heading_style`, `subtitle_style`, and `label_style` are named lists
following the same convention as
[`gt_grid()`](https://andreweatherman.github.io/gtUtils/reference/gt_grid.md)
and
[`gt_title_header()`](https://andreweatherman.github.io/gtUtils/reference/gt_title_header.md).
Recognized keys are `font` (a Google font name), `size`, `color`,
`weight`, `italic`, `spacing` (letter spacing), `transform` (such as
`"uppercase"`), and `align`, plus `line_height`, `margin_top`,
`margin_bottom`, `padding_top`, and `padding_bottom`. Any length takes a
number, read as pixels, or a CSS string. Any key left out keeps its
default. Colors left unset are derived from the table background, so the
key reads on light and dark themes. Apply the theme first, since an
unthemed table reports a white background and the heading and subtitle
come out near-black on whatever ground the theme sets later.

## See also

[`gt_legend_continuous()`](https://andreweatherman.github.io/gtUtils/reference/gt_legend_continuous.md)
for a continuous ramp.

## Examples

``` r
if (FALSE) { # \dontrun{
library(gt)

# a named vector is the quickest way in
gt(head(mtcars[c("mpg", "cyl", "hp")], 6)) %>%
  gt_legend_discrete(c("Efficient" = "#CCE7F5", "Thirsty" = "#F5CCCC"))

# rounded swatches below the table, labels on the swatches
gt(head(mtcars[c("mpg", "cyl", "hp")], 6)) %>%
  gt_legend_discrete(
    c("Yes" = "#FCCF10", "No" = "#EEEEEE"),
    heading = "Qualified",
    label_placement = "inside",
    location = "bottom",
    shape = "rounded"
  )

# style the heading and labels through lists, the same way gt_grid() does
gt(head(mtcars[c("mpg", "cyl", "hp")], 6)) %>%
  gt_legend_discrete(
    c("Home" = "#CCE7F5", "Away" = "#FFFFFF", "Bye" = "#EEEEEE"),
    heading = "2025 Schedule",
    heading_style = list(font = "Oswald", size = 20, transform = "uppercase"),
    label_style = list(size = 13, color = "#444444")
  )
} # }
```
