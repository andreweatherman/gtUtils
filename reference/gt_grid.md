# Arrange several `gt` tables in a grid

Lays a list of tables out in rows and columns as small multiples, one
table per region or per quarter.
[`gtExtras::gt_two_column_layout()`](https://jthomasmock.github.io/gtExtras/reference/gt_two_column_layout.html)
handles exactly two tables and
[`gt_stack_tables()`](https://andreweatherman.github.io/gtUtils/reference/gt_stack_tables.md)
stacks any number of them vertically. This covers the rest.

## Usage

``` r
gt_grid(
  tables = NULL,
  ncol = 2,
  labels = NULL,
  label_style = list(),
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  source_note = NULL,
  caption_rule = FALSE,
  title_style = list(),
  subtitle_style = list(),
  caption_style = list(),
  source_note_style = list(),
  gap = 24,
  align = c("top", "center", "bottom"),
  file = NULL,
  bg = "white",
  whitespace = 50,
  zoom = 2
)
```

## Arguments

- tables:

  A list of `gt` table objects.

- ncol:

  Integer. The number of tables across. The number of rows follows from
  the length of `tables`. Defaults to `2`.

- labels:

  Character. An optional caption above each table, recycled against
  `tables`. Rendered in a neutral style rather than each table's own,
  and placed outside the table so a wide label does not stretch its
  panel. Defaults to `NULL`.

- label_style:

  A list of style options for the per-table captions. See `title_style`
  for the keys. Defaults to an empty list.

- title:

  Character. An optional heading above the whole grid. Set this instead
  of giving each table its own
  [`gt::tab_header()`](https://gt.rstudio.com/reference/tab_header.html).
  Defaults to `NULL`.

- subtitle:

  Character. An optional line below `title`. Defaults to `NULL`.

- caption:

  Character. An optional note below the grid. Defaults to `NULL`.

- source_note:

  Character. An optional second line below `caption`, right-aligned by
  default. Set both, with `caption_rule = TRUE`, for the split caption
  [`gt_538_caption()`](https://andreweatherman.github.io/gtUtils/reference/gt_538_caption.md)
  gives a single table. Defaults to `NULL`.

- caption_rule:

  Logical. Should a hairline rule sit between `caption` and
  `source_note`? Defaults to `FALSE`.

- title_style, subtitle_style, caption_style, source_note_style:

  Lists of style options. Recognized keys are `font` (a Google font
  name), `size`, `color`, `weight`, `italic`, `spacing` (letter
  spacing), `transform` (such as `"uppercase"`), and `align`, plus
  `line_height`, `margin_top`, `margin_bottom`, `padding_top`, and
  `padding_bottom` for nudging the blocks closer together or further
  apart. Any key you leave out keeps its default, so
  `title_style = list(size = "34px")` changes only the size. Same
  convention as
  [`gt_title_header()`](https://andreweatherman.github.io/gtUtils/reference/gt_title_header.md).
  Lengths take a number, read as pixels, or a CSS string such as
  `"2rem"`; a negative margin pulls an element up tight against the one
  above it.

- gap:

  Numeric. The space between tables in pixels. Defaults to `24`.

- align:

  Character. How tables of differing height line up within a row. Either
  `"top"`, `"center"`, or `"bottom"`. Defaults to `"top"`.

- file:

  Optional. A path to write a PNG to. If `NULL`, the grid is returned
  for the viewer instead. Defaults to `NULL`.

- bg:

  Character. The background color, used when saving. Defaults to
  `"white"`.

- whitespace:

  Numeric. Padding left around the grid when saving, in pixels. Defaults
  to `50`.

- zoom:

  Numeric. The rendering zoom factor used when saving. Defaults to `2`.

## Value

Displays the grid in the viewer, or writes it to `file`.

## Details

Unlike most of this package, this does not return a `gt` table. Separate
tables have their own columns, widths, and headers, so there is no
single table to hand back; the grid is assembled as HTML. That makes it
a last step, after each table is themed and formatted, and it is why
saving happens here through `file` rather than through
[`gt_save_crop()`](https://andreweatherman.github.io/gtUtils/reference/gt_save_crop.md).

Each table also keeps its own heading. Use `title` when the tables are
blocks of a single thing, as with a long ranking split in two, and
per-table
[`gt::tab_header()`](https://gt.rstudio.com/reference/tab_header.html)s
when they are genuinely separate exhibits.

Saving needs the `webshot2` package.

## See also

[`gt_stack_tables()`](https://andreweatherman.github.io/gtUtils/reference/gt_stack_tables.md)
for a vertical stack, and
[`gt_snake()`](https://andreweatherman.github.io/gtUtils/reference/gt_snake.md)
for folding a single long table into blocks.

## Examples

``` r
if (FALSE) { # \dontrun{
library(gt)

by_cyl <- lapply(split(mtcars, mtcars$cyl), function(d) {
  gt(head(d[c("mpg", "hp", "wt")], 5)) %>%
    gt_theme_broadsheet() %>%
    tab_header(title = paste(d$cyl[[1]], "cylinders"))
})

gt_grid(by_cyl, ncol = 2)

# one heading over the whole sheet
gt_grid(by_cyl, ncol = 3, title = "Fuel economy by cylinder count",
        subtitle = "1974 Motor Trend road tests", caption = "Data: mtcars")

# a 538-style split caption under the whole sheet
gt_grid(by_cyl, ncol = 3,
        title = "Fuel economy by cylinder count",
        caption = gt::md("Cars are grouped by **cylinder count**."),
        source_note = "Data: mtcars | Andrew Weatherman",
        caption_rule = TRUE)

# tighten the subtitle up under the title, and open the gap to the grid
gt_grid(by_cyl, ncol = 3, title = "Fuel economy", subtitle = "Motor Trend, 1974",
        title_style = list(margin_bottom = -2),
        subtitle_style = list(margin_bottom = 24))

# styled without writing any css
gt_grid(by_cyl, ncol = 3,
        title = "Fuel economy by cylinder count",
        subtitle = "1974 Motor Trend road tests",
        title_style = list(font = "Oswald", size = 34, transform = "uppercase"),
        subtitle_style = list(italic = TRUE, color = "#8A8A8A"))

# straight to an image
gt_grid(by_cyl, ncol = 3, file = "cylinders.png", bg = "#FBFAF7")
} # }
```
