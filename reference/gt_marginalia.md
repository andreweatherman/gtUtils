# Style a column as margin notes

Styles a text column as commentary instead of data. It applies a muted
color, italics, a hairline separating the notes from the figures, a
blank column label, and a width constraint so the prose wraps instead of
stretching the table.

## Usage

``` r
gt_marginalia(
  gt_object,
  columns,
  width = 220,
  label = "",
  italic = TRUE,
  color = NULL,
  size = "0.92em",
  rule = TRUE,
  rule_color = NULL,
  align = "left"
)
```

## Arguments

- gt_object:

  A `gt` table object to modify.

- columns:

  The column or columns to treat as margin notes.

- width:

  Numeric. The column width in pixels, which is what makes the prose
  wrap. Pass `NULL` to leave the width alone. Defaults to `220`.

- label:

  Character. The column label. Defaults to `""`, since a note column
  rarely needs one; pass a string to keep a heading.

- italic:

  Logical. Should the notes be italicized? Defaults to `TRUE`.

- color:

  Optional. A hex color for the text. If `NULL`, a muted color is
  derived from the table background and checked for legibility. Defaults
  to `NULL`.

- size:

  Character. The font size, as a CSS size. Defaults to `"0.92em"`,
  slightly smaller than the body and scaling with whatever size the
  theme sets.

- rule:

  Logical. Should a hairline be drawn on the left edge, separating the
  notes from the data? Defaults to `TRUE`.

- rule_color:

  Optional. A hex color for that hairline. Derived from the table
  background when `NULL`. Defaults to `NULL`.

- align:

  Character. The text alignment. Defaults to `"left"`.

## Value

Returns a modified `gt` table with the selected columns styled as margin
notes.

## Details

A text column with no width set runs to a single long line, so `width`
is what wraps the prose. On a three-row example, that is the difference
between a table 625 pixels wide and one 482 pixels wide.

Colors are derived from the table background instead of being
hard-coded, so the treatment also reads correctly on a dark theme. That
means the theme has to be applied first. An unthemed table reports a
white background, and the notes come out too dark to read once a dark
theme lands on top. Pass `color` and `rule_color` to fix them regardless
of order.

## See also

[`gt_cutline()`](https://andreweatherman.github.io/gtUtils/reference/gt_cutline.md)
for a labeled break between rows.

## Examples

``` r
if (FALSE) { # \dontrun{
library(gt)

quarterly <- data.frame(
  Line = c("Revenue", "Cost of sales", "Operating expenses"),
  Actual = c(4820, 2110, 1360),
  Budget = c(4500, 2000, 1400),
  Comment = c("Enterprise renewals landed a quarter early.",
              "Freight costs above plan; contract renegotiated in Q3.",
              "Headcount hiring paused from February.")
)

gt(quarterly) %>%
  gt_theme_broadsheet() %>%
  gt_marginalia(Comment)

# keep a heading, widen it, and drop the italics
gt(quarterly) %>%
  gt_marginalia(Comment, label = "Commentary", width = 280, italic = FALSE)
} # }
```
