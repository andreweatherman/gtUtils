# Add a 538-style caption to a `gt` table

Adds a caption block beneath the table with a rule under its top line,
in the style of FiveThirtyEight tables. The `top_caption` renders as a
footnote with a border drawn under it, and the `bottom_caption` renders
as a right-aligned source note below it. Both accept markdown.

## Usage

``` r
gt_538_caption(
  gt_object,
  top_caption = NULL,
  bottom_caption = NULL,
  rule_color = NULL,
  rule_width = 1,
  size = 12,
  align = "right",
  ...
)
```

## Arguments

- gt_object:

  A `gt` table object to modify.

- top_caption:

  Character. Text rendered as a footnote beneath the table, with a rule
  drawn under it. Accepts markdown. Defaults to `NULL`, which draws the
  rule with no text above it.

- bottom_caption:

  Character. Text rendered as a source note below the top caption,
  aligned by `align`. Accepts markdown. Defaults to `NULL`, which shows
  only the top caption.

- rule_color:

  Optional. A hex color for the rule under the top caption. When `NULL`,
  the color is taken from the rendered table so it tracks the theme.
  Defaults to `NULL`.

- rule_width:

  Numeric. The rule width in pixels. Defaults to `1`.

- size:

  Numeric. The font size of the top caption in pixels. Defaults to `12`.

- align:

  Character. The alignment of the bottom caption. Defaults to `"right"`.

- ...:

  Additional arguments. Currently unused.

## Value

Returns a modified `gt` table with the styled captions.

## Details

The top caption is attached with
[`gt::tab_footnote()`](https://gt.rstudio.com/reference/tab_footnote.html)
on the column labels, which places it in the table footer. Scoped CSS
then hides the footnote mark and draws a bottom border under the
footnote, so it reads as a ruled caption rather than a numbered note.
When `rule_color` is `NULL` the border color is taken from the first
text color found in the rendered table (via `gt:::as.tags.gt_tbl()`), so
it tracks the theme across light and dark color modes, and falls back to
a neutral gray when the render carries no color. The rules are keyed on
the table id. If the table has none, one is generated and assigned.

Apply the theme before this function. The rule color is read off the
rendered table, so calling it first on a dark theme borrows a near-black
(`#333333`) and the rule disappears into the background. Pass
`rule_color` to sidestep the question entirely.

## Examples

``` r
if (FALSE) { # \dontrun{
library(gt)

gt(head(mtcars[c("mpg", "hp", "wt")], 6)) %>%
  gt_538_caption(
    top_caption = "Fuel economy and power",
    bottom_caption = "Source: *1974 Motor Trend* road tests"
  )
} # }
```
