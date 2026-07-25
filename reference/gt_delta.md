# Add a computed change column to a `gt` table

Takes two numeric columns and inserts a new column holding the change
between them, signed and colored by direction. The change is `to` minus
`from`, so a later period as `to` and an earlier one as `from` gives a
positive number when the value has grown. The arithmetic, the
formatting, and the sign coloring happen in one call.

## Usage

``` r
gt_delta(
  gt_object,
  from,
  to,
  column_label = "Change",
  percent = FALSE,
  decimals = 1,
  arrows = FALSE,
  color = TRUE,
  color_positive = "#1B7837",
  color_negative = "#B2182B",
  color_neutral = NULL,
  force_sign = TRUE,
  after = NULL
)
```

## Arguments

- gt_object:

  A `gt` table object to modify.

- from:

  The starting column, a single numeric column.

- to:

  The ending column, a single numeric column. The change is `to - from`.

- column_label:

  Character. The label for the new column. Defaults to `"Change"`.

- percent:

  Logical. Should the change be shown as a percent of `from` rather than
  an absolute difference? Defaults to `FALSE`.

- decimals:

  Integer. The number of decimal places. Defaults to `1`.

- arrows:

  Logical. Should an up or down triangle lead the value in place of a
  sign? Defaults to `FALSE`.

- color:

  Logical. Should the values be colored by direction? Defaults to
  `TRUE`.

- color_positive:

  Character. The color for an increase. Defaults to a green,
  `"#1B7837"`.

- color_negative:

  Character. The color for a decrease. Defaults to a red, `"#B2182B"`.

- color_neutral:

  Optional. The color for no change. Defaults to `NULL`, which leaves
  zero the table's normal text color.

- force_sign:

  Logical. Should a plus be shown on an increase? Ignored when `arrows`
  is `TRUE`, since the arrow carries the direction. Defaults to `TRUE`.

- after:

  Optional. The column the new one is placed after, a position or a
  name. Defaults to `NULL`, which places it after `to`.

## Value

Returns a modified `gt` table with the change column added.

## Details

`gt::fmt_number(force_sign = TRUE)` formats a difference you have
already worked out, and
[`gtExtras::gt_fa_rank_change()`](https://jthomasmock.github.io/gtExtras/reference/gt_fa_rank_change.html)
handles movement in a rank column. This computes the delta from two
columns, formats it, and colors it by sign together.

A row is left blank where either `from` or `to` is missing, and where a
percent change divides by a `from` of zero. When `arrows` is `TRUE` the
value is shown as a magnitude behind the triangle, so a decrease reads
as a down triangle in front of a positive number.

## See also

[`gt_scale_note()`](https://andreweatherman.github.io/gtUtils/reference/gt_scale_note.md)
for disclosing a divided scale.

## Examples

``` r
if (FALSE) { # \dontrun{
library(gt)

revenue <- data.frame(
  Segment = c("Hardware", "Software", "Services"),
  Q1 = c(482, 331, 198),
  Q2 = c(515, 302, 246)
)

# absolute change, colored by sign
gt(revenue) %>% gt_delta(Q1, Q2)

# as a percent of Q1, with arrows
gt(revenue) %>% gt_delta(Q1, Q2, percent = TRUE, arrows = TRUE)
} # }
```
