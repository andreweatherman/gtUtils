# Add colored indicator boxes to a `gt` table

Replaces column values with colored boxes, filled when a value meets a
rule and left neutral otherwise. By default the box is colored when a
value equals the second element of `indicator_vals` (`1`) and left
`color_no` when it equals the first (`0`); supply `indicator_rule` for
any other test. Name the columns to convert with `columns`, or the ones
to leave alone with `key_columns`.

## Usage

``` r
gt_indicator_boxes(
  gt_object,
  columns = NULL,
  key_columns = NULL,
  indicator_vals = c(0, 1),
  indicator_rule = function(x) x == indicator_vals[2],
  color_yes = "#FCCF10",
  color_no = "#EEEEEE",
  show_na_as_na = FALSE,
  show_text = FALSE,
  show_only = NULL,
  per_column_formats = NULL,
  color_na = NULL,
  border_color = NULL,
  border_width = 0.25,
  box_width = 20,
  box_height = 20,
  text_size = 12,
  text_weight = "bold"
)
```

## Arguments

- gt_object:

  A `gt` table object to modify.

- columns:

  The columns to convert to boxes, using tidyselect. Defaults to `NULL`,
  which converts every column not named in `key_columns`.

- key_columns:

  The columns to leave untouched, using tidyselect. Every other column
  is converted. Kept for the inverted way of saying the same thing; give
  this or `columns`, not both. Defaults to `NULL`.

- indicator_vals:

  Numeric. A length-2 vector giving the "no" and "yes" values. Defaults
  to `c(0, 1)`.

- indicator_rule:

  A function deciding when a box is colored. It receives the column
  values, and optionally the column name as a second argument, and
  returns a logical vector. Defaults to testing equality with
  `indicator_vals[2]`.

- color_yes:

  Character. The fill for boxes meeting the rule. Defaults to
  `"#FCCF10"`.

- color_no:

  Character. The fill for boxes not meeting the rule. Defaults to
  `"#EEEEEE"`.

- show_na_as_na:

  Logical. Should `NA` be shown as `NA` rather than treated as "no"?
  Defaults to `FALSE`.

- show_text:

  Logical. Should the formatted value be printed inside the box?
  Defaults to `FALSE`.

- show_only:

  Character. Restrict printed text to one class of box, one of `"yes"`,
  `"no"`, or `"NA"`. Defaults to `NULL`, which prints text for all.

- per_column_formats:

  A named list keyed by column name, each element a list of formatting
  options (`digits`, `format_type`, `suffix`) for that column. Defaults
  to `NULL`.

- color_na:

  Character. The fill for `NA` boxes. Defaults to `NULL`, which uses
  `color_no`.

- border_color:

  Character. The border color around the boxes. Defaults to `NULL`, no
  border.

- border_width:

  Numeric. The border width in pixels. Defaults to `0.25`.

- box_width:

  Numeric. The box width in pixels, used when `show_text` is `FALSE`.
  Defaults to `20`.

- box_height:

  Numeric. The box height in pixels. Defaults to `20`.

- text_size:

  Numeric. The font size of the box text in pixels, used when
  `show_text` is `TRUE`. Defaults to `12`.

- text_weight:

  Character. The font weight of the box text. Defaults to `"bold"`.

## Value

Returns a modified `gt` table with the converted columns shown as
colored boxes.

## Details

Every column outside `key_columns` is replaced with an HTML span through
[`gt::text_transform()`](https://gt.rstudio.com/reference/text_transform.html),
and the transformed columns are then center-aligned. The rule is applied
to the numeric coercion of each column, so text values become `NA`; `NA`
cells take `color_na` (falling back to `color_no`) unless
`show_na_as_na` keeps them labeled `NA`. Box text is set to black or
white, whichever measures higher contrast against the fill.

When `show_text` is `TRUE` the box widens to fit the widest formatted
value in the column; otherwise it is fixed at `box_width`.
`indicator_rule` may accept a second argument, the column name, which
allows a different test per column.

## Examples

``` r
if (FALSE) { # \dontrun{
library(gt)

roster <- data.frame(
  player = c("A", "B", "C"),
  starter = c(1, 0, 1),
  injured = c(0, 0, 1),
  captain = c(1, 0, 0)
)

gt(roster) %>% gt_indicator_boxes(key_columns = "player")

# print the underlying values and draw a border
gt(roster) %>%
  gt_indicator_boxes(key_columns = "player", show_text = TRUE,
                     border_color = "#333333")
} # }
```
