# Focus on rows of a `gt` table by dimming the rest

Emphasizes one or more rows and mutes everything else.
[`gtExtras::gt_highlight_rows()`](https://jthomasmock.github.io/gtExtras/reference/gt_highlight_rows.html)
makes a row louder; this leaves it as the only row at full strength.

## Usage

``` r
gt_spotlight(
  gt_object,
  rows,
  columns = gt::everything(),
  fill = NULL,
  text_color = NULL,
  bold = TRUE,
  accent_color = NULL,
  accent_width = 4,
  accent_column = NULL,
  dim_color = "#BBBBBB",
  if_none = c("warn", "dim", "ignore")
)
```

## Arguments

- gt_object:

  A `gt` table object to modify.

- rows:

  The rows to focus on. Either an expression evaluated against the
  table's data, such as `cyl == 8`, or a numeric vector of row indices.

- columns:

  The columns the spotlight covers. Cells outside this selection are
  dimmed, in the focused rows as well as the others, so only the
  selected cells are left at full strength. Defaults to all of them,
  which lights the whole row.

- fill:

  Optional. A hex color for the fill behind the focused rows. Defaults
  to `NULL`.

- text_color:

  Optional. A hex color for the text of the focused rows. Defaults to
  `NULL`.

- bold:

  Logical. Should the focused rows be bolded? Defaults to `TRUE`.

- accent_color:

  Optional. A color for a bar drawn on the left edge of the focused
  rows. Supplying one is what turns the bar on, the same way `fill` and
  `text_color` work. Defaults to `NULL`, no bar.

- accent_width:

  Numeric. The width of that bar in pixels, used only when
  `accent_color` is given. Defaults to `4`.

- accent_column:

  The column to draw the bar against, on its left edge. If `NULL`, the
  leftmost rendered column is used, which is what marks the start of the
  row. Name a column when the row you are lighting sits elsewhere, as
  with the second block of a
  [`gt_snake()`](https://andreweatherman.github.io/gtUtils/reference/gt_snake.md)
  table. More than one may be given, which draws a bar against each.
  Defaults to `NULL`.

- dim_color:

  Character. The text color applied to every other row. Pass `NULL` to
  leave the other rows alone, emphasizing without dimming. Defaults to
  `"#BBBBBB"`.

- if_none:

  Character. What to do when `rows` matches nothing. `"warn"` leaves the
  table alone and says so. `"dim"` dims the whole table, which is what
  you want when several tables are laid out together with
  [`gt_grid()`](https://andreweatherman.github.io/gtUtils/reference/gt_grid.md)
  and the highlighted row lives in only one of them. `"ignore"` leaves
  it alone silently. Defaults to `"warn"`.

## Value

Returns a modified `gt` table with the chosen rows emphasized.

## Details

Rows can be given as a filtering expression evaluated against the
underlying data, or as plain row numbers.

## See also

[`gt_cutline()`](https://andreweatherman.github.io/gtUtils/reference/gt_cutline.md)
for marking a threshold rather than a row.

## Examples

``` r
if (FALSE) { # \dontrun{
library(gt)

cars <- head(mtcars[c("mpg", "cyl", "hp")], 8)
cars$model <- rownames(cars)

# by expression
gt(cars) %>% gt_spotlight(rows = cyl == 8)

# by position, with an accent bar
gt(cars) %>%
  gt_spotlight(rows = 2, accent_color = "#0054AD", fill = "#EEF3FA")

# narrowing the spotlight to some columns dims the rest of the row too
gt(cars) %>% gt_spotlight(mpg:cyl, rows = 3, accent_color = "darkblue")

# put the bar somewhere other than the first column
gt(cars) %>%
  gt_spotlight(hp, rows = 3, accent_color = "darkblue", accent_column = hp)

# across several tables: blocks without the row dim rather than staying lit
chunks <- split(cars, ceiling(seq_len(nrow(cars)) / 4))
tbls <- lapply(chunks, function(x) {
  gt(x) %>% gt_spotlight(rows = cyl == 8, if_none = "dim")
})
gt_grid(tbls, ncol = 2)
} # }
```
