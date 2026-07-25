# Put a colored bar on the leading edge of each row

Draws a short color bar down the left edge of every row, keyed to a
column in the data such as a team, category, or brand color. It serves
as a per-row key without taking up a column.

## Usage

``` r
gt_row_accent(
  gt_object,
  column,
  palette = NULL,
  rows = NULL,
  width = 4,
  side = c("left", "right"),
  hide = TRUE,
  na_color = "transparent"
)
```

## Arguments

- gt_object:

  A `gt` table object to modify.

- column:

  The column the color is keyed to. Either a column already holding
  colors, or a grouping column used together with `palette`.

- palette:

  Optional. Colors to map `column` onto. A named vector maps values
  explicitly, as `c(ACC = "#003366", SEC = "#B8232F")`. An unnamed
  vector is assigned across the sorted unique values and recycled. If
  `NULL`, `column` is taken to hold colors already. Defaults to `NULL`.

- rows:

  The rows to accent. Either an expression evaluated against the table's
  data, such as `conf == "ACC"` or `net <= 10`, or a numeric vector of
  row indices, or a single row number. If `NULL`, every row is accented.
  Defaults to `NULL`.

- width:

  Numeric. The bar width in pixels. Defaults to `4`.

- side:

  Character. Which edge the bar sits on, `"left"` or `"right"`. Defaults
  to `"left"`.

- hide:

  Logical. Should `column` be hidden once the bar is drawn? Usually what
  you want when it holds hex codes. Defaults to `TRUE`.

- na_color:

  Character. The color for rows where the key is missing. Defaults to
  `"transparent"`, which draws no bar.

## Value

Returns a modified `gt` table with a color bar on each row.

## Details

It draws the same bar
[`gt_spotlight()`](https://andreweatherman.github.io/gtUtils/reference/gt_spotlight.md)
uses for emphasis, applied per row here.
[`gtExtras::gt_highlight_rows()`](https://jthomasmock.github.io/gtExtras/reference/gt_highlight_rows.html)
fills the whole row instead.

The bar is drawn as a cell border on the leftmost rendered column, or on
the stub when the table has one, so it lines up with the row rather than
sitting inside a column of its own.

Rows sharing a color are styled together rather than one at a time, so a
long table with a handful of categories stays cheap.

## See also

[`gt_spotlight()`](https://andreweatherman.github.io/gtUtils/reference/gt_spotlight.md)
for emphasis rather than a key.

## Examples

``` r
if (FALSE) { # \dontrun{
library(gt)

teams <- data.frame(
  team = c("Duke", "Kansas", "Auburn", "Houston"),
  conf = c("ACC", "B12", "SEC", "B12"),
  net = c(10, 20, 5, 1)
)

# keyed to a grouping column
gt(teams) %>%
  gt_row_accent(conf, palette = c(ACC = "#003366", B12 = "#C8102E",
                                  SEC = "#B8232F"))

# or straight from a column of colors
teams$color <- c("#003366", "#C8102E", "#B8232F", "#C8102E")
gt(teams) %>% gt_row_accent(color)

# only some rows: by expression, by indices, or by one row number
gt(teams) %>% gt_row_accent(color, rows = net <= 10)
gt(teams) %>% gt_row_accent(color, rows = c(1, 3))
gt(teams) %>% gt_row_accent(color, rows = 2)
} # }
```
