# Fill individual cells in a `gt` table by a condition

Fills each cell in a block of columns that meets a condition and leaves
the rest alone. It is the per-cell counterpart to
[`gtExtras::gt_highlight_cols()`](https://jthomasmock.github.io/gtExtras/reference/gt_highlight_cols.html)
and
[`gtExtras::gt_highlight_rows()`](https://jthomasmock.github.io/gtExtras/reference/gt_highlight_rows.html),
which fill a whole column or a whole row. The condition is tested
against every cell on its own, so a scattered set of cells can be filled
in one call.

## Usage

``` r
gt_highlight_cells(
  gt_object,
  columns,
  condition,
  fill = "#FFF3B0",
  text_color = NULL,
  bold = FALSE,
  ...
)
```

## Arguments

- gt_object:

  A `gt` table object to modify.

- columns:

  The block of columns to test, the body of the grid.

- condition:

  How a cell is chosen. Either a function or a right-hand-side formula
  applied to each column and returning a logical vector (`~ .x > 0.7`,
  `~ grepl("home", .x)`, `~ is.na(.x)`), or a logical matrix or data
  frame the same shape as `columns` for a mask computed ahead of time.

- fill:

  Character. The cell fill color. Defaults to `"#FFF3B0"`, a soft
  yellow.

- text_color:

  Optional. A hex color for the text in filled cells. Defaults to
  `NULL`, which leaves the text color alone.

- bold:

  Logical. Should filled cells be bolded? Defaults to `FALSE`.

- ...:

  Additional arguments passed to
  [`gt::cell_text()`](https://gt.rstudio.com/reference/cell_text.html)
  for the filled cells, such as `style = "italic"`.

## Value

Returns a modified `gt` table with the matching cells filled.

## Details

A single
[`gt::tab_style()`](https://gt.rstudio.com/reference/tab_style.html)
with
[`gt::cells_body()`](https://gt.rstudio.com/reference/cells_body.html)
cannot do this, since it reads `rows` and `columns` as a rectangle,
every selected row crossed with every selected column. Filling a
diagonal, a checker pattern, or any scatter otherwise takes a loop over
the columns, one pass each. This wraps that loop.

Each selected column is filled independently, so the pattern can be a
diagonal, a checker, or any scatter of cells.

The condition reads the underlying data rather than the rendered cell,
so a test like `~ grepl("home", .x)` sees whatever value the column
holds, HTML and all. A formula or function must return one logical per
row, and `NA` counts as not matched. For more than one color, layer the
calls, one per color.

## See also

[`gtExtras::gt_highlight_cols()`](https://jthomasmock.github.io/gtExtras/reference/gt_highlight_cols.html)
and
[`gtExtras::gt_highlight_rows()`](https://jthomasmock.github.io/gtExtras/reference/gt_highlight_rows.html)
for whole-column and whole-row fills.

## Examples

``` r
if (FALSE) { # \dontrun{
library(gt)

# a correlation matrix: flag strong pairs, but not the diagonal of ones
m <- round(cor(mtcars[, c("mpg", "disp", "hp", "wt", "qsec")]), 2)
cordf <- data.frame(var = rownames(m), m, row.names = NULL, check.names = FALSE)
cordf %>%
  gt(rowname_col = "var") %>%
  gt_highlight_cells(-var, ~ .x > 0.7 & .x < 1, fill = "#FFD1A9") %>%
  gt_highlight_cells(-var, ~ .x < -0.7, fill = "#A9D0FF")

# a schedule matrix: home games one color, byes another
sched <- data.frame(
  team = c("Alpha", "Bravo", "Charlie"),
  wk1 = c("vs X", "@ Y", "BYE"),
  wk2 = c("@ W", "vs V", "@ U")
)
gt(sched) %>%
  gt_highlight_cells(c(wk1, wk2), ~ grepl("^vs", .x), fill = "#CCE7F5") %>%
  gt_highlight_cells(c(wk1, wk2), ~ .x == "BYE", fill = "#D9D9D9")
} # }
```
