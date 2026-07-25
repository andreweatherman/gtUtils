# Add a border grid to a `gt` table

Draws borders between every column and every row of a `gt` table, giving
it a full grid, with an option to extend the borders around the column
and row labels.

## Usage

``` r
gt_border_grid(gt_object, color = "black", weight = 1, include_labels = FALSE)
```

## Arguments

- gt_object:

  A `gt` table object to modify.

- color:

  Character. The border color. Defaults to `"black"`.

- weight:

  Numeric. The border thickness in pixels. Defaults to `1`.

- include_labels:

  Logical. Should the borders extend around the row and column labels?
  Defaults to `FALSE`.

## Value

Returns a modified `gt` table with the grid borders applied.

## Details

Column borders are drawn with
[`gtExtras::gt_add_divider()`](https://jthomasmock.github.io/gtExtras/reference/gt_add_divider.html)
on every column but the last, and the row borders are added as scoped
CSS on the `.gt_row` top border. A table id is resolved or generated
first, since that CSS is keyed on `#<table_id>`.

## Examples

``` r
if (FALSE) { # \dontrun{
library(gt)

gt(head(mtcars)) %>% gt_border_grid()

# heavier gray lines, including around the labels
gt(head(iris)) %>%
  gt_border_grid(color = "#BBBBBB", weight = 2, include_labels = TRUE)
} # }
```
