# Reshape a parallel frame to match a snaked table

Lays a data frame out in side-by-side blocks the same way
[`gt_snake()`](https://andreweatherman.github.io/gtUtils/reference/gt_snake.md)
lays out the table, so a frame you computed against the long data still
lines up after the snake. It is the escape hatch for a precomputed grid
you do not want to rebuild as a condition: a logical mask for
[`gt_highlight_cells()`](https://andreweatherman.github.io/gtUtils/reference/gt_highlight_cells.md),
a frame of per-cell colors or text, anything the same shape as the
snaked body.

## Usage

``` r
gt_snake_align(x, n_cols = 2, rows_per_col = NULL, fill = NA)
```

## Arguments

- x:

  A data frame or matrix with one row per row of the un-snaked table.

- n_cols:

  Integer. The number of blocks, matching the
  [`gt_snake()`](https://andreweatherman.github.io/gtUtils/reference/gt_snake.md)
  call. Defaults to `2`.

- rows_per_col:

  Integer. Rows in each block. Given this, `n_cols` is worked out from
  the data instead, exactly as
  [`gt_snake()`](https://andreweatherman.github.io/gtUtils/reference/gt_snake.md)
  does it. Defaults to `NULL`.

- fill:

  What to put in the trailing cells when the rows do not divide evenly.
  Defaults to `NA`, which
  [`gt_highlight_cells()`](https://andreweatherman.github.io/gtUtils/reference/gt_highlight_cells.md)
  reads as no match.

## Value

Returns a data frame of the reshaped blocks.

## Details

Most of the time you do not need this. Body-cell styling applied before
[`gt_snake()`](https://andreweatherman.github.io/gtUtils/reference/gt_snake.md)
is carried through the reshape on its own, so coloring the long grid and
then snaking already lands the fills in the right place. Reach for this
only when the grid lives outside the table's styles.

Pass the same `n_cols` (or `rows_per_col`) you passed to
[`gt_snake()`](https://andreweatherman.github.io/gtUtils/reference/gt_snake.md).
Each block's columns are suffixed with its block number, `_1`, `_2`, and
so on, to match the names
[`gt_snake()`](https://andreweatherman.github.io/gtUtils/reference/gt_snake.md)
gives the table. The result has `ceiling(nrow(x) / n_cols)` rows and
`ncol(x) * n_cols` columns, so it is the shape
[`gt_highlight_cells()`](https://andreweatherman.github.io/gtUtils/reference/gt_highlight_cells.md)
expects for the block of snaked columns.

## See also

[`gt_snake()`](https://andreweatherman.github.io/gtUtils/reference/gt_snake.md),
which reshapes the table itself and carries body styling through.

## Examples

``` r
if (FALSE) { # \dontrun{
library(gt)

wide <- head(mtcars[c("mpg", "hp", "wt", "qsec")], 8)

# a logical grid the same shape as the data, built before the snake
mask <- wide > 20

# after the snake, each column carries its block number as a suffix
snaked <- unlist(lapply(1:2, function(i) paste0(names(wide), "_", i)))

wide %>%
  gt() %>%
  gt_snake(n_cols = 2) %>%
  gt_highlight_cells(tidyselect::all_of(snaked),
                     condition = gt_snake_align(mask, n_cols = 2),
                     fill = "#EDBD68")
} # }
```
