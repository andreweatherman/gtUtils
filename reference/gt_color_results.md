# Color rows of a `gt` table by win, loss, or tie result

Fills and recolors each row according to a result read from a column.
Results can be encoded as `"W"` and `"L"` or as `1` and `0`, with an
optional tie state. Rows holding none of the values are left unchanged.

## Usage

``` r
gt_color_results(
  gt_object,
  result_column = "result",
  win_color = "#5DA271",
  loss_color = "#C84630",
  tie_color = NULL,
  wins_text_color = "white",
  loss_text_color = "white",
  tie_text_color = "white",
  tie_value = "T",
  result_type = "wl"
)
```

## Arguments

- gt_object:

  A `gt` table object to modify.

- result_column:

  The column holding the result indicators, as a bare column name or a
  string. Defaults to `"result"`.

- win_color:

  Character. The background fill for winning rows. Defaults to
  `"#5DA271"`.

- loss_color:

  Character. The background fill for losing rows. Defaults to
  `"#C84630"`.

- tie_color:

  Optional. The background fill for tie rows. When `NULL`, ties are not
  colored. Defaults to `NULL`.

- wins_text_color:

  Character. The text color for winning rows. Defaults to `"white"`.

- loss_text_color:

  Character. The text color for losing rows. Defaults to `"white"`.

- tie_text_color:

  Character. The text color for tie rows. Defaults to `"white"`.

- tie_value:

  The value in `result_column` marking a tie, used when `tie_color` is
  set. Defaults to `"T"`.

- result_type:

  Character. The encoding of `result_column`. Either `"wl"` for `"W"`
  and `"L"`, or `"binary"` for `1` and `0`. Defaults to `"wl"`.

## Value

Returns a modified `gt` table with winning and losing rows colored.

## Details

A [`gt::tab_style()`](https://gt.rstudio.com/reference/tab_style.html)
pass fills and recolors the body rows whose `result_column` equals the
win value, then the loss value, and then, when `tie_color` is set, the
`tie_value`. Under `result_type = "wl"` the win and loss values compared
are `"W"` and `"L"`; under `"binary"` they are `1` and `0`. Any row
matching none of the values keeps its existing styling.

## Examples

``` r
if (FALSE) { # \dontrun{
library(gt)

results <- data.frame(
  game = paste("Game", 1:4),
  pts = c(88, 74, 102, 65),
  result = c("W", "L", "W", "L")
)

gt(results) %>% gt_color_results()

# binary encoding, with custom colors
results$result <- c(1, 0, 1, 0)
gt(results) %>%
  gt_color_results(result_type = "binary", win_color = "#1B7837",
                   loss_color = "#762A83")
} # }
```
