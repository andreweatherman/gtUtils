# Flag outlying values in a `gt` table

Marks cells that fall outside a threshold, either beyond an
interquartile fence, beyond a number of standard deviations, or outside
bounds you supply.

## Usage

``` r
gt_outliers(
  gt_object,
  columns,
  method = c("iqr", "sd", "bounds"),
  threshold = NULL,
  bounds = NULL,
  side = c("both", "high", "low"),
  fill = NULL,
  color = NULL,
  bold = TRUE,
  symbol = NULL,
  note = NULL
)
```

## Arguments

- gt_object:

  A `gt` table object to modify.

- columns:

  The column or columns to test. Non-numeric columns are skipped.

- method:

  Character. How to decide what counts as an outlier. One of `"iqr"`,
  `"sd"`, or `"bounds"`. Defaults to `"iqr"`.

- threshold:

  Numeric. The cutoff used by `"iqr"` and `"sd"`. If `NULL`, the
  convention for the method is used: `1.5` for `"iqr"` and `3` for
  `"sd"`. Defaults to `NULL`.

- bounds:

  A length-2 numeric vector giving `c(lower, upper)`, required when
  `method` is `"bounds"`. Use `NA` for an open end, such as
  `c(NA, 100)`. Defaults to `NULL`.

- side:

  Character. Which tail to flag. One of `"both"`, `"high"`, or `"low"`.
  Defaults to `"both"`.

- fill:

  Optional. A hex color for the cell fill behind flagged values.
  Defaults to `NULL`, which applies no fill.

- color:

  Optional. A hex color for flagged text. If `NULL`, a warning red is
  used, swapped for a readable alternative when it would not have enough
  contrast against `fill`. Defaults to `NULL`.

- bold:

  Logical. Should flagged values be bolded? Defaults to `TRUE`.

- symbol:

  Optional. A marker appended to flagged values, such as `"†"`. Defaults
  to `NULL`.

- note:

  Optional. A source note describing the rule that was applied. Pass
  `TRUE` for wording generated from `method` and `threshold`, a string
  for your own, or `NULL` for no note. Defaults to `NULL`.

## Value

Returns a modified `gt` table with outlying values marked.

## Details

[`gt::data_color()`](https://gt.rstudio.com/reference/data_color.html)
shades a continuous scale; this flags a value as unusual. Thresholds are
computed separately for each column, so every column is judged against
its own distribution.

The default rule is the interquartile fence, not standard deviations,
because an SD fence is built from a spread that the outlier itself
inflates. On `c(10.2, 10.4, 10.1, 19.8, 10.3, 10.0)` the mean is 11.8
and the standard deviation 3.92, so a three-SD fence reaches 23.6 and
misses the 19.8. The quartiles barely move, so the IQR fence stops at
10.75 and catches it. The masking is worst in small samples. Use
`method = "sd"` if you want it anyway.

## See also

[`gt_highlight_na()`](https://andreweatherman.github.io/gtUtils/reference/gt_highlight_na.md)
for missing values, and
[`gt_spotlight()`](https://andreweatherman.github.io/gtUtils/reference/gt_spotlight.md)
for drawing attention to whole rows.

## Examples

``` r
if (FALSE) { # \dontrun{
library(gt)

assays <- data.frame(
  Sample = paste0("S", 1:6),
  Run1 = c(10.2, 10.4, 10.1, 19.8, 10.3, 10.0),
  Run2 = c(9.9, 10.1, 10.3, 10.2, 2.1, 10.4)
)

# the default fence catches both the high and the low reading
gt(assays) %>% gt_outliers(c(Run1, Run2), note = TRUE)

# an explicit acceptance range, flagging only the high side
gt(assays) %>%
  gt_outliers(c(Run1, Run2), method = "bounds", bounds = c(9, 11),
              side = "high", fill = "#FDECEA", symbol = "†")

# works on wider data too
gt(head(airquality, 12)) %>% gt_outliers(c(Ozone, Wind, Temp))
} # }
```
