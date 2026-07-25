# Scale columns and disclose the scaling

Divides the selected columns and records the scaling in the same call,
so the numbers and the disclosure cannot drift apart.

## Usage

``` r
gt_scale_note(
  gt_object,
  columns,
  divisor = 1000,
  note = NULL,
  where = c("source_note", "label", "both"),
  label_suffix = NULL,
  decimals = 0,
  ...
)
```

## Arguments

- gt_object:

  A `gt` table object to modify.

- columns:

  The column or columns to scale.

- divisor:

  Numeric. The amount to divide by. Defaults to `1000`.

- note:

  Optional. The disclosure text. If `NULL`, it is derived from
  `divisor`, giving "Figures in thousands." for `1e3`, "Figures in
  millions." for `1e6`, and so on. Divisors without a common name fall
  back to "Figures divided by 2,500." Defaults to `NULL`.

- where:

  Character. Where the disclosure goes. One of `"source_note"`,
  `"label"` to append a suffix to the column labels instead, or
  `"both"`. Defaults to `"source_note"`.

- label_suffix:

  Optional. The suffix appended to column labels when `where` includes
  `"label"`. If `NULL`, it is derived from `divisor` (for example,
  `"(000s)"`). Defaults to `NULL`.

- decimals:

  Integer. The number of decimal places for the scaled values. Defaults
  to `0`.

- ...:

  Additional arguments passed to
  [`gt::fmt_number`](https://gt.rstudio.com/reference/fmt_number.html).

## Value

Returns a modified `gt` table with the columns scaled and the scaling
disclosed.

## Details

Scaling is applied through
[`gt::fmt_number()`](https://gt.rstudio.com/reference/fmt_number.html)
and its `scale_by` argument, so the underlying data is left untouched.

## Examples

``` r
if (FALSE) { # \dontrun{
library(gt)

revenue <- data.frame(
  Segment = c("Cloud", "Devices", "Services"),
  FY24 = c(4820000, 2110000, 1360000),
  FY23 = c(4100000, 2260000, 1180000)
)

# values render as 4.8, 2.1, 1.4 with "Figures in millions." beneath
gt(revenue) %>% gt_scale_note(c(FY24, FY23), divisor = 1e6, decimals = 1)

# disclose in the column labels instead
gt(revenue) %>% gt_scale_note(c(FY24, FY23), divisor = 1e3, where = "label")
} # }
```
