# Deprecated legend functions

These are the former names of the two legend helpers, kept for backward
compatibility. Use the new names instead.

## Usage

``` r
gt_color_legend(...)

gt_centered_legend(gt_table, ...)
```

## Arguments

- ...:

  Passed on to the replacement function.

- gt_table:

  The `gt` table, forwarded as `gt_object`.

## Value

Returns a modified `gt` table, from the replacement function.

## Details

- `gt_color_legend()` is now
  [`gt_legend_continuous()`](https://andreweatherman.github.io/gtUtils/reference/gt_legend_continuous.md).

- `gt_centered_legend()` is now
  [`gt_legend_discrete()`](https://andreweatherman.github.io/gtUtils/reference/gt_legend_discrete.md).
