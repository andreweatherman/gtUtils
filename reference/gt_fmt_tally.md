# Combine count columns into a single cell in a `gt` table

Merges two or more count columns into one `"32-5"` style cell,
optionally with one of them shown as a share of the row total. Any set
of tallies works, from wins and losses to tests passed and failed.

## Usage

``` r
gt_fmt_tally(
  gt_object,
  columns,
  separator = "-",
  label = NULL,
  share = FALSE,
  share_of = 1,
  share_location = c("inline", "column"),
  share_decimals = 1,
  share_label = "%",
  share_prefix = " (",
  share_suffix = ")",
  ...
)
```

## Arguments

- gt_object:

  A `gt` table object to modify.

- columns:

  The count columns to combine, in the order they should read. Two or
  more.

- separator:

  Character. The string placed between the counts. Defaults to `"-"`.

- label:

  Optional. A new label for the combined column. Defaults to `NULL`,
  which leaves the existing label alone.

- share:

  Logical. Should one of the counts be shown as a share of the row
  total? Defaults to `FALSE`.

- share_of:

  The column the share is computed for, either a position or a name.
  Defaults to `1`, the first column.

- share_location:

  Character. Where the share goes. Either `"inline"` to append it to the
  combined cell, or `"column"` to reuse the last of `columns` for it.
  Defaults to `"inline"`.

- share_decimals:

  Integer. The number of decimal places for the share. Defaults to `1`.

- share_label:

  Character. The label for the share column when `share_location` is
  `"column"`. Defaults to `"%"`.

- share_prefix:

  Character. The string placed before an inline share. Defaults to
  `" ("`.

- share_suffix:

  Character. The string placed after an inline share. Defaults to `")"`.

- ...:

  Additional arguments passed to
  [`gt::vec_fmt_percent`](https://gt.rstudio.com/reference/vec_fmt_percent.html).

## Value

Returns a modified `gt` table with the counts combined.

## Details

The counts are written into the first column and the rest are hidden, or
the last one is reused to carry the share, so the table ends up narrower
than it started.

[`gt::cols_merge()`](https://gt.rstudio.com/reference/cols_merge.html)
will join columns with a pattern and
[`gt::cols_merge_n_pct()`](https://gt.rstudio.com/reference/cols_merge_n_pct.html)
will pair a count with a percentage column you have already built. The
difference here is that the share is computed for you from the counts
themselves.

A row is left alone if any of its counts are missing, so a partial tally
is never shown. The share is also left blank where the counts sum to
zero.

## See also

[`gt_fmt_rank()`](https://andreweatherman.github.io/gtUtils/reference/gt_fmt_rank.md)
for ordinal formatting.

## Examples

``` r
if (FALSE) { # \dontrun{
library(gt)

suites <- data.frame(
  Suite = c("Parser", "Renderer", "Exporter"),
  Passed = c(142, 98, 211),
  Failed = c(8, 2, 17)
)

# renders as "142-8"
gt(suites) %>% gt_fmt_tally(c(Passed, Failed), label = "Result")

# renders as "142-8 (94.7%)"
gt(suites) %>% gt_fmt_tally(c(Passed, Failed), share = TRUE)

# the failure rate instead, in its own column
gt(suites) %>%
  gt_fmt_tally(c(Passed, Failed), share = TRUE, share_of = "Failed",
               share_location = "column", share_label = "Fail rate")

# three counts, as in a league table
league <- data.frame(
  Club = c("Arsenal", "Chelsea"),
  W = c(26, 18), D = c(6, 10), L = c(6, 10)
)

gt(league) %>% gt_fmt_tally(c(W, D, L), label = "W-D-L")
} # }
```
