# Add significance stars to a `gt` table

Appends significance notation to an estimate column based on a column of
p-values, then writes the matching legend into a source note. The
p-value column is hidden by default, since the stars stand in for it.
This is the usual convention for regression tables in the social and
medical sciences.

## Usage

``` r
gt_significance(
  gt_object,
  columns,
  p_columns,
  levels = c(0.01, 0.05, 0.1),
  symbols = c("***", "**", "*"),
  superscript = TRUE,
  size = "0.7em",
  legend = TRUE,
  legend_text = NULL,
  hide_p = TRUE
)
```

## Arguments

- gt_object:

  A `gt` table object to modify.

- columns:

  The estimate column or columns to annotate.

- p_columns:

  The column or columns holding the p-values, paired positionally with
  `columns`. Supply one p-value column per estimate column.

- levels:

  A numeric vector of significance thresholds, in ascending order
  (strictest first). Defaults to `c(0.01, 0.05, 0.1)`.

- symbols:

  A character vector of notation for each level. Must be the same length
  as `levels`. Defaults to `c("***", "**", "*")`.

- superscript:

  Logical. Should the stars be rendered as superscript? Defaults to
  `TRUE`.

- size:

  Character. The size of the stars, as a CSS size. Defaults to
  `"0.7em"`.

- legend:

  Logical. Should the legend be added as a source note? Defaults to
  `TRUE`.

- legend_text:

  Optional. Custom legend text. If `NULL`, the legend is built from
  `levels` and `symbols`. Defaults to `NULL`.

- hide_p:

  Logical. Should the p-value columns be hidden once the stars are
  applied? Defaults to `TRUE`.

## Value

Returns a modified `gt` table with significance notation applied.

## Details

The legend is generated from the same `levels` and `symbols` used to
place the stars, so changing one updates the other.

Each value takes the notation for the strictest threshold it satisfies,
so with the defaults a p-value of 0.004 gets `***` and not `*`. Values
that meet none of the thresholds are left alone, as are `NA` p-values.

## See also

[`gt_fmt_rank()`](https://andreweatherman.github.io/gtUtils/reference/gt_fmt_rank.md),
which uses the same superscript approach for ordinal suffixes.

## Examples

``` r
if (FALSE) { # \dontrun{
library(gt)

fit <- lm(mpg ~ wt + hp + factor(cyl), data = mtcars)
results <- data.frame(
  Term = rownames(summary(fit)$coefficients),
  Estimate = summary(fit)$coefficients[, 1],
  SE = summary(fit)$coefficients[, 2],
  p = summary(fit)$coefficients[, 4]
)

gt(results) %>%
  fmt_number(c(Estimate, SE), decimals = 3) %>%
  gt_significance(Estimate, p)

# daggers instead of stars, at a single threshold
gt(results) %>%
  gt_significance(Estimate, p, levels = 0.05, symbols = "†",
                  legend_text = "† p < .05")
} # }
```
