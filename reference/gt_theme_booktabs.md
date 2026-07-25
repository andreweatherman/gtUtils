# Academic booktabs theme for `gt` tables

Three horizontal rules and nothing else, the way LaTeX booktabs draws
them. A heavy rule sits above the column labels, a lighter one below
them, and a heavy rule closes the body. There are no vertical rules, no
rules between data rows, and no fills. The type is a Times-compatible
serif.

## Usage

``` r
gt_theme_booktabs(
  gt_object,
  accent = "#111111",
  density = c("comfortable", "compact", "social"),
  ...
)
```

## Arguments

- gt_object:

  A `gt` table object to modify.

- accent:

  Character. A hex color for the three rules and the row-group labels.
  Defaults to `"#111111"`, near-black, for the standard black rules.

- density:

  Character. The type and padding scale. One of `"comfortable"`,
  `"compact"`, or `"social"`. See Density. Defaults to `"comfortable"`.

- ...:

  Additional arguments passed to
  [`gt::tab_options`](https://gt.rstudio.com/reference/tab_options.html),
  applied last so they override anything the theme sets.

## Value

Returns a modified `gt` table with the theme applied.

## Details

`accent` recolors the three rules and the row-group labels.

## Density

`density` sets the type and padding scale together. `"comfortable"` uses
a 14px body with roomy rows, `"compact"` a 12px body with tight rows,
and `"social"` a 17px body with generous rows and a larger title, at the
scale
[`gt_save_crop()`](https://andreweatherman.github.io/gtUtils/reference/gt_save_crop.md)
and
[`gt_social_crop()`](https://andreweatherman.github.io/gtUtils/reference/gt_social_crop.md)
export at.

## Figures

![](figures/gt_theme_booktabs.png)

## See also

[`gt_theme_tufte()`](https://andreweatherman.github.io/gtUtils/reference/gt_theme_tufte.md)
for a lighter, minimal-ink relative, and
[`gt_significance()`](https://andreweatherman.github.io/gtUtils/reference/gt_significance.md)
for significance notation.

## Examples

``` r
if (FALSE) { # \dontrun{
library(gt)

gt(head(mtcars[c("mpg", "hp", "wt")], 8)) %>% gt_theme_booktabs()

# a regression table, with significance stars and a colored rule
fit <- lm(mpg ~ wt + hp, data = mtcars)
data.frame(
  Term = names(coef(fit)),
  Estimate = coef(fit),
  p = summary(fit)$coefficients[, 4]
) %>%
  gt() %>%
  fmt_number(Estimate, decimals = 3) %>%
  gt_significance(Estimate, p) %>%
  gt_theme_booktabs(accent = "#1A3E6F")
} # }
```
