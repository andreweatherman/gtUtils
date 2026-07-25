# Minimal-ink theme for `gt` tables

A spare, Tufte-inspired look. An old-style serif on a warm white ground,
italic column labels, and a single hairline under them. There are no
vertical rules, no rules between rows, and no fills. A faint hairline
closes the body.

## Usage

``` r
gt_theme_tufte(
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

  Character. A hex color for the header hairline and the row-group
  labels. Defaults to `"#111111"`, near-black. A muted red or rust gives
  the Tufte marginal accent.

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

`accent` recolors the header hairline and the row-group labels.

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

![](figures/gt_theme_tufte.png)

## See also

[`gt_theme_booktabs()`](https://andreweatherman.github.io/gtUtils/reference/gt_theme_booktabs.md)
for a firmer, academic relative.

## Examples

``` r
if (FALSE) { # \dontrun{
library(gt)

gt(head(mtcars[c("mpg", "hp", "wt")], 8)) %>% gt_theme_tufte()

# a muted rust accent on the hairline
gt(head(airquality, 8)) %>% gt_theme_tufte(accent = "#7B3F2B")
} # }
```
