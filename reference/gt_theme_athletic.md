# The Athletic theme for `gt` tables

Modeled on The Athletic's tables. A monospaced Spline Sans Mono body,
uppercase Work Sans labels and titles, dotted rules between rows, thin
vertical rules separating the columns, and a solid black row-group band
with knocked-out white labels. All columns are centered.

## Usage

``` r
gt_theme_athletic(
  gt_object,
  density = c("comfortable", "compact", "social"),
  ...
)
```

## Arguments

- gt_object:

  A `gt` table object to modify.

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

The row rules are a dotted top border applied to every body row, and the
column separators are a thin left border on every column except the
first, so the stub reads without a leading rule. Both are drawn with
[`gt::cell_borders()`](https://gt.rstudio.com/reference/cell_borders.html)
rather than table options. A table id is resolved (or generated) up
front so the
[`gt::opt_css()`](https://gt.rstudio.com/reference/opt_css.html) block
can pin the closing row border and heading padding to this table alone.
`density` rescales the finished table, since this theme sets its sizes
directly rather than deriving them from a scale.

## Density

`density` scales the theme's type and row padding together.
`"comfortable"` leaves every size as the theme sets it, `"compact"`
scales both down, and `"social"` scales both up, to the scale
[`gt_save_crop()`](https://andreweatherman.github.io/gtUtils/reference/gt_save_crop.md)
and
[`gt_social_crop()`](https://andreweatherman.github.io/gtUtils/reference/gt_social_crop.md)
export at.

## Figures

![](figures/gt_theme_athletic.png)

## Examples

``` r
if (FALSE) { # \dontrun{
library(gt)
gt(head(mtcars)) %>% gt_theme_athletic()
gt(head(mtcars)) %>% gt_theme_athletic(density = "compact")
} # }
```
