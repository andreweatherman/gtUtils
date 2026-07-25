# Band alternate row groups of a `gt` table

Shades every other row group, so each group reads as a block.
[`gt::opt_row_striping()`](https://gt.rstudio.com/reference/opt_row_striping.html)
bands every other row, which fights the grouping on a table that is
already divided into sections.

## Usage

``` r
gt_group_stripes(gt_object, color = "#F5F5F5", start = 2, include_stub = TRUE)
```

## Arguments

- gt_object:

  A `gt` table object to modify. It must have row groups.

- color:

  Character. A hex color for the banded groups. Defaults to `"#F5F5F5"`.

- start:

  Integer. Which group to begin banding on, in the order the groups are
  rendered. `2` leaves the first group unshaded, `1` shades it. Defaults
  to `2`.

- include_stub:

  Logical. Should the stub column be banded along with the body?
  Defaults to `TRUE`.

## Value

Returns a modified `gt` table with alternate groups banded.

## Details

Groups are banded in the order they render, so this follows
[`gt::row_group_order()`](https://gt.rstudio.com/reference/row_group_order.html)
rather than the order the groups happen to appear in the data.

The fill is applied to body cells rather than through CSS, because a
group heading occupies a row of its own and shifts every `nth-child`
count below it. Group heading rows are left alone;
`gt::tab_options(row_group.background.color)` sets those, and it sets
all of them at once.

## Examples

``` r
if (FALSE) { # \dontrun{
library(gt)

cars <- mtcars[c("mpg", "hp", "wt")]
cars$cyl <- paste(mtcars$cyl, "cylinders")

gt(head(cars, 15), groupname_col = "cyl") %>%
  gt_theme_broadsheet() %>%
  gt_group_stripes()

# shade from the first group instead, in a warmer tone
gt(head(cars, 15), groupname_col = "cyl") %>%
  gt_group_stripes(color = "#FBF3E4", start = 1)
} # }
```
