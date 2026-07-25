# Save a matched set of images, one per group

Splits data by a column, builds a table for each group with a function
you supply, and writes one image per group. The images are padded to a
common width so a posted series does not come out at ragged sizes.

## Usage

``` r
gt_save_batch(
  data,
  group,
  fn,
  file,
  dir = ".",
  match_width = TRUE,
  bg = "white",
  whitespace = 50,
  zoom = 2,
  quiet = FALSE
)
```

## Arguments

- data:

  A data frame to split.

- group:

  The column to split on.

- fn:

  A function building one table. It is called as `fn(df, group)`, where
  `df` is that group's rows and `group` is its value, and it must return
  a `gt_tbl`.

- file:

  Character. A file name pattern containing `{group}`, which is replaced
  by the group value, as `"net-{group}.png"`.

- dir:

  Character. The directory to write into. Created if it does not exist.
  Defaults to `"."`.

- match_width:

  Logical. Should every image be padded to the width of the widest one,
  so a posted series shares one width? Defaults to `TRUE`.

- bg:

  Character. The background color of the padding. Defaults to `"white"`.

- whitespace:

  Numeric. Padding left around each table, in pixels. Defaults to `50`.

- zoom:

  Numeric. The rendering zoom factor. Defaults to `2`.

- quiet:

  Logical. Suppress the per-group progress message. Defaults to `FALSE`.

## Value

Invisibly, a character vector of the files written.

## Details

A group whose table fails to build does not abandon the run. The error
is collected, the remaining groups are written, and a warning at the end
names every group that failed.

Group values are used in file names with anything awkward replaced by a
dash, so a group called `"North / East"` writes to `north-east`.

Without `match_width`, each image is trimmed to its own content, so a
set posted together can come out at different widths. With it, every
image is padded to the width of the widest before its border is added.

## See also

[`gt_grid()`](https://andreweatherman.github.io/gtUtils/reference/gt_grid.md)
for the same split composed into one image instead.

## Examples

``` r
if (FALSE) { # \dontrun{
library(gt)

build <- function(df, group) {
  gt(df[c("mpg", "hp", "wt")]) %>%
    gt_theme_broadsheet() %>%
    tab_header(title = paste(group, "cylinders"))
}

gt_save_batch(mtcars, cyl, build, "cars-{group}.png", dir = "out")
} # }
```
