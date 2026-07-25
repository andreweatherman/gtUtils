# Preview data in every theme

Renders the same few rows through each `gt_theme_*` function in the
package and lays the results out in a grid, one panel per theme, labeled
with its name.

## Usage

``` r
gt_theme_preview(
  data,
  themes = NULL,
  n = 5,
  ncol = 3,
  density = "compact",
  file = NULL,
  ...
)
```

## Arguments

- data:

  A data frame. A `gt` table is also accepted, in which case its
  underlying data is used.

- themes:

  Character. The theme functions to show, by name. If `NULL`, every
  `gt_theme_*` in the package is used. Defaults to `NULL`.

- n:

  Integer. How many rows of `data` to show in each panel. Defaults to
  `5`.

- ncol:

  Integer. The number of panels across. Defaults to `3`.

- density:

  Character. A `density` passed to every theme, so the panels are
  comparable. If `NULL`, each theme uses its own default. Defaults to
  `"compact"`.

- file:

  Optional. A path to write a PNG to. If `NULL`, the grid is returned
  for the viewer. Defaults to `NULL`.

- ...:

  Further arguments passed to
  [`gt_grid()`](https://andreweatherman.github.io/gtUtils/reference/gt_grid.md),
  such as `gap` or `bg`.

## Value

Displays the grid in the viewer, or writes it to `file`.

## Details

Each panel is captioned with the theme's name in a neutral style, set
outside the table. Using each theme's own heading instead would let a
wide display title stretch its panel out of shape.

Themes that take extra arguments, such as `style` on
[`gt_theme_sofa()`](https://andreweatherman.github.io/gtUtils/reference/gt_theme_sofa.md),
are shown at their defaults.

## See also

[`gt_grid()`](https://andreweatherman.github.io/gtUtils/reference/gt_grid.md),
which does the layout.

## Examples

``` r
if (FALSE) { # \dontrun{
gt_theme_preview(mtcars[c("mpg", "cyl", "hp")])

# a subset, sized for a wide screenshot
gt_theme_preview(
  iris,
  themes = c("gt_theme_broadsheet", "gt_theme_swiss", "gt_theme_midnight"),
  ncol = 3, file = "themes.png"
)
} # }
```
