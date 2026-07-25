# Color and bold the tier column of a `gt` table

Fills the cells of a `tier` column by tier level and bolds them, pairing
each level with a color. The remaining columns are rendered as images,
since the intended layout is a tier list with logos or headshots ranked
into colored bands.

## Usage

``` r
gt_tiers(
  gt_object,
  levels,
  colors = NULL,
  style = "dark",
  img_height = "55px",
  tier_column = "tier",
  image_columns = NULL
)
```

## Arguments

- gt_object:

  A `gt` table object to modify.

- levels:

  Character. The tier levels, matching the values in the table's tier
  column. May instead be a single named vector of `level = color`, in
  which case `colors` is left unset. That is the same shape
  [`gt_legend_discrete()`](https://andreweatherman.github.io/gtUtils/reference/gt_legend_discrete.md)
  takes, so one object can drive both.

- colors:

  Character. Hex color codes paired with `levels`, in the same order.
  Must be the same length as `levels`. Defaults to `NULL`, which
  requires `levels` to carry the colors as names.

- style:

  Character. The color scheme, passed to
  [`gt_theme_tier()`](https://andreweatherman.github.io/gtUtils/reference/gt_theme_tier.md).
  Either `"dark"` for a near-black ground or `"light"` for a white one.
  Defaults to `"dark"`.

- img_height:

  Character. The height of the images rendered in the image columns, as
  a CSS size. Defaults to `"55px"`.

- tier_column:

  Character. The name of the column holding the tier levels. Defaults to
  `"tier"`.

- image_columns:

  Optional. The columns to render as images. When `NULL`, every column
  other than `tier_column` is rendered as images. Defaults to `NULL`.

## Value

Returns a modified `gt` table with the tier column colored and bolded.

## Details

The theme is applied once with
[`gt_theme_tier()`](https://andreweatherman.github.io/gtUtils/reference/gt_theme_tier.md),
the image columns are passed through
[`gt::fmt_image()`](https://gt.rstudio.com/reference/fmt_image.html) at
`img_height`, and all column labels are cleared, so the input for those
columns must be image paths or URLs. The function then reduces over
`levels`, and for each level fills the matching `tier_column` cells with
the paired color and sets their text to black or white, whichever
measures higher contrast against that fill, so each band keeps a legible
label. Naming `image_columns` leaves the rest untouched, so a text
column can sit alongside the images.

## Examples

``` r
if (FALSE) { # \dontrun{
library(gt)

standings <- data.frame(
  tier = c("A", "A", "B"),
  logo = c("https://example.com/1.png",
           "https://example.com/2.png",
           "https://example.com/3.png")
)

gt(standings) %>%
  gt_tiers(levels = c("A", "B"), colors = c("#1B7837", "#B2182B"))
} # }
```
