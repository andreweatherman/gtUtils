# Save a `gt` table onto a fixed-ratio canvas

A variant of
[`gt_save_crop()`](https://andreweatherman.github.io/gtUtils/reference/gt_save_crop.md)
that centers the trimmed table on a canvas of a given aspect ratio, for
posting to social platforms. The table itself is never cropped; the
short side of the canvas is expanded until the ratio is met.

## Usage

``` r
gt_social_crop(
  data,
  file = NULL,
  aspect_ratio = "1:1",
  bg = "white",
  whitespace = 60,
  gravity = "center",
  zoom = 2,
  expand = 5,
  width = NULL
)
```

## Arguments

- data:

  A `gt` table object to save.

- file:

  Optional. A path to write the image to. Defaults to `NULL`.

- aspect_ratio:

  Character or numeric. The target ratio, given as `"1:1"`, `"16:9"`,
  `"4:5"`, or a number. Defaults to `"1:1"`.

- bg:

  Character. The background color of the canvas. Defaults to `"white"`.

- whitespace:

  Numeric. The amount of whitespace to leave around the table before
  padding out to the ratio. Defaults to `60`.

- gravity:

  Character. Where the table sits on the canvas, passed to
  [`magick::image_extent`](https://docs.ropensci.org/magick/reference/transform.html).
  Defaults to `"center"`.

- zoom:

  Numeric. The rendering zoom factor. Defaults to `2`.

- expand:

  Numeric. The pixel expansion passed to the underlying save. Defaults
  to `5`.

- width:

  Optional. A final output width in pixels. The finished canvas is
  scaled to it, keeping the aspect ratio, so a series of posts can share
  one width. Defaults to `NULL`.

## Value

Returns the cropped and padded image file.

## See also

[`gt_save_crop()`](https://andreweatherman.github.io/gtUtils/reference/gt_save_crop.md)
for a plain trimmed save.

## Examples

``` r
if (FALSE) { # \dontrun{
library(gt)

gt(head(mtcars)) %>%
  gt_theme_broadsheet(density = "social") %>%
  gt_social_crop("mtcars.png", aspect_ratio = "4:5")

# match the canvas to a dark theme
gt(head(mtcars)) %>%
  gt_theme_midnight() %>%
  gt_social_crop("dark.png", bg = "#0C0D10")
} # }
```
