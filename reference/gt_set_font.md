# Set one font family across a whole `gt` table

Applies a single font family to every styleable part of a `gt` table in
one call: the title and subtitle, the stubhead, column spanners and
labels, row groups, the stub, the body, footnotes, and source notes.
Setting the same font through
[`gt::tab_options()`](https://gt.rstudio.com/reference/tab_options.html)
means naming each `*_font` option in turn.

## Usage

``` r
gt_set_font(
  gt_object,
  font_family,
  from_google_font = TRUE,
  weight = NULL,
  style = NULL,
  gt_table = NULL
)
```

## Arguments

- gt_object:

  A `gt` table object to modify.

- font_family:

  Character. The font family to apply to the whole table.

- from_google_font:

  Logical. Should the font be pulled from Google Fonts through
  [`gt::google_font()`](https://gt.rstudio.com/reference/google_font.html)?
  `FALSE` treats `font_family` as a font already installed on the local
  machine. Defaults to `TRUE`.

- weight:

  The font weight applied to every part, passed to
  [`gt::cell_text()`](https://gt.rstudio.com/reference/cell_text.html).
  Either a keyword such as `"bold"` or a numeric weight. Defaults to
  `NULL`, which leaves the weight alone.

- style:

  Character. The font style applied to every part, one of `"normal"`,
  `"italic"`, or `"oblique"`. Defaults to `NULL`, which leaves the style
  alone.

- gt_table:

  Deprecated. Use `gt_object`.

## Value

Returns a modified `gt` table with the font family applied to every
covered part.

## Details

The font is applied with a single
[`gt::tab_style()`](https://gt.rstudio.com/reference/tab_style.html)
over a list of cell locations covering each part of the table. Summary
and grand-summary cells are not included, so a font set here does not
reach them. When `from_google_font` is `TRUE` the family is wrapped in
[`gt::google_font()`](https://gt.rstudio.com/reference/google_font.html),
which adds the import so the font renders in an exported table without
relying on it being installed locally.

## Examples

``` r
if (FALSE) { # \dontrun{
library(gt)

gt(head(mtcars)) %>% gt_set_font("Oswald")

# use a font already installed locally
gt(head(iris)) %>% gt_set_font("Helvetica", from_google_font = FALSE)
} # }
```
