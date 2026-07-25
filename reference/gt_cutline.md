# Draw a labeled cut line between rows of a `gt` table

Draws a rule across the table after a given row, with an optional label
on it. This is the cut line convention used in ranked and sorted tables
to mark a cutoff or qualifying threshold.

## Usage

``` r
gt_cutline(
  gt_object,
  after,
  label = NULL,
  color = "#A6081A",
  weight = 2,
  style = "dashed",
  label_color = NULL,
  label_size = 9,
  label_position = c("below", "above"),
  gap = 0
)
```

## Arguments

- gt_object:

  A `gt` table object to modify.

- after:

  Numeric. The row number or numbers to draw a line after. `after = 4`
  draws between rows 4 and 5. Vectorized, so `c(4, 25)` draws two lines.
  `after = 0` draws a line above the first row, which is how you label
  the top section of a table sorted into a high group and a low group.

- label:

  Optional. The label or labels for the lines, recycled against `after`.
  Use `NA` for an unlabeled line among labeled ones. Rendered in
  uppercase. Defaults to `NULL`.

- color:

  Character. A hex color for the rule. Defaults to `"#A6081A"`.

- weight:

  Numeric. The thickness of the rule in pixels. Defaults to `2`.

- style:

  Character. The line style. One of `"dashed"`, `"solid"`, or
  `"dotted"`. Defaults to `"dashed"`.

- label_color:

  Optional. A hex color for the label. Defaults to `NULL`, which uses
  `color`.

- label_size:

  Numeric. The label size in pixels. Defaults to `9`.

- label_position:

  Character. Either `"below"` to put the label under the line or
  `"above"` to put it over. Defaults to `"below"`.

- gap:

  Numeric. Extra space in pixels added around the line to set the two
  sections further apart. A single number is applied above and below the
  line; a length-2 vector `c(above, below)` sets the two sides
  separately, so `c(0, 12)` opens space only under the line. Applies to
  labeled and unlabeled lines alike. Defaults to `0`.

## Value

Returns a modified `gt` table with the cut line or lines added.

## Details

The line is positional, not data-driven.
[`gt::tab_row_group()`](https://gt.rstudio.com/reference/tab_row_group.html)
splits a table on a value in the data and turns the result into labeled
sections with heading rows. A cut line leaves the table flowing and
marks a fixed row number.

The label is drawn as an inline SVG background image on the row, because
`gt`'s CSS inliner strips the `::before` and `::after` pseudo-elements
this would normally be built with. That costs two things. An SVG
background cannot use the page's webfonts, so the label renders in a
system sans-serif rather than the theme's font. And a background image
cannot straddle a border, so the labeled row is given extra padding for
the label to sit in.

The labeled row's cell backgrounds are cleared, since row striping and
themed body fills would otherwise cover the label. The row's own color
is then reapplied to the row itself, so a striped theme such as
[`gt_theme_almanac()`](https://andreweatherman.github.io/gtUtils/reference/gt_theme_almanac.md)
keeps an unbroken stripe pattern.

Labeled lines assume the table has no row groups, since group heading
rows shift the row positions the label CSS targets. The rule itself is
unaffected.

Apply the theme before this function. The stripe color is read from the
table's options, so a line labeled first loses the stripe under its own
row.

## See also

[`gt_spotlight()`](https://andreweatherman.github.io/gtUtils/reference/gt_spotlight.md)
for drawing attention to a row.

## Examples

``` r
if (FALSE) { # \dontrun{
library(gt)

efficient <- head(mtcars[order(-mtcars$mpg), c("mpg", "hp", "wt")], 12)
efficient$car <- rownames(efficient)

# mark where the top six ends
gt(efficient) %>%
  gt_theme_broadsheet() %>%
  gt_cutline(after = 6, label = "Top six")

# two lines, one of them unlabeled
gt(efficient) %>%
  gt_cutline(after = c(3, 6), label = c("Shortlist", NA), color = "#0054AD")
} # }
```
