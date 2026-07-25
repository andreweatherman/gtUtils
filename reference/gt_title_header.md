# Build a styled header block for a `gt` table

Wraps
[`gt::tab_header()`](https://gt.rstudio.com/reference/tab_header.html)
to produce a fuller header: an optional kicker above the title, the
title, an optional subtitle, and an optional date. Each of the four is
styled independently through its own list, so the typography can be set
without writing the
[`tab_style()`](https://gt.rstudio.com/reference/tab_style.html) calls
yourself.

## Usage

``` r
gt_title_header(
  gt_object,
  title,
  subtitle = NULL,
  kicker = NULL,
  date = NULL,
  kicker_style = list(),
  title_style = list(),
  subtitle_style = list(),
  date_style = list()
)
```

## Arguments

- gt_object:

  A `gt` table object to modify.

- title:

  Character. The title text.

- subtitle:

  Optional. The subtitle text. Defaults to `NULL`.

- kicker:

  Optional. A short line above the title. Defaults to `NULL`.

- date:

  Optional. A date shown beneath the subtitle. A `Date` is formatted as,
  for example, "July 21, 2026"; anything else is shown as given.
  Defaults to `NULL`.

- kicker_style:

  A named list styling the kicker. See Styling. Defaults to an empty
  list.

- title_style:

  A named list styling the title. Defaults to an empty list.

- subtitle_style:

  A named list styling the subtitle. Defaults to an empty list.

- date_style:

  A named list styling the date. Defaults to an empty list.

## Value

Returns a modified `gt` table with the header block applied.

## Details

Google fonts named in the style lists are loaded through `gt`'s own
machinery, so they survive
[`gtsave()`](https://gt.rstudio.com/reference/gtsave.html) as well as
the HTML output.

`gt_title_header()` calls
[`gt::tab_header()`](https://gt.rstudio.com/reference/tab_header.html),
which replaces the whole header. Paired with
[`gt_legend_continuous()`](https://andreweatherman.github.io/gtUtils/reference/gt_legend_continuous.md)
at `location = "top"`, call this one first or it will overwrite the
legend.

## Styling

Each `*_style` argument takes a named list. Recognized keys are `font`
(a Google font name), `size`, `color`, `weight`, `italic`, `spacing`
(letter spacing), `transform` (such as `"uppercase"`), and `align`, plus
`line_height`, `margin_top`, `margin_bottom`, `padding_top`, and
`padding_bottom` for nudging the blocks closer together or further
apart. Any key you leave out keeps its default. Lengths take a number,
read as pixels, or a CSS string such as `"2rem"`. This is the same
convention used by
[`gt_grid()`](https://andreweatherman.github.io/gtUtils/reference/gt_grid.md),
[`gt_legend_continuous()`](https://andreweatherman.github.io/gtUtils/reference/gt_legend_continuous.md),
and
[`gt_legend_discrete()`](https://andreweatherman.github.io/gtUtils/reference/gt_legend_discrete.md).

## See also

[`gt_538_caption()`](https://andreweatherman.github.io/gtUtils/reference/gt_538_caption.md)
for the footer equivalent.

## Examples

``` r
if (FALSE) { # \dontrun{
library(gt)

gt(head(mtcars[c("mpg", "hp", "wt")], 6)) %>%
  gt_title_header(
    title = "Fuel economy and power",
    subtitle = "A sample of the 1974 Motor Trend road tests",
    kicker = "Motor Trend",
    date = as.Date("2026-07-21"),
    kicker_style = list(color = "#0054AD", size = "0.8em",
                        transform = "uppercase", spacing = "0.1em"),
    title_style = list(font = "Libre Franklin", weight = 800, size = "26px"),
    subtitle_style = list(color = "#666666", italic = TRUE),
    date_style = list(color = "#B8232F", weight = 600)
  )
} # }
```
