# Styling Headers, Legends, and Captions

Five functions in the package take styling as named lists rather than as
a long tail of arguments.
[`gt_title_header()`](https://andreweatherman.github.io/gtUtils/reference/gt_title_header.md)
has `kicker_style`, `title_style`, `subtitle_style`, and `date_style`.
[`gt_legend_continuous()`](https://andreweatherman.github.io/gtUtils/reference/gt_legend_continuous.md)
has `title_style` and `labels_style`.
[`gt_legend_discrete()`](https://andreweatherman.github.io/gtUtils/reference/gt_legend_discrete.md)
has `heading_style`, `subtitle_style`, and `label_style`.
[`gt_grid()`](https://andreweatherman.github.io/gtUtils/reference/gt_grid.md)
and
[`gt_stack_tables()`](https://andreweatherman.github.io/gtUtils/reference/gt_stack_tables.md)
each have four, one per piece of their shared header and footer.

They all read the same keys, so what you learn on one carries to the
others. This vignette runs through them on `mtcars`.

``` r

cars <- head(mtcars, 6)
cars$car <- rownames(cars)
cars <- cars[c("car", "mpg", "hp", "wt")]
```

## The keys

Every style list accepts the same thirteen names. Anything you leave out
keeps its default.

| Key | Takes | Notes |
|----|----|----|
| `font` | A Google font name | Loaded through `gt`, so it survives [`gtsave()`](https://gt.rstudio.com/reference/gtsave.html) |
| `size` | Number or CSS string |  |
| `color` | Hex color |  |
| `weight` | Number or CSS keyword | `600`, `"bold"` |
| `italic` | `TRUE` / `FALSE` |  |
| `spacing` | Number or CSS string | Letter spacing |
| `transform` | CSS string | `"uppercase"`, `"lowercase"` |
| `align` | `"left"`, `"center"`, `"right"` |  |
| `line_height` | Number |  |
| `margin_top`, `margin_bottom` | Number or CSS string |  |
| `padding_top`, `padding_bottom` | Number or CSS string |  |

Any key that takes a length reads a bare number as pixels, so
`size = 30` gives `30px`. Pass a string when you want other units, like
`size = "2rem"` or `spacing = "0.08em"`.

## A header block

[`gt_title_header()`](https://andreweatherman.github.io/gtUtils/reference/gt_title_header.md)
builds a header out of four parts, each styled on its own. Only `title`
is required. Start with everything at its default.

``` r

cars %>%
  gt() %>%
  gt_theme_broadsheet() %>%
  gt_title_header(
    title = "Fuel economy and power",
    kicker = "1974 Motor Trend",
    subtitle = "Ten of the cars road tested that year",
    date = as.Date("2026-07-25")
  )
```

[TABLE]

The kicker arrives with a color and uppercase treatment already set,
since a kicker is nearly always a small colored line above the title.
The date is formatted as `"July 25, 2026"` when you pass a `Date`. Any
other value passes through as written, so `date = "Week 12"` renders as
`Week 12`.

Now tune it. Each list only has to name the keys you want to change.

``` r

cars %>%
  gt() %>%
  gt_theme_broadsheet() %>%
  gt_title_header(
    title = "Fuel economy and power",
    kicker = "1974 Motor Trend",
    subtitle = "Ten of the cars road tested that year",
    date = as.Date("2026-07-25"),
    kicker_style = list(color = "#0F5257", spacing = "0.16em", size = "0.7em"),
    title_style = list(font = "Oswald", size = 34, weight = 600,
                       spacing = "-0.01em", margin_bottom = 2),
    subtitle_style = list(size = "1.05em", color = "#5C574F", italic = TRUE),
    date_style = list(size = "0.75em", transform = "uppercase", spacing = "0.1em")
  )
```

[TABLE]

Because the lists merge over the defaults rather than replacing them,
setting `kicker_style = list(size = 20)` changes the size and leaves the
color and uppercase treatment alone.

## Fonts

Naming a `font` loads it through `gt`’s own font machinery, so it works
in the browser and in a saved image. Leave `font` unset and the element
inherits whatever the theme is using, which is usually what you want for
a title sitting on a themed table.

``` r

cars %>%
  gt() %>%
  gt_theme_terminal() %>%
  gt_title_header(
    title = "Inherited from the theme",
    subtitle = "No font named, so both pick up the theme's monospace",
    title_style = list(size = 26),
    subtitle_style = list(size = "0.95em")
  )
```

[TABLE]

## Legend text

The legends read the same keys.
[`gt_legend_continuous()`](https://andreweatherman.github.io/gtUtils/reference/gt_legend_continuous.md)
styles its title and its tick labels.

``` r

cars %>%
  gt() %>%
  gt_theme_swiss() %>%
  gt_color_ranks(mpg, domain = c(10, 35)) %>%
  gt_legend_continuous(
    title = "Miles per gallon",
    title_style = list(font = "Oswald", size = 12, transform = "uppercase",
                       spacing = "0.12em", color = "#12263F"),
    labels_style = list(size = 11, color = "#5A6B7E")
  )
```

[TABLE]

[`gt_legend_discrete()`](https://andreweatherman.github.io/gtUtils/reference/gt_legend_discrete.md)
styles a heading, a subtitle, and the swatch labels.

``` r

cars %>%
  gt() %>%
  gt_theme_broadsheet() %>%
  gt_legend_discrete(
    c("Under 20 mpg" = "#DB9070", "20 mpg and over" = "#9DC5A7"),
    heading = "Fuel economy",
    subtitle = "Grouped at the 20 mpg mark",
    location = "top",
    heading_style = list(font = "Oswald", size = 22, transform = "uppercase"),
    subtitle_style = list(size = 12, italic = TRUE, color = "#5C574F"),
    label_style = list(size = 12, weight = 500)
  )
```

[TABLE]

One exception worth knowing. With `label_placement = "inside"`, each
label is printed on its own swatch and takes black or white by whichever
contrasts better with that swatch, so a `color` in `label_style` is
ignored in that mode. Every other key still applies.

## Grids and stacks

[`gt_grid()`](https://andreweatherman.github.io/gtUtils/reference/gt_grid.md)
and
[`gt_stack_tables()`](https://andreweatherman.github.io/gtUtils/reference/gt_stack_tables.md)
wrap several tables in a shared header and footer, and style each piece
the same way.

``` r

small <- cars %>% slice_head(n = 3) %>% gt() %>% gt_theme_booktabs()

gt_grid(
  list(small, small),
  ncol = 2,
  labels = c("First three", "Repeated"),
  title = "Two blocks, one header",
  subtitle = "The header belongs to the grid, not to either table",
  caption = "Caption sits above the rule",
  source_note = "Source note sits below it",
  caption_rule = TRUE,
  title_style = list(font = "Oswald", size = 26, transform = "uppercase"),
  subtitle_style = list(size = 13, color = "#5A6B7E", margin_bottom = 18),
  caption_style = list(size = 11, color = "#12263F"),
  source_note_style = list(size = 11, color = "#5A6B7E", italic = TRUE),
  label_style = list(font = "Oswald", size = 12, transform = "uppercase")
)
```

Two blocks, one header

The header belongs to the grid, not to either table

First three

| car           | mpg  | hp  | wt    |
|---------------|------|-----|-------|
| Mazda RX4     | 21.0 | 110 | 2.620 |
| Mazda RX4 Wag | 21.0 | 110 | 2.875 |
| Datsun 710    | 22.8 | 93  | 2.320 |

Repeated

| car           | mpg  | hp  | wt    |
|---------------|------|-----|-------|
| Mazda RX4     | 21.0 | 110 | 2.620 |
| Mazda RX4 Wag | 21.0 | 110 | 2.875 |
| Datsun 710    | 22.8 | 93  | 2.320 |

Caption sits above the rule

Source note sits below it

There is one real difference here. A grid is composed HTML rather than a
`gt` table, so it has no theme to inherit from. Elements with no `font`
fall back to a system stack instead of picking up the tables’
typography. Name a `font` on the grid’s own lists when you want the
header to match the blocks under it.

## Ordering

[`gt_title_header()`](https://andreweatherman.github.io/gtUtils/reference/gt_title_header.md)
calls
[`gt::tab_header()`](https://gt.rstudio.com/reference/tab_header.html),
which replaces the whole header.
[`gt_legend_continuous()`](https://andreweatherman.github.io/gtUtils/reference/gt_legend_continuous.md)
and
[`gt_legend_discrete()`](https://andreweatherman.github.io/gtUtils/reference/gt_legend_discrete.md)
ride in that same header when `location = "top"`. Call
[`gt_title_header()`](https://andreweatherman.github.io/gtUtils/reference/gt_title_header.md)
first, or it will overwrite the legend you just added.

``` r

cars %>%
  gt() %>%
  gt_theme_almanac() %>%
  gt_color_ranks(mpg, domain = c(10, 35)) %>%
  gt_title_header(
    title = "Header first",
    title_style = list(font = "Oswald", size = 24)
  ) %>%
  gt_legend_continuous(location = "top")
```

[TABLE]

That last call takes no palette or domain.
[`gt_color_ranks()`](https://andreweatherman.github.io/gtUtils/reference/gt_color_ranks.md)
records the scale it used, and the legend reads it back, so the two
cannot disagree.
