# gtUtils 1.0.0 (July 25, 2026)

The first major update to `gtUtils`: 10 new themes and 27 new functions, along
with new arguments, deprecations, and fixes across the existing functions.

## New themes

Each takes an `accent` (or `color`) argument and a `density` argument.

* `gt_theme_broadsheet()` sets a serif body with hairline rules and no fills.
  `paper` takes `"white"`, `"salmon"` for the financial-press pink, or a hex.
* `gt_theme_midnight()` is a dark theme on near-black. The default green-to-red
  ramp goes muddy there, so it ships with `pal_midnight` instead.
* `gt_theme_scoreboard()` puts condensed uppercase labels on a solid band over
  tight rows. `accent` colors the band.
* `gt_theme_swiss()` has no fills, no banding, and two rules in the whole table.
* `gt_theme_terminal()` is monospace, amber on black, with a rule under every row.
* `gt_theme_brutalist()` uses thick black rules and a large display face.
* `gt_theme_drench()` colors the entire table one saturated color and derives the
  type and rules from it.
* `gt_theme_almanac()` uses a slab body, condensed labels, and banded rows.
* `gt_theme_booktabs()` is an academic booktabs look: rules at the top and
  bottom, one under the labels, and no verticals.
* `gt_theme_tufte()` is a minimal-ink theme that strips the table back to its
  data.

### Density

`density` sets the type and padding scale in one argument.

* `"comfortable"` for articles and blog embeds
* `"compact"` for long tables
* `"social"` for tables you're saving as an image and posting

## New functions

Coloring:

* `gt_legend_continuous()` draws a color legend for a continuously colored
  column, as a gradient, steps, or blocks.
* `gt_legend_discrete()` draws a discrete color key for a categorically colored
  column.
* `gt_color_ranks()` fills cells by value for columns that already hold ranks.
* `gt_row_accent()` puts a colored bar on the leading edge of each row.
* `gt_percentile_bar()` draws a percentile as a filled track with a numbered
  circular marker at the tip.
* `gt_highlight_cells()` fills individual cells that meet a condition.
* `gt_highlight_na()` styles and relabels missing values.
* `gt_group_stripes()` shades alternate row groups.
* `gt_outliers()` flags values outside an IQR fence, a number of standard
  deviations, or bounds you set.

Annotations:

* `gt_cutline()` draws a labeled rule after a given row.
* `gt_marginalia()` styles a text column as margin notes, wrapping the text
  instead of letting it stretch the table.
* `gt_title_header()` builds a header with an optional kicker, subtitle, and
  date.
* `gt_social_tag()` puts social handles and Font Awesome icons in the source
  note.
* `gt_spotlight()` highlights rows by dimming the others.
* `gt_watermark()` puts a faint wordmark or logo behind the table body.

Formatting:

* `gt_delta()` adds a computed change column between two value columns.
* `gt_significance()` adds significance stars from a p-value column and writes
  the matching legend.
* `gt_scale_note()` divides columns by 1,000 (or any divisor) and adds a
  "Figures in thousands." note to match, e.g.
* `gt_fmt_rank()` formats numbers as ordinals.
* `gt_fmt_tally()` merges two or more count columns into one cell and
  can work out one of them as a share of the total.

Layout:

* `gt_snake()` lays a long table out in side-by-side blocks, each with its own
  column labels. Body-cell styling applied before the snake is carried through
  the reshape.
* `gt_snake_align()` reshapes a parallel frame (a precomputed mask, per-cell
  colors) into the same blocks, for grids that live outside the table's styles.
* `gt_grid()` arranges a list of separate tables in a grid, essentially facets
  for tables.
* `gt_theme_preview()` renders your data through every theme in the package.

Saving:

* `gt_social_crop()` centers a saved table on a canvas of a given aspect ratio.
* `gt_save_batch()` writes one image per group.

Labels:

* `gt_wrap_labels()` breaks long column labels across lines instead of letting
  them stretch the table.

## Extended functions

* `gt_stack_tables()` now takes a shared heading and footer (`title`,
  `subtitle`, `caption`, `source_note`, `caption_rule`) with matching `*_style`
  lists, a `gap` and `align` for the stack, and a `file` path to write straight
  to a PNG. It follows the same styling convention as `gt_grid()`.
* `gt_color_results()` gains a tie state through `tie_color`, `tie_text_color`,
  and `tie_value`, so a third result can be colored alongside wins and losses.
* `gt_tiers()` gains `tier_column` to name the tier column and `image_columns`
  to choose which columns render as images, so a text column can sit beside the
  logos. It no longer requires a column literally named `tier`.
* `gt_set_font()` gains `weight` and `style` to set the weight and italic of the
  applied font.

## Multiple columns and row targeting

`gt_color_pills()` took one column, despite the argument being named `columns`.
Anything other than a single bare name or string failed with
`Can't convert to a symbol.`, including `c(mpg, hp)` and `starts_with()` (or any other `tidyselect` verb). It now takes `tidyselect` like the rest of the package. Several columns share one
`domain`, so their colors stay comparable. Pill width is worked out per column,
so each column's pills line up with each other; and `fill_type = "rank"` ranks
each column against itself.

`rows` is new on `gt_color_pills()`, `gt_color_ranks()`, and
`gt_percentile_bar()`, taking an expression such as `mpg > 20` or a vector of row
indices, matching `gt_spotlight()` and `gt_row_accent()`. Rows left out keep
their raw value.

`gt_bold_rows()` gains `rows` on the same convention. Its `row` and
`filter_statement` arguments are deprecated. `filter_statement` took the
expression as a string and ran it through `parse()`. Both still work, with one warning per session.

## Legends pick up the scale

`gt_color_ranks()`, `gt_color_pills()`, and `gt_percentile_bar()` record the
`columns`, `palette`, `domain`, `reverse`, and `pal_type` they used.
`gt_legend_continuous()` reads whatever the caller did not supply, so this now
works:

```r
gt(df) %>%
  gt_color_ranks(net, palette = "viridis::mako", domain = c(-10, 12)) %>%
  gt_legend_continuous()
```

Previously, the palette and domain had to be repeated in full, and changing one
without the other produced a legend that disagreed with the cells it explained,
with nothing to flag it.

`gt_tiers()` records its `level = color` mapping the same way, so
`gt_legend_discrete()` can be called with no `key_info`.

Anything passed explicitly wins over the recorded value, and with several
coloring calls the most recent is the one recorded. The record is an attribute on
the table, so it survives a pipeline apart from `gt_snake()`, which rebuilds the
table from its data.

## Selection consistency

* `gt_color_results()` took `result_column` as a string only; a bare column name
  errored. It now accepts either.
* `gt_indicator_boxes()` gains `columns`, naming the columns to convert, which is
  how every other function in the package reads. `key_columns` named the columns
  to leave alone, the inverted form, and accepted only a character vector. It is
  kept, now also takes `tidyselect`, and passing both is an error.
* `gt_tiers()` accepts `levels` as a single named vector of `level = color`, the
  same shape `gt_legend_discrete()` takes, so one object can drive the tiers and
  their key.

## New arguments

* `gt_color_pills()` gains `text_color` to override the automatic contrast color
  and `na_color` to fill the pill drawn over a missing value.
* `gt_indicator_boxes()` gains `text_size` and `text_weight` for the box text.
* `gt_538_caption()` gains `rule_color`, `rule_width`, `size`, and `align`.
* `gt_column_subheaders()` gains `heading_size`, `subtitle_size`, and `font`.

## Deprecations

* `gt_centered_legend()` is deprecated in favor of `gt_legend_discrete()`. The old name still
  work with a deprecation warning.
* The first argument of `gt_set_font()` and `gt_column_subheaders()` is now
  `gt_object`, matching the rest of the package. The old `gt_table` argument
  still works with a deprecation warning.

## Bug fixes

* `gt_column_subheaders()` used `%||%` without importing it, so it failed with
  "could not find function" for everyone. Now imported from `rlang`.
* `gt_border_bars_top()` and `gt_border_bars_bottom()` called `filter()`
  unqualified, which could resolve to `stats::filter()` depending on what the
  user had attached. Now imported explicitly from `dplyr`.
* `gt_border_bars_*()` reached `google_font()` through `gt:::` when the function
  is exported, and errored outright on a theme that set no font on the title or
  source notes. Both now fall back to the inherited font.
* `gt_bold_rows()`: `highlight_color = NULL` now leaves the fill off, matching
  its documentation. It previously forced a white fill.
* `gt_tiers()`: `style` is now passed through to `gt_theme_tier()`, so
  `"light"` works. It was fixed at `"dark"` regardless. The theme, images, and
  labels are applied once rather than once per tier, and each tier now takes its
  own contrast text color.
* `gt_color_pills()` and `gt_indicator_boxes()`: `digits = NULL` no longer
  prints trailing zeros, so a whole number reads as `21` rather than `21.0000`.
* `gt_538_caption()` no longer errors when the rendered table carries no color
  to borrow for the rule; it falls back to a neutral gray. `top_caption` now
  defaults to `NULL`, so a bottom caption can be set on its own.

## Dependencies

* Added to Imports: `cli`, `fontawesome`, `magrittr`, `paletteer`, `base64enc`, and
  `tidyselect`.
* Moved `magick` from Suggests to Imports.
* Dropped `purrr` and `stringr`.

# gtUtils 0.1.0

* Initial dev release on September 13, 2024.
