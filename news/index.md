# Changelog

## gtUtils 1.0.0 (July 25, 2026)

The first major update to `gtUtils`: 10 new themes and 27 new functions,
along with new arguments, deprecations, and fixes across the existing
functions.

### New themes

Each takes an `accent` (or `color`) argument and a `density` argument.

- [`gt_theme_broadsheet()`](https://andreweatherman.github.io/gtUtils/reference/gt_theme_broadsheet.md)
  sets a serif body with hairline rules and no fills. `paper` takes
  `"white"`, `"salmon"` for the financial-press pink, or a hex.
- [`gt_theme_midnight()`](https://andreweatherman.github.io/gtUtils/reference/gt_theme_midnight.md)
  is a dark theme on near-black. The default green-to-red ramp goes
  muddy there, so it ships with `pal_midnight` instead.
- [`gt_theme_scoreboard()`](https://andreweatherman.github.io/gtUtils/reference/gt_theme_scoreboard.md)
  puts condensed uppercase labels on a solid band over tight rows.
  `accent` colors the band.
- [`gt_theme_swiss()`](https://andreweatherman.github.io/gtUtils/reference/gt_theme_swiss.md)
  has no fills, no banding, and two rules in the whole table.
- [`gt_theme_terminal()`](https://andreweatherman.github.io/gtUtils/reference/gt_theme_terminal.md)
  is monospace, amber on black, with a rule under every row.
- [`gt_theme_brutalist()`](https://andreweatherman.github.io/gtUtils/reference/gt_theme_brutalist.md)
  uses thick black rules and a large display face.
- [`gt_theme_drench()`](https://andreweatherman.github.io/gtUtils/reference/gt_theme_drench.md)
  colors the entire table one saturated color and derives the type and
  rules from it.
- [`gt_theme_almanac()`](https://andreweatherman.github.io/gtUtils/reference/gt_theme_almanac.md)
  uses a slab body, condensed labels, and banded rows.
- [`gt_theme_booktabs()`](https://andreweatherman.github.io/gtUtils/reference/gt_theme_booktabs.md)
  is an academic booktabs look: rules at the top and bottom, one under
  the labels, and no verticals.
- [`gt_theme_tufte()`](https://andreweatherman.github.io/gtUtils/reference/gt_theme_tufte.md)
  is a minimal-ink theme that strips the table back to its data.

#### Density

`density` sets the type and padding scale in one argument.

- `"comfortable"` for articles and blog embeds
- `"compact"` for long tables
- `"social"` for tables you’re saving as an image and posting

### New functions

Coloring:

- [`gt_legend_continuous()`](https://andreweatherman.github.io/gtUtils/reference/gt_legend_continuous.md)
  draws a color legend for a continuously colored column, as a gradient,
  steps, or blocks.
- [`gt_legend_discrete()`](https://andreweatherman.github.io/gtUtils/reference/gt_legend_discrete.md)
  draws a discrete color key for a categorically colored column.
- [`gt_color_ranks()`](https://andreweatherman.github.io/gtUtils/reference/gt_color_ranks.md)
  fills cells by value for columns that already hold ranks.
- [`gt_row_accent()`](https://andreweatherman.github.io/gtUtils/reference/gt_row_accent.md)
  puts a colored bar on the leading edge of each row.
- [`gt_percentile_bar()`](https://andreweatherman.github.io/gtUtils/reference/gt_percentile_bar.md)
  draws a percentile as a filled track with a numbered circular marker
  at the tip.
- [`gt_highlight_cells()`](https://andreweatherman.github.io/gtUtils/reference/gt_highlight_cells.md)
  fills individual cells that meet a condition.
- [`gt_highlight_na()`](https://andreweatherman.github.io/gtUtils/reference/gt_highlight_na.md)
  styles and relabels missing values.
- [`gt_group_stripes()`](https://andreweatherman.github.io/gtUtils/reference/gt_group_stripes.md)
  shades alternate row groups.
- [`gt_outliers()`](https://andreweatherman.github.io/gtUtils/reference/gt_outliers.md)
  flags values outside an IQR fence, a number of standard deviations, or
  bounds you set.

Annotations:

- [`gt_cutline()`](https://andreweatherman.github.io/gtUtils/reference/gt_cutline.md)
  draws a labeled rule after a given row.
- [`gt_marginalia()`](https://andreweatherman.github.io/gtUtils/reference/gt_marginalia.md)
  styles a text column as margin notes, wrapping the text instead of
  letting it stretch the table.
- [`gt_title_header()`](https://andreweatherman.github.io/gtUtils/reference/gt_title_header.md)
  builds a header with an optional kicker, subtitle, and date.
- [`gt_social_tag()`](https://andreweatherman.github.io/gtUtils/reference/gt_social_tag.md)
  puts social handles and Font Awesome icons in the source note.
- [`gt_spotlight()`](https://andreweatherman.github.io/gtUtils/reference/gt_spotlight.md)
  highlights rows by dimming the others.
- [`gt_watermark()`](https://andreweatherman.github.io/gtUtils/reference/gt_watermark.md)
  puts a faint wordmark or logo behind the table body.

Formatting:

- [`gt_delta()`](https://andreweatherman.github.io/gtUtils/reference/gt_delta.md)
  adds a computed change column between two value columns.
- [`gt_significance()`](https://andreweatherman.github.io/gtUtils/reference/gt_significance.md)
  adds significance stars from a p-value column and writes the matching
  legend.
- [`gt_scale_note()`](https://andreweatherman.github.io/gtUtils/reference/gt_scale_note.md)
  divides columns by 1,000 (or any divisor) and adds a “Figures in
  thousands.” note to match, e.g.
- [`gt_fmt_rank()`](https://andreweatherman.github.io/gtUtils/reference/gt_fmt_rank.md)
  formats numbers as ordinals.
- [`gt_fmt_tally()`](https://andreweatherman.github.io/gtUtils/reference/gt_fmt_tally.md)
  merges two or more count columns into one cell and can work out one of
  them as a share of the total.

Layout:

- [`gt_snake()`](https://andreweatherman.github.io/gtUtils/reference/gt_snake.md)
  lays a long table out in side-by-side blocks, each with its own column
  labels. Body-cell styling applied before the snake is carried through
  the reshape.
- [`gt_snake_align()`](https://andreweatherman.github.io/gtUtils/reference/gt_snake_align.md)
  reshapes a parallel frame (a precomputed mask, per-cell colors) into
  the same blocks, for grids that live outside the table’s styles.
- [`gt_grid()`](https://andreweatherman.github.io/gtUtils/reference/gt_grid.md)
  arranges a list of separate tables in a grid, essentially facets for
  tables.
- [`gt_theme_preview()`](https://andreweatherman.github.io/gtUtils/reference/gt_theme_preview.md)
  renders your data through every theme in the package.

Saving:

- [`gt_social_crop()`](https://andreweatherman.github.io/gtUtils/reference/gt_social_crop.md)
  centers a saved table on a canvas of a given aspect ratio.
- [`gt_save_batch()`](https://andreweatherman.github.io/gtUtils/reference/gt_save_batch.md)
  writes one image per group.

Labels:

- [`gt_wrap_labels()`](https://andreweatherman.github.io/gtUtils/reference/gt_wrap_labels.md)
  breaks long column labels across lines instead of letting them stretch
  the table.

### Extended functions

- [`gt_stack_tables()`](https://andreweatherman.github.io/gtUtils/reference/gt_stack_tables.md)
  now takes a shared heading and footer (`title`, `subtitle`, `caption`,
  `source_note`, `caption_rule`) with matching `*_style` lists, a `gap`
  and `align` for the stack, and a `file` path to write straight to a
  PNG. It follows the same styling convention as
  [`gt_grid()`](https://andreweatherman.github.io/gtUtils/reference/gt_grid.md).
- [`gt_color_results()`](https://andreweatherman.github.io/gtUtils/reference/gt_color_results.md)
  gains a tie state through `tie_color`, `tie_text_color`, and
  `tie_value`, so a third result can be colored alongside wins and
  losses.
- [`gt_tiers()`](https://andreweatherman.github.io/gtUtils/reference/gt_tiers.md)
  gains `tier_column` to name the tier column and `image_columns` to
  choose which columns render as images, so a text column can sit beside
  the logos. It no longer requires a column literally named `tier`.
- [`gt_set_font()`](https://andreweatherman.github.io/gtUtils/reference/gt_set_font.md)
  gains `weight` and `style` to set the weight and italic of the applied
  font.

### Multiple columns and row targeting

[`gt_color_pills()`](https://andreweatherman.github.io/gtUtils/reference/gt_color_pills.md)
took one column, despite the argument being named `columns`. Anything
other than a single bare name or string failed with
`Can't convert to a symbol.`, including `c(mpg, hp)` and
[`starts_with()`](https://tidyselect.r-lib.org/reference/starts_with.html)
(or any other `tidyselect` verb). It now takes `tidyselect` like the
rest of the package. Several columns share one `domain`, so their colors
stay comparable. Pill width is worked out per column, so each column’s
pills line up with each other; and `fill_type = "rank"` ranks each
column against itself.

`rows` is new on
[`gt_color_pills()`](https://andreweatherman.github.io/gtUtils/reference/gt_color_pills.md),
[`gt_color_ranks()`](https://andreweatherman.github.io/gtUtils/reference/gt_color_ranks.md),
and
[`gt_percentile_bar()`](https://andreweatherman.github.io/gtUtils/reference/gt_percentile_bar.md),
taking an expression such as `mpg > 20` or a vector of row indices,
matching
[`gt_spotlight()`](https://andreweatherman.github.io/gtUtils/reference/gt_spotlight.md)
and
[`gt_row_accent()`](https://andreweatherman.github.io/gtUtils/reference/gt_row_accent.md).
Rows left out keep their raw value.

[`gt_bold_rows()`](https://andreweatherman.github.io/gtUtils/reference/gt_bold_rows.md)
gains `rows` on the same convention. Its `row` and `filter_statement`
arguments are deprecated. `filter_statement` took the expression as a
string and ran it through
[`parse()`](https://rdrr.io/r/base/parse.html). Both still work, with
one warning per session.

### Legends pick up the scale

[`gt_color_ranks()`](https://andreweatherman.github.io/gtUtils/reference/gt_color_ranks.md),
[`gt_color_pills()`](https://andreweatherman.github.io/gtUtils/reference/gt_color_pills.md),
and
[`gt_percentile_bar()`](https://andreweatherman.github.io/gtUtils/reference/gt_percentile_bar.md)
record the `columns`, `palette`, `domain`, `reverse`, and `pal_type`
they used.
[`gt_legend_continuous()`](https://andreweatherman.github.io/gtUtils/reference/gt_legend_continuous.md)
reads whatever the caller did not supply, so this now works:

``` r

gt(df) %>%
  gt_color_ranks(net, palette = "viridis::mako", domain = c(-10, 12)) %>%
  gt_legend_continuous()
```

Previously, the palette and domain had to be repeated in full, and
changing one without the other produced a legend that disagreed with the
cells it explained, with nothing to flag it.

[`gt_tiers()`](https://andreweatherman.github.io/gtUtils/reference/gt_tiers.md)
records its `level = color` mapping the same way, so
[`gt_legend_discrete()`](https://andreweatherman.github.io/gtUtils/reference/gt_legend_discrete.md)
can be called with no `key_info`.

Anything passed explicitly wins over the recorded value, and with
several coloring calls the most recent is the one recorded. The record
is an attribute on the table, so it survives a pipeline apart from
[`gt_snake()`](https://andreweatherman.github.io/gtUtils/reference/gt_snake.md),
which rebuilds the table from its data.

### Selection consistency

- [`gt_color_results()`](https://andreweatherman.github.io/gtUtils/reference/gt_color_results.md)
  took `result_column` as a string only; a bare column name errored. It
  now accepts either.
- [`gt_indicator_boxes()`](https://andreweatherman.github.io/gtUtils/reference/gt_indicator_boxes.md)
  gains `columns`, naming the columns to convert, which is how every
  other function in the package reads. `key_columns` named the columns
  to leave alone, the inverted form, and accepted only a character
  vector. It is kept, now also takes `tidyselect`, and passing both is
  an error.
- [`gt_tiers()`](https://andreweatherman.github.io/gtUtils/reference/gt_tiers.md)
  accepts `levels` as a single named vector of `level = color`, the same
  shape
  [`gt_legend_discrete()`](https://andreweatherman.github.io/gtUtils/reference/gt_legend_discrete.md)
  takes, so one object can drive the tiers and their key.

### New arguments

- [`gt_color_pills()`](https://andreweatherman.github.io/gtUtils/reference/gt_color_pills.md)
  gains `text_color` to override the automatic contrast color and
  `na_color` to fill the pill drawn over a missing value.
- [`gt_indicator_boxes()`](https://andreweatherman.github.io/gtUtils/reference/gt_indicator_boxes.md)
  gains `text_size` and `text_weight` for the box text.
- [`gt_538_caption()`](https://andreweatherman.github.io/gtUtils/reference/gt_538_caption.md)
  gains `rule_color`, `rule_width`, `size`, and `align`.
- [`gt_column_subheaders()`](https://andreweatherman.github.io/gtUtils/reference/gt_column_subheaders.md)
  gains `heading_size`, `subtitle_size`, and `font`.

### Deprecations

- [`gt_centered_legend()`](https://andreweatherman.github.io/gtUtils/reference/gtUtils-deprecated.md)
  is deprecated in favor of
  [`gt_legend_discrete()`](https://andreweatherman.github.io/gtUtils/reference/gt_legend_discrete.md).
  The old name still work with a deprecation warning.
- The first argument of
  [`gt_set_font()`](https://andreweatherman.github.io/gtUtils/reference/gt_set_font.md)
  and
  [`gt_column_subheaders()`](https://andreweatherman.github.io/gtUtils/reference/gt_column_subheaders.md)
  is now `gt_object`, matching the rest of the package. The old
  `gt_table` argument still works with a deprecation warning.

### Bug fixes

- [`gt_column_subheaders()`](https://andreweatherman.github.io/gtUtils/reference/gt_column_subheaders.md)
  used `%||%` without importing it, so it failed with “could not find
  function” for everyone. Now imported from `rlang`.
- [`gt_border_bars_top()`](https://andreweatherman.github.io/gtUtils/reference/gt_border_bars_top.md)
  and
  [`gt_border_bars_bottom()`](https://andreweatherman.github.io/gtUtils/reference/gt_border_bars_bottom.md)
  called [`filter()`](https://rdrr.io/r/stats/filter.html) unqualified,
  which could resolve to
  [`stats::filter()`](https://rdrr.io/r/stats/filter.html) depending on
  what the user had attached. Now imported explicitly from `dplyr`.
- `gt_border_bars_*()` reached
  [`google_font()`](https://gt.rstudio.com/reference/google_font.html)
  through `gt:::` when the function is exported, and errored outright on
  a theme that set no font on the title or source notes. Both now fall
  back to the inherited font.
- [`gt_bold_rows()`](https://andreweatherman.github.io/gtUtils/reference/gt_bold_rows.md):
  `highlight_color = NULL` now leaves the fill off, matching its
  documentation. It previously forced a white fill.
- [`gt_tiers()`](https://andreweatherman.github.io/gtUtils/reference/gt_tiers.md):
  `style` is now passed through to
  [`gt_theme_tier()`](https://andreweatherman.github.io/gtUtils/reference/gt_theme_tier.md),
  so `"light"` works. It was fixed at `"dark"` regardless. The theme,
  images, and labels are applied once rather than once per tier, and
  each tier now takes its own contrast text color.
- [`gt_color_pills()`](https://andreweatherman.github.io/gtUtils/reference/gt_color_pills.md)
  and
  [`gt_indicator_boxes()`](https://andreweatherman.github.io/gtUtils/reference/gt_indicator_boxes.md):
  `digits = NULL` no longer prints trailing zeros, so a whole number
  reads as `21` rather than `21.0000`.
- [`gt_538_caption()`](https://andreweatherman.github.io/gtUtils/reference/gt_538_caption.md)
  no longer errors when the rendered table carries no color to borrow
  for the rule; it falls back to a neutral gray. `top_caption` now
  defaults to `NULL`, so a bottom caption can be set on its own.

### Dependencies

- Added to Imports: `cli`, `fontawesome`, `magrittr`, `paletteer`,
  `base64enc`, and `tidyselect`.
- Moved `magick` from Suggests to Imports.
- Dropped `purrr` and `stringr`.

## gtUtils 0.1.0

- Initial dev release on September 13, 2024.
