# Lay a long table out in side-by-side blocks

Breaks a long table into two or more blocks set beside each other, each
with its own column labels. A sixty-row ranking becomes two columns of
thirty, which is the usual treatment in print and the difference between
an image that is unreadably tall and one that fits the frame.

## Usage

``` r
gt_snake(
  gt_object,
  n_cols = 2,
  rows_per_col = NULL,
  gap = 20,
  fill = "",
  clean_gaps = TRUE
)
```

## Arguments

- gt_object:

  A `gt` table object to modify. Body-cell styling applied before this
  call is carried through the reshape; formatting and content transforms
  are not, so they still come after (see Details).

- n_cols:

  Integer. The number of blocks to lay out. Defaults to `2`.

- rows_per_col:

  Integer. Rows in each block. Given this, `n_cols` is worked out from
  the data instead. Defaults to `NULL`.

- gap:

  Numeric. The space between blocks in pixels, added as an empty spacer
  column so it does not disturb the alignment of the columns either side
  of it. Its borders, fill, and rules are scrubbed so it stays clean
  whitespace under any theme (see Details). `0` for no gap. Defaults to
  `20`.

- fill:

  Character. What to show in the trailing cells when the rows do not
  divide evenly. Defaults to `""`, an empty cell.

- clean_gaps:

  Logical. Should the spacer columns be scrubbed of borders, fill, and
  rules so the gap reads as clean whitespace (see Details)? Set `FALSE`
  when you style the gap yourself, e.g. with a coloured
  [`gtExtras::gt_add_divider()`](https://jthomasmock.github.io/gtExtras/reference/gt_add_divider.html).
  Defaults to `TRUE`.

## Value

Returns a `gt` table laid out in blocks.

## Details

The table is rebuilt from its data. Body-cell **styles** set beforehand,
with
[`gt_highlight_cells()`](https://andreweatherman.github.io/gtUtils/reference/gt_highlight_cells.md)
or [`tab_style()`](https://gt.rstudio.com/reference/tab_style.html) on
[`cells_body()`](https://gt.rstudio.com/reference/cells_body.html), are
carried through: each is moved to its block's suffixed column and its
row within that block, so a fill on the long table lands on the same
cell once snaked. This means you can color the natural, un-snaked grid
and snake afterwards.

**Formatting and content transforms are not styles** and are not
remapped, so `fmt_*()`,
[`text_transform()`](https://gt.rstudio.com/reference/text_transform.html),
and
[`gtExtras::gt_img_rows()`](https://jthomasmock.github.io/gtExtras/reference/gt_img_rows.html)
still have to be applied after the snake, against the suffixed names
above. Themes go afterwards too. Any heading and source notes already
set are carried over.

To reshape a parallel frame (a precomputed mask, per-cell colors) to
match the same blocks, see
[`gt_snake_align()`](https://andreweatherman.github.io/gtUtils/reference/gt_snake_align.md).

[`gt::gt_split()`](https://gt.rstudio.com/reference/gt_split.html) also
divides a table, but it returns a group of separate tables to be
rendered one after another, and it errors on any table carrying
body-cell styles, which every theme in this package sets.

## Column names after snaking

Snaking is a reshape. The same columns appear once per block, so they
cannot all keep the same name: each is suffixed with its block number. A
four-column table split into two blocks becomes eight columns.

    before                     after gt_snake(n_cols = 2)
    ----------------------     ---------------------------------------
    mpg cyl hp model           mpg_1 cyl_1 hp_1 model_1    <- rows 1-10
                               mpg_2 cyl_2 hp_2 model_2    <- rows 11-20

The *labels* are untouched, so the table still reads `mpg cyl hp model`
twice over. Only the names you address in code change.

That gives three ways to target columns afterwards:

    # every block of one column, via a tidyselect helper
    fmt_number(starts_with("mpg"), decimals = 1)

    # one block only, by its full name
    tab_style(cell_text(weight = "bold"), cells_body(columns = mpg_2))

    # the whole table, exactly as usual
    cols_align(columns = everything(), align = "center")

Reach for
[`starts_with()`](https://tidyselect.r-lib.org/reference/starts_with.html)
rather than listing `mpg_1, mpg_2` by hand: it keeps working if you
change `n_cols` later. Watch out for prefixes that overlap, though. With
columns named `pts` and `pts_pg`, `starts_with("pts")` catches both
blocks of both; `matches("^pts_[0-9]+$")` catches only `pts`.

The `gap` between blocks is itself an empty column, named `.gap1`,
`.gap2`, and so on. It holds nothing and carries no label. To target the
spacers in a later call, use `starts_with(".gap")` or
`matches("^\\.gap")`;
[`everything()`](https://tidyselect.r-lib.org/reference/everything.html)
selects them too. Set `gap = 0` if you would rather it did not exist.

Because the gap is a real column, a theme or
[`gt_border_grid()`](https://andreweatherman.github.io/gtUtils/reference/gt_border_grid.md)
would otherwise draw its column borders and row rules straight through
it, leaving the "whitespace" boxed in. When `clean_gaps = TRUE` (the
default), the body of each spacer and the inner edges of the columns
either side are cleared with scoped `!important` CSS, so it wins even
against borders applied after the snake; the spacer's own column label
is cleared through `gt` directly, which holds up even when
[`tab_spanner()`](https://gt.rstudio.com/reference/tab_spanner.html)
merges header cells. The table's `id` is set (or carried over from the
input) so the CSS can be scoped to it. Set `clean_gaps = FALSE` to style
the gap yourself instead.

## See also

[`gt_stack_tables()`](https://andreweatherman.github.io/gtUtils/reference/gt_stack_tables.md)
for stacking separate tables vertically.

## Examples

``` r
if (FALSE) { # \dontrun{
library(gt)

cars <- head(mtcars[c("mpg", "cyl", "hp")], 20)
cars$model <- rownames(cars)

# two blocks of ten
gt(cars) %>%
  gt_snake(n_cols = 2) %>%
  gt_theme_broadsheet()

# or set the block length and let the count follow
gt(cars) %>%
  gt_snake(rows_per_col = 7, gap = 30) %>%
  gt_theme_swiss()

# formatting after the reshape: note the suffixed names
gt(cars) %>%
  gt_snake(n_cols = 2) %>%
  gt_theme_swiss() %>%
  fmt_number(starts_with("mpg"), decimals = 1) %>%
  cols_align(columns = everything(), align = "center")

# and picking out a single block
gt(cars) %>%
  gt_snake(n_cols = 2) %>%
  gt_theme_swiss() %>%
  tab_style(gt::cell_text(weight = "bold"),
            gt::cells_body(columns = model_1))
} # }
```
