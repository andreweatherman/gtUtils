# Stack several `gt` tables vertically

Places a list of tables one above another in a single block, with an
optional shared heading and footer. Each table keeps its own columns,
widths, and header.
[`gt_snake()`](https://andreweatherman.github.io/gtUtils/reference/gt_snake.md)
folds one table into blocks instead, and
[`gt_grid()`](https://andreweatherman.github.io/gtUtils/reference/gt_grid.md)
arranges tables side by side.

## Usage

``` r
gt_stack_tables(
  tables = NULL,
  gap = 16,
  align = c("center", "left", "right"),
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  source_note = NULL,
  caption_rule = FALSE,
  title_style = list(),
  subtitle_style = list(),
  caption_style = list(),
  source_note_style = list(),
  file = NULL,
  bg = "white",
  whitespace = 50,
  zoom = 2
)
```

## Arguments

- tables:

  A list of `gt` table objects to stack.

- gap:

  Numeric. The space between tables in pixels. Defaults to `16`.

- align:

  Character. How tables of differing width line up, one of `"center"`,
  `"left"`, or `"right"`. Defaults to `"center"`.

- title:

  Character. An optional heading above the stack. Defaults to `NULL`.

- subtitle:

  Character. An optional line below `title`. Defaults to `NULL`.

- caption:

  Character. An optional note below the stack. Defaults to `NULL`.

- source_note:

  Character. An optional second line below `caption`, right-aligned by
  default. Set both, with `caption_rule = TRUE`, for the split caption
  [`gt_538_caption()`](https://andreweatherman.github.io/gtUtils/reference/gt_538_caption.md)
  gives a single table. Defaults to `NULL`.

- caption_rule:

  Logical. Should a hairline sit between `caption` and `source_note`?
  Defaults to `FALSE`.

- title_style, subtitle_style, caption_style, source_note_style:

  Named lists of style options. See Styling. Each defaults to an empty
  list.

- file:

  Optional. A path to write a PNG to. If `NULL`, the stack is returned
  for the viewer instead. Defaults to `NULL`.

- bg:

  Character. The background color, used when saving. Defaults to
  `"white"`.

- whitespace:

  Numeric. Padding left around the stack when saving, in pixels.
  Defaults to `50`.

- zoom:

  Numeric. The rendering zoom factor used when saving. Defaults to `2`.

## Value

Displays the stacked tables in the viewer, or writes them to `file`.

## Details

The stack is assembled as HTML rather than a `gt` table, since each
table keeps its own columns and header. That makes it a last step, after
every table is themed and formatted, and it means the output cannot be
passed back into further `gt` calls. Saving happens here through
`webshot2` rather than through
[`gt_save_crop()`](https://andreweatherman.github.io/gtUtils/reference/gt_save_crop.md),
so a `file` write needs the `webshot2` package.

A shared heading and footer sit outside the stack in a shrink-to-fit
wrapper, so they line up with the tables rather than the page. Google
fonts named in a style list are loaded through a stylesheet link, since
the composed HTML does not run through `gt`'s own font machinery.

## Styling

`title_style`, `subtitle_style`, `caption_style`, and
`source_note_style` are named lists following the same convention as
[`gt_grid()`](https://andreweatherman.github.io/gtUtils/reference/gt_grid.md)
and
[`gt_title_header()`](https://andreweatherman.github.io/gtUtils/reference/gt_title_header.md).
Recognized keys are `font` (a Google font name), `size`, `color`,
`weight`, `italic`, `spacing` (letter spacing), `transform` (such as
`"uppercase"`), and `align`, plus `line_height`, `margin_top`,
`margin_bottom`, `padding_top`, and `padding_bottom`. Any length takes a
number, read as pixels, or a CSS string. Any key left out keeps its
default.

## See also

[`gt_grid()`](https://andreweatherman.github.io/gtUtils/reference/gt_grid.md)
for a side-by-side grid and
[`gt_snake()`](https://andreweatherman.github.io/gtUtils/reference/gt_snake.md)
for folding one long table into blocks.

## Examples

``` r
if (FALSE) { # \dontrun{
library(gt)

t1 <- gt(head(mtcars[c("mpg", "hp")]))
t2 <- gt(head(iris[c("Sepal.Length", "Species")]))

gt_stack_tables(list(t1, t2))

# one heading over the stack, saved straight to an image
gt_stack_tables(
  list(t1, t2),
  title = "Two tables",
  subtitle = "Stacked into one block",
  title_style = list(font = "Oswald", size = 30, transform = "uppercase"),
  file = "stack.png"
)
} # }
```
