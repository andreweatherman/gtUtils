# Add social handles to a `gt` table

Renders a set of accounts, each an icon plus a handle, into the table's
source note, styled like the bottom line of
[`gt_538_caption()`](https://andreweatherman.github.io/gtUtils/reference/gt_538_caption.md).
Icons come from Font Awesome through the `fontawesome` package.

## Usage

``` r
gt_social_tag(
  gt_object,
  accounts,
  caption = NULL,
  stack = FALSE,
  separator = " | ",
  align = "right",
  icon_color = NULL,
  icon_height = "0.9em",
  text_size = NULL,
  text_weight = NULL,
  ...
)
```

## Arguments

- gt_object:

  A `gt` table object to modify.

- accounts:

  A named character vector mapping platform to handle, such as
  `c(x = "@you", bluesky = "you.bsky.social")`. Names are matched to
  Font Awesome brand icons, and friendly aliases are accepted, including
  `x` and `twitter`, `ig` and `instagram`, `bsky` and `bluesky`, `gh`
  and `github`, `yt` and `youtube`, `web` and `website`, and `email`. An
  unrecognized name is used as the icon name directly.

- caption:

  Optional. A caption rendered as the top footnote line. Defaults to
  `NULL`, which shows only the handle line.

- stack:

  Logical. Should the accounts be stacked vertically, one per line,
  instead of sitting in a row? Defaults to `FALSE`.

- separator:

  Character. The string placed between accounts when not stacked.
  Defaults to `" | "`.

- align:

  Character. The alignment of the handle line. One of `"right"`,
  `"center"`, or `"left"`. Defaults to `"right"`.

- icon_color:

  Optional. A hex color for the icons. If `NULL`, they inherit the
  surrounding text color. Defaults to `NULL`.

- icon_height:

  Character. The height of the icons, as a CSS size. Given in `em` it
  scales with `text_size`. Defaults to `"0.9em"`.

- text_size:

  Optional. The font size for the handles, as a CSS size. If `NULL`, it
  inherits the source-note size. Defaults to `NULL`.

- text_weight:

  Optional. The font weight for the handles. If `NULL`, it inherits the
  source-note weight. Defaults to `NULL`.

- ...:

  Additional arguments passed to
  [`gt_538_caption()`](https://andreweatherman.github.io/gtUtils/reference/gt_538_caption.md),
  used only when a `caption` is supplied.

## Value

Returns a modified `gt` table with the handles, and any caption, added.

## Details

If a `caption` is supplied it becomes the top line, a bordered footnote
in the same manner as
[`gt_538_caption()`](https://andreweatherman.github.io/gtUtils/reference/gt_538_caption.md),
with the handles beneath it. With no caption, only the handle line is
shown.

If an icon name is not present in your installed version of
`fontawesome`, the function stops with a message naming the icon and the
version, instead of rendering a broken glyph.

## See also

[`gt_538_caption()`](https://andreweatherman.github.io/gtUtils/reference/gt_538_caption.md).

## Examples

``` r
if (FALSE) { # \dontrun{
library(gt)

gt(head(mtcars)) %>%
  gt_social_tag(c(x = "@yourhandle", gh = "yourname"))

# with a caption line above, stacked and left-aligned
gt(head(mtcars)) %>%
  gt_social_tag(
    c(x = "@yourhandle", web = "example.com"),
    caption = "Data: R built-in datasets",
    stack = TRUE, align = "left"
  )
} # }
```
