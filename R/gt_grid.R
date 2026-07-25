#' Arrange several `gt` tables in a grid
#'
#' Lays a list of tables out in rows and columns as small multiples, one table
#' per region or per quarter. `gtExtras::gt_two_column_layout()` handles exactly
#' two tables and [gt_stack_tables()] stacks any number of them vertically. This
#' covers the rest.
#'
#' @param tables A list of `gt` table objects.
#' @param ncol Integer. The number of tables across. The number of rows follows
#'   from the length of `tables`. Defaults to `2`.
#' @param title Character. An optional heading above the whole grid. Set this
#'   instead of giving each table its own `gt::tab_header()`. Defaults to `NULL`.
#' @param subtitle Character. An optional line below `title`. Defaults to `NULL`.
#' @param caption Character. An optional note below the grid. Defaults to `NULL`.
#' @param source_note Character. An optional second line below `caption`,
#'   right-aligned by default. Set both, with `caption_rule = TRUE`, for the
#'   split caption [gt_538_caption()] gives a single table. Defaults to `NULL`.
#' @param caption_rule Logical. Should a hairline rule sit between `caption` and
#'   `source_note`? Defaults to `FALSE`.
#' @param title_style,subtitle_style,caption_style,source_note_style Lists of
#'   style options. Recognized keys are `font` (a Google font name),
#'   `size`, `color`, `weight`, `italic`, `spacing` (letter spacing), `transform`
#'   (such as `"uppercase"`), and `align`, plus `line_height`, `margin_top`,
#'   `margin_bottom`, `padding_top`, and `padding_bottom` for nudging the blocks
#'   closer together or further apart. Any key you leave out keeps its default,
#'   so `title_style = list(size = "34px")` changes only the size. Same
#'   convention as [gt_title_header()]. Lengths take a number, read as pixels, or
#'   a CSS string such as `"2rem"`; a negative margin pulls an element up tight
#'   against the one above it.
#' @param labels Character. An optional caption above each table, recycled
#'   against `tables`. Rendered in a neutral style rather than each table's own,
#'   and placed outside the table so a wide label does not stretch its panel.
#'   Defaults to `NULL`.
#' @param label_style A list of style options for the per-table captions. See
#'   `title_style` for the keys. Defaults to an empty list.
#' @param gap Numeric. The space between tables in pixels. Defaults to `24`.
#' @param align Character. How tables of differing height line up within a row.
#'   Either `"top"`, `"center"`, or `"bottom"`. Defaults to `"top"`.
#' @param file Optional. A path to write a PNG to. If `NULL`, the grid is
#'   returned for the viewer instead. Defaults to `NULL`.
#' @param bg Character. The background color, used when saving. Defaults to
#'   `"white"`.
#' @param whitespace Numeric. Padding left around the grid when saving, in
#'   pixels. Defaults to `50`.
#' @param zoom Numeric. The rendering zoom factor used when saving. Defaults to
#'   `2`.
#'
#' @details
#' Unlike most of this package, this does not return a `gt` table. Separate
#' tables have their own columns, widths, and headers, so there is no single
#' table to hand back; the grid is assembled as HTML. That makes it a last step,
#' after each table is themed and formatted, and it is why saving happens here
#' through `file` rather than through [gt_save_crop()].
#'
#' Each table also keeps its own heading. Use `title` when the tables are blocks
#' of a single thing, as with a long ranking split in two, and per-table
#' `gt::tab_header()`s when they are genuinely separate exhibits.
#'
#' Saving needs the `webshot2` package.
#'
#' @returns Displays the grid in the viewer, or writes it to `file`.
#'
#' @examples
#' \dontrun{
#' library(gt)
#'
#' by_cyl <- lapply(split(mtcars, mtcars$cyl), function(d) {
#'   gt(head(d[c("mpg", "hp", "wt")], 5)) %>%
#'     gt_theme_broadsheet() %>%
#'     tab_header(title = paste(d$cyl[[1]], "cylinders"))
#' })
#'
#' gt_grid(by_cyl, ncol = 2)
#'
#' # one heading over the whole sheet
#' gt_grid(by_cyl, ncol = 3, title = "Fuel economy by cylinder count",
#'         subtitle = "1974 Motor Trend road tests", caption = "Data: mtcars")
#'
#' # a 538-style split caption under the whole sheet
#' gt_grid(by_cyl, ncol = 3,
#'         title = "Fuel economy by cylinder count",
#'         caption = gt::md("Cars are grouped by **cylinder count**."),
#'         source_note = "Data: mtcars | Andrew Weatherman",
#'         caption_rule = TRUE)
#'
#' # tighten the subtitle up under the title, and open the gap to the grid
#' gt_grid(by_cyl, ncol = 3, title = "Fuel economy", subtitle = "Motor Trend, 1974",
#'         title_style = list(margin_bottom = -2),
#'         subtitle_style = list(margin_bottom = 24))
#'
#' # styled without writing any css
#' gt_grid(by_cyl, ncol = 3,
#'         title = "Fuel economy by cylinder count",
#'         subtitle = "1974 Motor Trend road tests",
#'         title_style = list(font = "Oswald", size = 34, transform = "uppercase"),
#'         subtitle_style = list(italic = TRUE, color = "#8A8A8A"))
#'
#' # straight to an image
#' gt_grid(by_cyl, ncol = 3, file = "cylinders.png", bg = "#FBFAF7")
#' }
#'
#' @seealso [gt_stack_tables()] for a vertical stack, and [gt_snake()] for
#'   folding a single long table into blocks.
#' @importFrom htmltools div browsable
#' @importFrom magrittr %>%
#' @export
gt_grid <- function(tables = NULL, ncol = 2, labels = NULL, label_style = list(),
                    title = NULL, subtitle = NULL, caption = NULL,
                    source_note = NULL, caption_rule = FALSE,
                    title_style = list(), subtitle_style = list(),
                    caption_style = list(), source_note_style = list(),
                    gap = 24, align = c("top", "center", "bottom"),
                    file = NULL, bg = "white", whitespace = 50, zoom = 2) {

  align <- match.arg(align)

  blank <- .style_blank()

  defaults <- list(
    title = utils::modifyList(blank, list(size = "28px", weight = 700,
                                          color = "#111111", align = "center",
                                          margin_bottom = 4)),
    subtitle = utils::modifyList(blank, list(size = "16px", weight = 400,
                                             color = "#666666", align = "center",
                                             margin_bottom = 12)),
    caption = utils::modifyList(blank, list(size = "12px", weight = 400,
                                            color = "#8A8A8A", align = "center",
                                            margin_top = 10)),
    source_note = utils::modifyList(blank, list(size = "12px", weight = 400,
                                                color = "#8A8A8A", align = "right",
                                                margin_top = 6)),
    label = utils::modifyList(blank, list(size = "12px", weight = 600,
                                          color = "#555555", align = "left",
                                          margin_bottom = 6))
  )

  s_title <- utils::modifyList(defaults$title, title_style)
  s_subtitle <- utils::modifyList(defaults$subtitle, subtitle_style)
  s_caption <- utils::modifyList(defaults$caption, caption_style)
  s_source <- utils::modifyList(defaults$source_note, source_note_style)
  s_label <- utils::modifyList(defaults$label, label_style)

  # with no subtitle the title carries the gap the subtitle would have
  if (is.null(subtitle) && !"margin_bottom" %in% names(title_style)) {
    s_title$margin_bottom <- defaults$subtitle$margin_bottom
  }
  # room between the text and the rule under it
  if (isTRUE(caption_rule) && !"padding_bottom" %in% names(caption_style)) {
    s_caption$padding_bottom <- 6
  }

  # html so links and emphasis work, and gt::md() behaves like it does in gt
  as_html <- function(x) {
    if (is.null(x)) return(NULL)
    if (inherits(x, "from_markdown")) {
      x <- commonmark::markdown_html(as.character(x))
      x <- sub("</p>\n$", "", sub("^<p>", "", x))
    }
    htmltools::HTML(as.character(x))
  }

  # composed html has no theme to inherit from, so always set a stack
  build_css <- function(s) .style_css(s, font_fallback = "system-ui, -apple-system, sans-serif")

  # gt::google_font() never runs over composed html, so fetch a named font here
  fonts <- unique(unlist(lapply(list(s_title, s_subtitle, s_caption, s_source, s_label),
                                function(s) s$font)))
  font_link <- if (length(fonts)) {
    htmltools::tags$link(
      rel = "stylesheet",
      href = paste0("https://fonts.googleapis.com/css2?",
                    paste0("family=", gsub(" ", "+", fonts), ":wght@100..900",
                           collapse = "&"),
                    "&display=swap")
    )
  }

  if (!length(tables)) {
    cli::cli_abort("{.arg tables} must be a list of {.cls gt_tbl} objects.")
  }
  bad <- !vapply(tables, function(x) inherits(x, "gt_tbl"), logical(1))
  if (any(bad)) {
    cli::cli_abort("{.arg tables} must contain only {.cls gt_tbl} objects; item{?s} {.val {which(bad)}} {?is/are} not.")
  }
  if (ncol < 1) cli::cli_abort("{.arg ncol} must be at least 1.")

  items <- c(top = "start", center = "center", bottom = "end")

  grid <- htmltools::div(
    style = paste0(
      "display: grid;",
      " grid-template-columns: repeat(", ncol, ", max-content);",
      " gap: ", gap, "px;",
      " align-items: ", items[[align]], ";",
      " justify-content: center;"
    ),
    lapply(seq_along(tables), function(i) {
      if (is.null(labels)) {
        htmltools::div(tables[[i]])
      } else {
        lab <- rep_len(labels, length(tables))[[i]]
        htmltools::div(
          htmltools::div(as_html(lab), style = build_css(s_label)),
          htmltools::div(tables[[i]])
        )
      }
    })
  )

  # title sits outside the grid, or it lands in the first cell and is sized by it
  has_header <- !is.null(title) || !is.null(subtitle)
  has_footer <- !is.null(caption) || !is.null(source_note)

  # gt_538_caption()'s split caption, in css. a grid has no footnote cells
  rule_css <- if (isTRUE(caption_rule)) {
    paste0("border-bottom:1px solid ", s_caption$color, ";")
  } else ""

  footer <- if (has_footer) {
    htmltools::div(
      if (!is.null(caption)) {
        htmltools::div(as_html(caption), style = paste0(build_css(s_caption), rule_css))
      },
      if (!is.null(source_note)) {
        htmltools::div(as_html(source_note), style = build_css(s_source))
      }
    )
  }

  if (has_header || has_footer) {
    grid <- htmltools::div(
      # inner wrapper shrinks to the grid, outer one recenters it
      style = "display: flex; justify-content: center;",
      font_link,
      htmltools::div(
        style = "display: inline-block;",
        if (has_header) {
          htmltools::div(
            if (!is.null(title)) {
              htmltools::div(as_html(title), style = build_css(s_title))
            },
            if (!is.null(subtitle)) {
              htmltools::div(as_html(subtitle), style = build_css(s_subtitle))
            }
          )
        },
        grid,
        footer
      )
    )
  }

  if (is.null(file)) {
    return(htmltools::browsable(grid))
  }

  if (!requireNamespace("webshot2", quietly = TRUE)) {
    cli::cli_abort("Saving a grid needs the {.pkg webshot2} package.")
  }

  # plain html rather than a gt table, so gtsave() is out
  page <- htmltools::div(
    style = paste0("display: inline-block; padding: 8px; background-color: ", bg, ";"),
    grid
  )
  tmp_html <- tempfile(fileext = ".html")
  tmp_png <- tempfile(fileext = ".png")
  htmltools::save_html(htmltools::browsable(page), tmp_html)

  webshot2::webshot(paste0("file://", normalizePath(tmp_html)), tmp_png,
                    zoom = zoom, selector = "body", quiet = TRUE)

  magick::image_read(tmp_png) %>%
    magick::image_trim() %>%
    magick::image_border(bg, glue::glue("{whitespace}x{whitespace}")) %>%
    magick::image_write(file)

  unlink(c(tmp_html, tmp_png))
  invisible(file)
}
