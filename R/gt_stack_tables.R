#' Stack several `gt` tables vertically
#'
#' Places a list of tables one above another in a single block, with an optional
#' shared heading and footer. Each table keeps its own columns, widths, and
#' header. [gt_snake()] folds one table into blocks instead, and [gt_grid()]
#' arranges tables side by side.
#'
#' @section Styling:
#'
#' `title_style`, `subtitle_style`, `caption_style`, and `source_note_style` are
#' named lists following the same convention as [gt_grid()] and
#' [gt_title_header()]. Recognized keys are `font` (a Google font name), `size`,
#' `color`, `weight`, `italic`, `spacing` (letter spacing), `transform` (such as
#' `"uppercase"`), and `align`, plus `line_height`, `margin_top`, `margin_bottom`,
#' `padding_top`, and `padding_bottom`. Any length takes a number, read as pixels,
#' or a CSS string. Any key left out keeps its default.
#'
#' @param tables A list of `gt` table objects to stack.
#' @param gap Numeric. The space between tables in pixels. Defaults to `16`.
#' @param align Character. How tables of differing width line up, one of
#'   `"center"`, `"left"`, or `"right"`. Defaults to `"center"`.
#' @param title Character. An optional heading above the stack. Defaults to
#'   `NULL`.
#' @param subtitle Character. An optional line below `title`. Defaults to `NULL`.
#' @param caption Character. An optional note below the stack. Defaults to `NULL`.
#' @param source_note Character. An optional second line below `caption`,
#'   right-aligned by default. Set both, with `caption_rule = TRUE`, for the split
#'   caption [gt_538_caption()] gives a single table. Defaults to `NULL`.
#' @param caption_rule Logical. Should a hairline sit between `caption` and
#'   `source_note`? Defaults to `FALSE`.
#' @param title_style,subtitle_style,caption_style,source_note_style Named lists
#'   of style options. See Styling. Each defaults to an empty list.
#' @param file Optional. A path to write a PNG to. If `NULL`, the stack is
#'   returned for the viewer instead. Defaults to `NULL`.
#' @param bg Character. The background color, used when saving. Defaults to
#'   `"white"`.
#' @param whitespace Numeric. Padding left around the stack when saving, in
#'   pixels. Defaults to `50`.
#' @param zoom Numeric. The rendering zoom factor used when saving. Defaults to
#'   `2`.
#'
#' @details
#' The stack is assembled as HTML rather than a `gt` table, since each table keeps
#' its own columns and header. That makes it a last step, after every table is
#' themed and formatted, and it means the output cannot be passed back into
#' further `gt` calls. Saving happens here through `webshot2` rather than through
#' [gt_save_crop()], so a `file` write needs the `webshot2` package.
#'
#' A shared heading and footer sit outside the stack in a shrink-to-fit wrapper,
#' so they line up with the tables rather than the page. Google fonts named in a
#' style list are loaded through a stylesheet link, since the composed HTML does
#' not run through `gt`'s own font machinery.
#'
#' @returns Displays the stacked tables in the viewer, or writes them to `file`.
#'
#' @examples
#' \dontrun{
#' library(gt)
#'
#' t1 <- gt(head(mtcars[c("mpg", "hp")]))
#' t2 <- gt(head(iris[c("Sepal.Length", "Species")]))
#'
#' gt_stack_tables(list(t1, t2))
#'
#' # one heading over the stack, saved straight to an image
#' gt_stack_tables(
#'   list(t1, t2),
#'   title = "Two tables",
#'   subtitle = "Stacked into one block",
#'   title_style = list(font = "Oswald", size = 30, transform = "uppercase"),
#'   file = "stack.png"
#' )
#' }
#'
#' @seealso [gt_grid()] for a side-by-side grid and [gt_snake()] for folding one
#'   long table into blocks.
#' @importFrom htmltools div browsable tags save_html HTML
#' @importFrom magrittr %>%
#' @export
gt_stack_tables <- function(tables = NULL, gap = 16,
                            align = c("center", "left", "right"),
                            title = NULL, subtitle = NULL, caption = NULL,
                            source_note = NULL, caption_rule = FALSE,
                            title_style = list(), subtitle_style = list(),
                            caption_style = list(), source_note_style = list(),
                            file = NULL, bg = "white", whitespace = 50, zoom = 2) {

  align <- match.arg(align)

  if (!length(tables)) {
    cli::cli_abort("{.arg tables} must be a list of {.cls gt_tbl} objects.")
  }
  bad <- !vapply(tables, function(x) inherits(x, "gt_tbl"), logical(1))
  if (any(bad)) {
    cli::cli_abort("{.arg tables} must contain only {.cls gt_tbl} objects; item{?s} {.val {which(bad)}} {?is/are} not.")
  }

  defaults <- list(
    title = utils::modifyList(.style_blank(), list(size = "28px", weight = 700,
                                                   color = "#111111", align = "center",
                                                   margin_bottom = 4)),
    subtitle = utils::modifyList(.style_blank(), list(size = "16px", weight = 400,
                                                      color = "#666666", align = "center",
                                                      margin_bottom = 12)),
    caption = utils::modifyList(.style_blank(), list(size = "12px", weight = 400,
                                                     color = "#8A8A8A", align = "center",
                                                     margin_top = 10)),
    source_note = utils::modifyList(.style_blank(), list(size = "12px", weight = 400,
                                                         color = "#8A8A8A", align = "right",
                                                         margin_top = 6))
  )
  s_title <- utils::modifyList(defaults$title, title_style)
  s_subtitle <- utils::modifyList(defaults$subtitle, subtitle_style)
  s_caption <- utils::modifyList(defaults$caption, caption_style)
  s_source <- utils::modifyList(defaults$source_note, source_note_style)

  # with no subtitle the title carries the gap the subtitle would have held
  if (is.null(subtitle) && !"margin_bottom" %in% names(title_style)) {
    s_title$margin_bottom <- defaults$subtitle$margin_bottom
  }
  if (isTRUE(caption_rule) && !"padding_bottom" %in% names(caption_style)) {
    s_caption$padding_bottom <- 6
  }

  as_html <- function(x) {
    if (is.null(x)) return(NULL)
    if (inherits(x, "from_markdown")) {
      x <- commonmark::markdown_html(as.character(x))
      x <- sub("</p>\n$", "", sub("^<p>", "", x))
    }
    htmltools::HTML(as.character(x))
  }
  build_css <- function(s) .style_css(s, font_fallback = "system-ui, -apple-system, sans-serif")

  fonts <- .style_fonts(s_title, s_subtitle, s_caption, s_source)
  font_link <- if (length(fonts)) {
    htmltools::tags$link(
      rel = "stylesheet",
      href = paste0("https://fonts.googleapis.com/css2?",
                    paste0("family=", gsub(" ", "+", fonts), ":wght@100..900", collapse = "&"),
                    "&display=swap")
    )
  }

  items <- c(left = "flex-start", center = "center", right = "flex-end")

  stack <- htmltools::div(
    style = paste0("display: flex; flex-direction: column; gap: ", gap,
                   "px; align-items: ", items[[align]], ";"),
    lapply(tables, function(t) htmltools::div(t))
  )

  has_header <- !is.null(title) || !is.null(subtitle)
  has_footer <- !is.null(caption) || !is.null(source_note)
  rule_css <- if (isTRUE(caption_rule)) paste0("border-bottom:1px solid ", s_caption$color, ";") else ""

  footer <- if (has_footer) {
    htmltools::div(
      if (!is.null(caption)) htmltools::div(as_html(caption), style = paste0(build_css(s_caption), rule_css)),
      if (!is.null(source_note)) htmltools::div(as_html(source_note), style = build_css(s_source))
    )
  }

  composed <- if (has_header || has_footer) {
    htmltools::div(
      style = "display: flex; justify-content: center;",
      font_link,
      htmltools::div(
        style = "display: inline-block;",
        if (has_header) {
          htmltools::div(
            if (!is.null(title)) htmltools::div(as_html(title), style = build_css(s_title)),
            if (!is.null(subtitle)) htmltools::div(as_html(subtitle), style = build_css(s_subtitle))
          )
        },
        stack,
        footer
      )
    )
  } else if (length(fonts)) {
    htmltools::div(font_link, stack)
  } else {
    stack
  }

  if (is.null(file)) {
    return(htmltools::browsable(composed))
  }

  if (!requireNamespace("webshot2", quietly = TRUE)) {
    cli::cli_abort("Saving a stack needs the {.pkg webshot2} package.")
  }

  page <- htmltools::div(
    style = paste0("display: inline-block; padding: 8px; background-color: ", bg, ";"),
    composed
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
