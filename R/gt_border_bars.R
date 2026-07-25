
# File containing both variants of gt_border_bars -------------------------

#' Add horizontal bars to the top of a `gt` table
#'
#' Adds a row of horizontal color bars above a `gt` table from a vector of hex
#' colors, optionally carrying an image and a line of text. The bars are attached
#' as the table caption, so they sit above everything else.
#'
#' @param gt_object A `gt` table object to modify.
#' @param colors Character. Hex color codes, one per bar. When `img` or `text` is
#'   supplied only the first color is used.
#' @param bar_height Numeric. The height of the bars in pixels. Defaults to `10`.
#' @param bar_width Character. The width of the bar block, as a CSS width.
#'   Defaults to `"100%"`.
#' @param bar_align Character. Alignment of the block when `bar_width` is under
#'   `"100%"`. One of `"left"`, `"center"`, `"right"`. Defaults to `"center"`.
#' @param img Optional. A URL for an image to render in the bar. Defaults to
#'   `NULL`.
#' @param img_width Numeric. The image width in pixels. Defaults to `30`.
#' @param img_height Numeric. The image height in pixels. Defaults to `30`.
#' @param img_padding Numeric. Padding around the image in pixels, so it does not
#'   touch the edge. Defaults to `10`.
#' @param img_align Character. The side the image padding is applied to, one of
#'   `"left"`, `"center"`, `"right"`. Defaults to `"right"`.
#' @param text Optional. Text to display in the bar. Defaults to `NULL`.
#' @param text_weight Character. The font weight of the text. Defaults to
#'   `"bold"`.
#' @param text_color Character. The text color. Defaults to `"#FFFFFF"`.
#' @param text_size Numeric. The font size in pixels. Defaults to `18`.
#' @param text_align Character. The side the text padding is applied to, one of
#'   `"left"`, `"center"`, `"right"`. Defaults to `"left"`.
#' @param text_padding Numeric. Padding around the text in pixels. Defaults to
#'   `10`.
#'
#' @details
#' The bars are added with `gt::tab_caption()`, and a table id is resolved or
#' generated so scoped CSS can zero the caption padding. With neither `img` nor
#' `text`, each entry in `colors` becomes its own full-width bar stacked in a
#' block. When `img` or `text` is supplied, a single bar is drawn in the first
#' color as a flex row with the text at one end and the image at the other. The
#' text font is read from the table's title styling and imported as a Google
#' Font, falling back to the inherited font when none is set, so it renders in an
#' exported table.
#'
#' @returns Returns a modified `gt` table with a row of bars above it.
#'
#' @examples
#' \dontrun{
#' library(gt)
#'
#' gt(head(mtcars)) %>%
#'   gt_border_bars_top(c("#1B7837", "#FFFFFF", "#B2182B"))
#'
#' # a single bar carrying a title
#' gt(head(iris)) %>%
#'   gt_border_bars_top("#22223B", text = "Iris measurements", bar_height = 34)
#' }
#'
#' @import htmltools
#' @import gt
#'
#' @importFrom dplyr filter
#' @export
gt_border_bars_top <- function(gt_object,
                               colors,
                               bar_height = 10,
                               bar_width = "100%",
                               bar_align = "center",
                               img = NULL,
                               img_width = 30,
                               img_height = 30,
                               img_padding = 10,
                               img_align = "right",
                               text = NULL,
                               text_weight = "bold",
                               text_color = "#FFFFFF",
                               text_size = 18,
                               text_align = "left",
                               text_padding = 10) {

  .check_gt(gt_object)

  res <- .table_id(gt_object)
  gt_object <- res$object
  table_id <- res$id

  # try to get font from title class but just inherit base if no font is specified
  font_info <- tryCatch({
    filter(gt:::dt_styles_get(gt_object), locname == "title")$styles[[1]]$cell_text$font
  }, error = function(e) {
    "inherit"
  })

  google_font <- tryCatch({
    gt::google_font(font_info)$import_stmt
  }, error = function(e) {
    NULL # no font rec. above
  })

  align_style <- switch(bar_align,
                        "left" = "margin-left: 0; margin-right: auto;",
                        "center" = "margin-left: auto; margin-right: auto;",
                        "right" = "margin-left: auto; margin-right: 0;",
                        "margin-left: auto; margin-right: auto;"
  )

  bars <- if (is.null(text) && is.null(img)) {
    paste0(
      '<div style="background-color: transparent; width: ', bar_width, '; ', align_style, '">',
      paste0(
        sapply(colors, function(color) {
          paste0('<div style="height: ', bar_height, 'px; background-color: ', color, ';"></div>')
        }),
        collapse = ""
      ),
      '</div>'
    )
  } else {
    paste0(
      if (!is.null(google_font)) paste0('<style>', google_font, '</style>') else "",
      '<div style="display: flex; justify-content: space-between; align-items: center; height: ', bar_height, 'px; background-color: ', colors[1], '; width: ', bar_width, '; ', align_style, '">',
      if (!is.null(text)) {
        paste0('<span style="font-weight:', text_weight,
               '; color:', text_color, '; font-size:', text_size, 'px; padding-', text_align, ': ', text_padding, 'px; font-family: ', font_info, ';">', text, '</span>')
      } else {
        paste0('<span></span>')
      },
      if (!is.null(img)) {
        paste0('<img src="', img, '" width="', img_width, 'px" height="', img_height,
               'px" style="padding-', img_align, ':', img_padding, 'px;" />')
      } else {
        ""
      },
      '</div>'
    )
  }

  gt_object %>%
    gt::tab_caption(html(bars)) %>%
    gt::opt_css(paste0("#", table_id, " .gt_caption {padding-top: 0px !important; padding-bottom: 0px !important;}"), add = TRUE)
}



#' Add horizontal bars to the bottom of a `gt` table
#'
#' Adds a row of horizontal color bars below a `gt` table from a vector of hex
#' colors, optionally carrying an image and a line of text. The bars are attached
#' as a source note, so they sit below everything else. It is the bottom-edge
#' counterpart to [gt_border_bars_top()].
#'
#' @param gt_object A `gt` table object to modify.
#' @param colors Character. Hex color codes, one per bar. When `img` or `text` is
#'   supplied only the first color is used.
#' @param bar_height Numeric. The height of the bars in pixels. Defaults to `10`.
#' @param bar_width Character. The width of the bar block, as a CSS width.
#'   Defaults to `"100%"`.
#' @param bar_align Character. Alignment of the block when `bar_width` is under
#'   `"100%"`. One of `"left"`, `"center"`, `"right"`. Defaults to `"center"`.
#' @param img Optional. A URL for an image to render in the bar. Defaults to
#'   `NULL`.
#' @param img_width Numeric. The image width in pixels. Defaults to `30`.
#' @param img_height Numeric. The image height in pixels. Defaults to `30`.
#' @param img_padding Numeric. Padding around the image in pixels, so it does not
#'   touch the edge. Defaults to `10`.
#' @param img_align Character. The side the image padding is applied to, one of
#'   `"left"`, `"center"`, `"right"`. Defaults to `"right"`.
#' @param text Optional. Text to display in the bar. Defaults to `NULL`.
#' @param text_weight Character. The font weight of the text. Defaults to
#'   `"bold"`.
#' @param text_color Character. The text color. Defaults to `"#FFFFFF"`.
#' @param text_size Numeric. The font size in pixels. Defaults to `18`.
#' @param text_align Character. The side the text padding is applied to, one of
#'   `"left"`, `"center"`, `"right"`. Defaults to `"left"`.
#' @param text_padding Numeric. Padding around the text in pixels. Defaults to
#'   `10`.
#'
#' @details
#' The bars are added with `gt::tab_source_note()`, and a table id is resolved or
#' generated so scoped CSS can zero the source-note padding. With neither `img`
#' nor `text`, each entry in `colors` becomes its own full-width bar stacked in a
#' block. When `img` or `text` is supplied, a single bar is drawn in the first
#' color as a flex row with the text at one end and the image at the other. The
#' text font is read from the table's source-note styling and imported as a
#' Google Font, falling back to the inherited font when none is set, so it renders
#' in an exported table.
#'
#' @returns Returns a modified `gt` table with a row of bars below it.
#'
#' @examples
#' \dontrun{
#' library(gt)
#'
#' gt(head(mtcars)) %>%
#'   gt_border_bars_bottom(c("#1B7837", "#FFFFFF", "#B2182B"))
#'
#' # a single bar carrying a credit line
#' gt(head(iris)) %>%
#'   gt_border_bars_bottom("#22223B", text = "Source: iris", bar_height = 28)
#' }
#'
#' @import htmltools
#' @import gt
#'
#' @export
gt_border_bars_bottom <- function(gt_object,
                                  colors,
                                  bar_height = 10,
                                  bar_width = "100%",
                                  bar_align = "center",
                                  img = NULL,
                                  img_width = 30,
                                  img_height = 30,
                                  img_padding = 10,
                                  img_align = "right",
                                  text = NULL,
                                  text_weight = "bold",
                                  text_color = "#FFFFFF",
                                  text_size = 18,
                                  text_align = "left",
                                  text_padding = 10) {

  .check_gt(gt_object)

  res <- .table_id(gt_object)
  gt_object <- res$object
  table_id <- res$id

  font_info <- tryCatch({
    filter(gt:::dt_styles_get(gt_object), locname == "source_notes")$styles[[1]]$cell_text$font
  }, error = function(e) {
    "inherit"
  })

  google_font <- tryCatch({
    gt::google_font(font_info)$import_stmt
  }, error = function(e) {
    NULL
  })

  align_style <- switch(bar_align,
                        "left" = "margin-left: 0; margin-right: auto;",
                        "center" = "margin-left: auto; margin-right: auto;",
                        "right" = "margin-left: auto; margin-right: 0;",
                        "margin-left: auto; margin-right: auto;"
  )

  if (is.null(text) && is.null(img)) {
    bars <- paste0(
      '<div style="background-color: transparent; width: ', bar_width, '; ', align_style, '">',
      paste0(
        sapply(colors, function(color) {
          paste0('<div style="height: ', bar_height, 'px; background-color: ', color, ';"></div>')
        }),
        collapse = ""
      ),
      '</div>'
    )
  } else {
    bars <- paste0(
      if (!is.null(google_font)) paste0('<style>', google_font, '</style>') else "",
      '<div style="display: flex; justify-content: space-between; align-items: center; height: ', bar_height, 'px; background-color: ', colors[1], '; width: ', bar_width, '; ', align_style, '">',
      if (!is.null(text)) {
        paste0('<span style="font-weight:', text_weight,
               '; color:', text_color, '; font-size:', text_size, 'px; padding-', text_align, ': ', text_padding, 'px; font-family: ', font_info, ';">', text, '</span>')
      } else {
        paste0('<span></span>')
      },
      if (!is.null(img)) {
        paste0('<img src="', img, '" width="', img_width, 'px" height="', img_height,
               'px" style="padding-', img_align, ':', img_padding, 'px;" />')
      } else {
        ""
      },
      '</div>'
    )
  }

  gt_object %>%
    gt::tab_source_note(html(bars)) %>%
    gt::opt_css(paste0("#", table_id, " .gt_sourcenote {padding-right: 0px !important; padding-left: 0px !important; padding-bottom: 0px;}"), add = TRUE)
}
