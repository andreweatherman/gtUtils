
# File containing both variants of gt_border_bars -------------------------

#' Add horizontal bars with optional image, text, and Google Font to the top of a `gt` table
#'
#' This function adds a row of horizontal bars at the top of a `gt` table,
#' using a character vector of hex color codes. The bars are added as a caption with
#' customizable height, width, alignment, image size, text formatting, and image/text alignment.
#' If image or text is provided, only one color will be used, and that bar will
#' contain the image and/or text. The function automatically inherits the font from the `gt`
#' object and uses the corresponding Google Font for any text rendered in the bar.
#'
#' @param gt_object A `gt` table object.
#' @param colors A character vector of hex color codes for the bars.
#' @param bar_height The height of the bars in pixels. Default is 10px.
#' @param bar_width The width of the bars in percentage. Default is 100%.
#' @param bar_align Alignment of the bars when `bar_width` is not 100%. Options are 'left', 'center', or 'right'. Default is 'center'.
#' @param img A URL for an image to render in the bar (optional).
#' @param img_width The width of the image in pixels. Default is 30px.
#' @param img_height The height of the image in pixels. Default is 30px.
#' @param img_padding Padding around the image to ensure it's not touching the edges. Default is 10px.
#' @param img_align Alignment of the image within the bar ('left', 'center', 'right'). Default is 'right'.
#' @param text Optional text to display in the bar.
#' @param text_weight Font weight of the text. Default is 'bold' for emphasis.
#' @param text_color Color of the text. Default is '#FFFFFF' (white).
#' @param text_size Font size for the text. Default is 18px.
#' @param text_align Alignment of the text within the bar ('left', 'center', 'right'). Default is 'left'.
#' @param text_padding Padding around the text to ensure it's not touching the edges. Default is 10px.
#'
#' @return A `gt` table with a row of horizontal bars at the top.
#'
#' @import htmltools
#' @import gt
#'
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

  ## need table id later
  table_id <- subset(gt_object[['_options']], parameter == 'table_id')$value[[1]]

  if (is.na(table_id)) {
    table_id <- gt::random_id()
    opt_position <- which("table_id" %in% gt_object[["_options"]][["parameter"]])[[1]]
    gt_object[["_options"]][["value"]][[opt_position]] <- table_id
  }

  # grab font used in source notes
  font <- filter(gt:::dt_styles_get(gt_object), locname == "title")$styles[[1]]$cell_text$font
  google_font <- gt:::google_font(font)$import_stmt

  align_style <- switch(bar_align,
                        "left" = "margin-left: 0; margin-right: auto;",
                        "center" = "margin-left: auto; margin-right: auto;",
                        "right" = "margin-left: auto; margin-right: 0;",
                        "margin-left: auto; margin-right: auto;"
  )

  ## a bunch of switching and if/elses for css; probably a cleaner way
  ## to do this but this seems to work *shrug*

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
      '<style>', google_font, '</style>',
      '<div style="display: flex; justify-content: space-between; align-items: center; height: ', bar_height, 'px; background-color: ', colors[1], '; width: ', bar_width, '; ', align_style, '">',
      if (!is.null(text)) {
        paste0('<span style="font-weight:', text_weight,
               '; color:', text_color, '; font-size:', text_size, 'px; padding-', text_align, ': ', text_padding, 'px; font-family: ', font, ';">', text, '</span>')
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



#' Add horizontal bars with optional image, text, and Google Font to the bottom of a `gt` table
#'
#' This function adds a row of horizontal bars at the bottom of a `gt` table,
#' using a character vector of hex color codes. The bars are added as a source note with
#' customizable height, width, alignment, image size, text formatting, and image/text alignment.
#' If image or text is provided, only one color will be used, and that bar will
#' contain the image and/or text. The function automatically inherits the font from the `gt`
#' object and uses the corresponding Google Font for any text rendered in the bar.
#'
#' @param gt_object A `gt` table object.
#' @param colors A character vector of hex color codes for the bars.
#' @param bar_height The height of the bars in pixels. Default is 10px.
#' @param bar_width The width of the bars in percentage. Default is 100%.
#' @param bar_align Alignment of the bars when `bar_width` is not 100%. Options are 'left', 'center', or 'right'. Default is 'center'.
#' @param img A URL for an image to render in the bar (optional).
#' @param img_width The width of the image in pixels. Default is 30px.
#' @param img_height The height of the image in pixels. Default is 30px.
#' @param img_padding Padding around the image to ensure it's not touching the edges. Default is 10px.
#' @param img_align Alignment of the image within the bar ('left', 'center', 'right'). Default is 'right'.
#' @param text Optional text to display in the bar.
#' @param text_weight Font weight of the text. Default is 'bold' for emphasis.
#' @param text_color Color of the text. Default is '#FFFFFF' (white).
#' @param text_size Font size for the text. Default is 18px.
#' @param text_align Alignment of the text within the bar ('left', 'center', 'right'). Default is 'left'.
#' @param text_padding Padding around the text to ensure it's not touching the edges. Default is 10px.
#'
#' @return A `gt` table with a row of horizontal bars at the bottom.
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

  ## need table id later
  table_id <- subset(gt_object[['_options']], parameter == 'table_id')$value[[1]]

  if (is.na(table_id)) {
    table_id <- gt::random_id()
    opt_position <- which("table_id" %in% gt_object[["_options"]][["parameter"]])[[1]]
    gt_object[["_options"]][["value"]][[opt_position]] <- table_id
  }

  # grab font used in source notes
  font <- filter(gt:::dt_styles_get(gt_object), locname == "source_notes")$styles[[1]]$cell_text$font
  google_font <- gt:::google_font(font)$import_stmt

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
      '<style>', google_font, '</style>',
      '<div style="display: flex; justify-content: space-between; align-items: center; height: ', bar_height, 'px; background-color: ', colors[1], '; width: ', bar_width, '; ', align_style, '">',
      if (!is.null(text)) {
        paste0('<span style="font-weight:', text_weight,
               '; color:', text_color, '; font-size:', text_size, 'px; padding-', text_align, ': ', text_padding, 'px; font-family: ', font, ';">', text, '</span>')
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
