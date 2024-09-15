#' Set Font Family for All Parts of a gt Table
#'
#' This function sets the font family for all customizable parts of a `gt` table
#' including title, table body, labels, label spanners, and more. This is a convenience
#' function if you want to quickly apply a font to your table.
#'
#' @param gt_table A gt table object.
#' @param font_family The font family to apply to the entire table.
#' @param from_goog_font Should the font be pulled from Google Fonts (TRUE;
#'   default) or from your local machine (FALSE)
#'
#' @return A `gt` table with the specified font family applied to all customizable parts.
#'
#' @export
gt_set_font <- function(gt_table,
                        font_family,
                        from_google_font = TRUE) {

  style_list <- ifelse(from_google_font == TRUE,
                       list(cell_text(font = gt::google_font(font_family))),
                       list(cell_text(font = font_family)))

  gt_table %>%
    gt::tab_style(
      style = style_list,
      locations = list(
        gt::cells_title(),
        gt::cells_stubhead(),
        gt::cells_column_spanners(spanners = gt::everything()),
        gt::cells_column_labels(columns = gt::everything()),
        gt::cells_row_groups(groups = gt::everything()),
        gt::cells_stub(rows = gt::everything()),
        gt::cells_body(columns = gt::everything()),
        #gt::cells_summary(columns = gt::everything(), rows = gt::everything(), groups = gt::everything()),
        #gt::cells_grand_summary(columns = gt::everything(), rows = gt::everything()),
        #gt::cells_stub_summary(rows = gt::everything(), groups = gt::everything()),
        #gt::cells_stub_grand_summary(rows = gt::everything()),
        gt::cells_footnotes(),
        gt::cells_source_notes()
      )
    )
}
