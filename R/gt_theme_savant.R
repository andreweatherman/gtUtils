#' Baseball Savant `gt` Table Theme
#'
#' Apply Baseball Savant theme to a gt table
#'
#' @returns Returns a styled gt table
#' @param gt_object An existing gt table object of class `gt_tbl`
#' @param ... Optional additional arguments to `gt::table_options()`
#' @import gt
#' @importFrom magrittr %>%
#' @section Figures:
#' \if{html}{\figure{gt_theme_savant.png}{options: width=100\%}}
#' @export
gt_theme_savant <- function(gt_object, ...) {

  stopifnot(`'gt_object' must be a 'gt_tbl', have you accidentally passed raw data?` = "gt_tbl" %in%
              class(gt_object))

  table_id <- subset(gt_object[['_options']], parameter == 'table_id')$value[[1]]

  if (is.na(table_id)) {
    table_id <- gt::random_id()
    opt_position <- which("table_id" %in% gt_object[["_options"]][["parameter"]])[[1]]
    gt_object[["_options"]][["value"]][[opt_position]] <- table_id
  }

  gt_object %>%
    # cell body
    gt::tab_style(locations = gt::cells_body(),
                  style = gt::cell_text(font = gt::google_font('Roboto Condensed'), size = px(14))) %>%
    # col. headers
    gt::tab_style(locations = gt::cells_column_labels(),
                  style = gt::cell_text(weight = 'bold', font = gt::google_font('Roboto Condensed'), size = px(14))) %>%
    # group rows
    gt::tab_style(
      locations = gt::cells_row_groups(),
      style = list(
        gt::cell_text(
          font = gt::google_font("Roboto Condensed"),
          weight = 650,
          size = px(14),
          color = "#FFFDF5"
        ),
        gt::cell_fill(
          color = "#000000"
        )
      )
    ) %>%
    # footnote
    gt::tab_style(locations = gt::cells_footnotes(),
                  style = gt::cell_text(font = gt::google_font('Roboto Condensed'), size = px(12))) %>%
    # title
    gt::tab_style(locations = gt::cells_title('title'),
                  style = gt::cell_text(weight = 'bold', font = gt::google_font('Roboto Condensed'), size = px(18))) %>%
    # subtitle
    gt::tab_style(locations = gt::cells_title('subtitle'),
                  style = gt::cell_text(font = gt::google_font('Roboto Condensed'), size = px(14))) %>%
    # caption
    gt::tab_style(locations = gt::cells_source_notes(),
                  style = gt::cell_text(font = gt::google_font('Roboto Condensed'), size = px(12))) %>%
    # spanner
    gt::tab_style(
      locations = gt::cells_column_spanners(),
      style = gt::cell_text(
        font = gt::google_font("Roboto Condensed"),
        weight = 650,
        size = px(8)
      )
    ) %>%
    gt::tab_options(
      data_row.padding = 1,
      table_body.hlines.color = "transparent",
      column_labels.border.top.color = 'black',
      column_labels.border.top.width = px(1),
      column_labels.border.bottom.style = 'none',
      row_group.border.top.style = "none",
      row_group.border.top.color = "black",
      row_group.border.bottom.width = px(1),
      row_group.border.bottom.color = "black",
      row_group.border.bottom.style = 'solid',
      row_group.padding = px(1.5),
      heading.align = 'center',
      heading.border.bottom.style = "none",
      table_body.border.top.style = "none",
      table_body.border.bottom.color = "white",
      table.border.bottom.style = 'none',
      table.border.top.style = 'none',
      source_notes.border.lr.style = "none",
      ...
    ) %>%
    gt::opt_row_striping() %>%
    gt::opt_css(c(paste0("#", table_id, " tbody tr:last-child {border-bottom: 2px solid #ffffff00;}"),
                  paste0("#", table_id, " .gt_col_heading {padding-bottom: 2px; padding-top: 2px;}"),
                  paste0("#", table_id, " .gt_subtitle {padding-top:0px !important; padding-bottom: 4px !important;}"),
                  paste0("#", table_id, " .gt_heading {padding-bottom: 0px; padding-top: 6px;}"),
                  paste0("#", table_id, " .gt_column_spanner {font-size: 12px; font-weight: bold; text-decoration: underline;}")))

}
