#' Premier League `gt` Table Theme
#'
#' Apply Premier League theme to a gt table
#'
#' @returns Returns a styled gt table
#' @param gt_object An existing gt table object of class `gt_tbl`
#' @param ... Optional additional arguments to `gt::table_options()`
#' @import gt
#' @importFrom magrittr %>%
#' @section Figures:
#' \if{html}{\figure{gt_theme_pl.png}{options: width=100\%}}
#' @export
gt_theme_pl <- function(gt_object, ...) {

  table_id <- subset(gt_object[['_options']], parameter == 'table_id')$value[[1]]

  if (is.na(table_id)) {
    table_id <- gt::random_id()
    opt_position <- which("table_id" %in% gt_object[["_options"]][["parameter"]])[[1]]
    gt_object[["_options"]][["value"]][[opt_position]] <- table_id
  }

  data <- gt_object[["_data"]]

  gt_object %>%
    gt::tab_style(
      locations = gt::cells_body(
        columns = gt::everything()
      ),
      style = gt::cell_text(
        font = gt::google_font('DM Sans'),
        color = '#37003c',
        size = px(14)
      )
    ) %>%
    gt::tab_style(
      locations = gt::cells_column_labels(
        columns = gt::everything()
      ),
      style = gt::cell_text(
        font = gt::google_font('DM Sans'),
        color = '#87668a',
        weight = 650,
        size = px(13)
      )
    ) %>%
    gt::tab_style(
      locations = gt::cells_title('title'),
      style = gt::cell_text(
        font = gt::google_font('DM Sans'),
        weight = 650
      )
    ) %>%
    gt::tab_style(
      locations = gt::cells_title('subtitle'),
      style = gt::cell_text(
        font = gt::google_font('DM Sans'),
        weight = 500
      )
    ) %>%
    gt::tab_style(
      locations = gt::cells_column_spanners(),
      style = gt::cell_text(
        font = gt::google_font("DM Sans"),
        weight = 650,
        size = px(12),
        color = "#37003c"
      )
    ) %>%
    gt::tab_style(
      locations = gt::cells_row_groups(),
      style = list(
        gt::cell_text(
          font = gt::google_font("DM Sans"),
          weight = 650,
          size = px(12),
          color = "#ffffff"
        ),
        gt::cell_fill(
          color = "#C0BACA"
        )
      )
    ) %>%
    gt::tab_style(
      locations = gt::cells_footnotes(),
      style = gt::cell_text(
        font = gt::google_font("DM Sans"),
        size = px(12)
      )
    ) %>%
    gt::tab_style(
      locations = gt::cells_source_notes(),
      style = gt::cell_text(
        font = gt::google_font('DM Sans'),
        size = px(12)
      )
    ) %>%
    gt::tab_style(
      locations = gt::cells_body(rows = 1:(nrow(data) - 1)),
      style = gt::cell_borders(sides = "bottom", color = "#37003c")
    ) %>%
    gt::tab_style(
      locations = gt::cells_body(rows = 1),
      style = gt::cell_borders(sides = "top", color = "#37003c")
    ) %>%
    gt::tab_options(
      heading.align = "left",
      column_labels.border.top.style = "none",
      table.border.top.style = "none",
      table_body.border.top.style = "solid",
      table_body.border.top.width = px(1),
      table_body.border.top.color = "#37003c",
      table_body.border.bottom.color = "white",
      heading.border.bottom.style = "none",
      data_row.padding = px(2),
      row_group.padding = px(1.5),
      row_group.border.top.style = "none",
      row_group.border.bottom.width = px(1),
      row_group.border.bottom.color = "#37003c",
      row_group.border.bottom.style = 'solid',
      table.border.bottom.style = "none",
      source_notes.border.lr.style = "none",
      column_labels.border.bottom.style = "solid",
      column_labels.border.bottom.width = px(1),
      column_labels.border.bottom.color = "#37003c",
      ...
    ) %>%
    gt::opt_css(
      paste0("#", table_id,
             " .gt_col_heading
             {
              padding-bottom: 3px;
             }",
             "#", table_id,
             " .gt_heading
             {
              padding-bottom: 0px;
              padding-top: 6px
            }",
             "#", table_id,
             " .gt_subtitle
             {
              padding-top: 2px;
              padding-bottom: 6px;
            }",
             paste0("#", table_id, " .gt_column_spanner {font-size: 13px; font-weight: bold; padding-bottom: 2px;}"),
             "#", table_id,
             " .gt_sourcenote
             {
              line-height: 1.2
            }"),
      add = TRUE
    )

}
