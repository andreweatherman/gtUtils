#' KenPom `gt` Table Theme
#'
#' A theme for styling `gt` tables similar to The Athletic.
#'
#' @returns Returns data with an appended HTML column.
#' @param gt_object An existing gt table object of class `gt_tbl`
#' @param ... Optional additional arguments to `gt::table_options()`
#' @import gt
#' @importFrom magrittr %>%
#' @section Figures:
#' \if{html}{\figure{gt_theme_kenpom.png}{options: width=100\%}}
#'
#' @export
gt_theme_kenpom <- function(gt_object, ...) {
  stopifnot(`'gt_object' must be a 'gt_tbl', have you accidentally passed raw data?` = "gt_tbl" %in%
              class(gt_object))

  table_id <- subset(gt_object[["_options"]], parameter == "table_id")$value[[1]]
  data <- subset(gt_object[["_data"]])

  if (is.na(table_id)) {
    table_id <- gt::random_id()
    opt_position <- which("table_id" %in% gt_object[["_options"]][["parameter"]])[[1]]
    gt_object[["_options"]][["value"]][[opt_position]] <- table_id
  }

  table <- gt_object %>%
    gt::opt_table_font(
      font = list(
        gt::google_font("Helvetica Neue"),
        gt::default_fonts()
      ),
      weight = 500
    ) %>%
    gt::tab_style(
      locations = gt::cells_body(rows = seq(1, nrow(data), 2)),
      style = gt::cell_fill(color = "#F2FAFD")
    ) %>%
    gt::tab_style(
      locations = gt::cells_body(rows = seq(2, nrow(data), 2)),
      style = gt::cell_fill(color = "#e5ecf9")
    ) %>%
    gt::tab_style(
      locations = gt::cells_column_labels(
        columns = gt::everything()
      ),
      style = list(
        gt::cell_text(
          font = gt::google_font("Helvetica Neue"),
          weight = 650,
          size = px(14),
          color = "#02b"
        ),
        gt::cell_fill(color = "#c3d9ff")
      )
    ) %>%
    gt::tab_style(
      locations = gt::cells_title("title"),
      style = gt::cell_text(
        font = gt::google_font("Helvetica Neue"),
        weight = 650,
        size = px(18),
        align = "left"
      )
    ) %>%
    gt::tab_style(
      locations = gt::cells_title("subtitle"),
      style = gt::cell_text(
        font = gt::google_font("Helvetica Neue"),
        weight = 500,
        size = px(14),
        align = "left"
      )
    ) %>%
    gt::tab_style(
      locations = gt::cells_column_spanners(),
      style = gt::cell_text(
        font = gt::google_font("Helvetica Neue"),
        weight = 650,
        size = px(12)
      )
    ) %>%
    gt::tab_style(
      locations = gt::cells_row_groups(),
      style = list(
        gt::cell_text(
          font = gt::google_font("Helvetica Neue"),
          weight = 650,
          size = px(14),
          color = "#02b"
        ),
        gt::cell_fill(
          color = "#c3d9ff"
        )
      )
    ) %>%
    gt::tab_style(
      locations = gt::cells_source_notes(),
      style = gt::cell_text(
        font = gt::google_font("Helvetica Neue"),
        size = px(12)
      )
    ) %>%
    gt::tab_style(
      locations = gt::cells_row_groups(),
      style = gt::cell_text(
        weight = "bold",
        font = gt::google_font("Helvetica Neue"),
        size = px(14)
      )
    ) %>%
    # footnote
    gt::tab_style(
      locations = gt::cells_footnotes(),
      style = gt::cell_text(
        font = gt::google_font("Helvetica Neue"),
        size = px(12)
      )
    ) %>%
    gt::tab_style(
      locations = gt::cells_body(rows = 1:(nrow(data) - 1)),
      style = gt::cell_borders(sides = "bottom", color = "#000000", weight = px(1))
    ) %>%
    # uh this is kinda hacky but it works
    tab_spanner(columns = everything(), "toss_out_spanner_dev") %>%
    gt::tab_options(
      data_row.padding = 2,
      table_body.hlines.color = "transparent",
      column_labels.border.top.style = "none",
      column_labels.border.bottom.style = "none",
      row_group.border.top.style = "none",
      row_group.border.top.color = "black",
      row_group.border.bottom.width = px(1),
      row_group.border.bottom.color = "black",
      row_group.border.bottom.style = "solid",
      row_group.padding = px(1.5),
      heading.align = "center",
      heading.border.bottom.style = "none",
      table_body.border.top.style = "none",
      table_body.border.bottom.color = "white",
      table.border.bottom.style = "none",
      table.border.top.style = "none",
      source_notes.border.lr.style = "none"
    ) %>%
    gt::opt_css(c(
      paste0("#", table_id, " tbody tr:last-child {border-bottom: 2px solid #ffffff00;}"),
      paste0("#", table_id, " .gt_col_heading {padding-bottom: 2px; padding-top: 2px;}"),
      paste0("#", table_id, " .gt_subtitle {padding-top:0px !important; padding-bottom: 4px !important;}"),
      paste0("#", table_id, " .gt_heading {padding-bottom: 0px; padding-top: 6px;}"),
      paste0("#", table_id, " .gt_column_spanner {text-decoration: underline;}"),
      paste0("#", table_id, " #toss_out_spanner_dev {display: none;}")
    ))

  return(table)
}
