#' The Athletic `gt` Table Theme
#'
#' Apply The Athletic theme to a gt table
#'
#' @returns Returns a styled gt table
#' @param gt_object An existing gt table object of class `gt_tbl`
#' @param ... Optional additional arguments to `gt::table_options()`
#' @import gt
#' @importFrom magrittr %>%
#' @section Figures:
#' \if{html}{\figure{gt_theme_athletic.png}{options: width=100\%}}
#'
#' @export
gt_theme_athletic <- function(gt_object, ...) {
  stopifnot(`'gt_object' must be a 'gt_tbl', have you accidentally passed raw data?` = "gt_tbl" %in%
    class(gt_object))

  table_id <- subset(gt_object[["_options"]], parameter == "table_id")$value[[1]]

  if (is.na(table_id)) {
    table_id <- gt::random_id()
    opt_position <- which("table_id" %in% gt_object[["_options"]][["parameter"]])[[1]]
    gt_object[["_options"]][["value"]][[opt_position]] <- table_id
  }

  table <- gt_object %>%
    gt::opt_table_font(
      font = list(
        gt::google_font("Spline Sans Mono"),
        gt::default_fonts()
      ),
      weight = 500
    ) %>%
    gt::tab_style(
      locations = gt::cells_column_labels(
        columns = gt::everything()
      ),
      style = gt::cell_text(
        font = gt::google_font("Work Sans"),
        weight = 650,
        size = px(12),
        transform = "uppercase"
      )
    ) %>%
    gt::tab_style(
      locations = gt::cells_title("title"),
      style = gt::cell_text(
        font = gt::google_font("Work Sans"),
        weight = 650,
        size = px(22)
      )
    ) %>%
    gt::tab_style(
      locations = gt::cells_title("subtitle"),
      style = gt::cell_text(
        font = gt::google_font("Work Sans"),
        weight = 500,
        size = px(14)
      )
    ) %>%
    gt::tab_style(
      locations = gt::cells_row_groups(),
      style = list(
        gt::cell_text(
          weight = 650,
          size = px(12),
          color = "white"
        ),
        gt::cell_fill(
          color = "black"
        )
      )
    ) %>%
    gt::tab_style(
      style = gt::cell_borders(sides = "left", weight = px(0.5), color = "black"),
      locations = gt::cells_body(
        columns = c(-names(gt_object[["_data"]])[1])
      )
    ) %>%
    gt::tab_style(
      style = gt::cell_borders(sides = "top", color = "black", weight = px(1.5), style = "dotted"),
      locations = gt::cells_body(
        rows = gt::everything()
      )
    ) %>%
    gt::cols_align(
      align = "center",
      columns = gt::everything()
    ) %>%
    gt::tab_options(
      table.font.size = 12,
      column_labels.border.top.style = "none",
      column_labels.border.bottom.style = "solid",
      column_labels.border.bottom.width = px(1),
      column_labels.border.bottom.color = "black",
      table.border.top.style = "none",
      table.border.bottom.style = "none",
      table_body.border.top.style = "none",
      heading.border.bottom.style = "none",
      heading.align = "left",
      heading.title.font.size = px(26),
      source_notes.border.lr.style = "none",
      source_notes.font.size = 10,
      row_group.border.top.style = "none",
      row_group.border.top.color = "black",
      row_group.border.bottom.width = px(1),
      row_group.border.bottom.color = "black",
      row_group.border.bottom.style = "solid",
      row_group.padding = px(1.5),
      ...
    ) %>%
    gt::opt_css(c(
      paste0(
        "#",
        table_id,
        " tbody tr:last-child {border-bottom: 2px solid #ffffff00;}"
      ),
      paste0(
        "#",
        table_id,
        " .gt_subtitle {padding-top:0px !important; padding-bottom: 4px !important;}"
      ),
      paste0(
        "#",
        table_id,
        " .gt_sourcenote {border-bottom-color: #FFFDF5 !important;}"
      ),
      paste0(
        "#",
        table_id,
        " .gt_heading {padding-bottom: 0px; padding-top: 6px;}"
      )
    ))

  return(table)
}
