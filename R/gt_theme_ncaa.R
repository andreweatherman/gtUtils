#' NCAA `gt` Table Theme
#'
#' Apply NCAA theme to a gt table
#'
#' @returns Returns a styled gt table
#' @param gt_object An existing gt table object of class `gt_tbl`
#' @param ... Optional additional arguments to `gt::table_options()`
#' @import gt
#' @importFrom magrittr %>%
#' @section Figures:
#' \if{html}{\figure{gt_theme_ncaa.png}{options: width=100\%}}
#' @export
gt_theme_ncaa <- function(gt_object, ...) {

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
                  style = gt::cell_text(font = gt::google_font('Open Sans'), size = px(14))) %>%
    # col. headers
    gt::tab_style(locations = gt::cells_column_labels(),
                  style = list(
                    gt::cell_text(font = gt::google_font('Open Sans'), size = px(14),
                                        transform = "uppercase", color = "white", align = "left"),
                    gt::cell_fill(color = "#000000"))
                  ) %>%
    # group rows
    gt::tab_style(locations = gt::cells_row_groups(),
                  style = gt::cell_text(weight = 'bold', font = gt::google_font('Open Sans'), size = px(14))) %>%
    # footnote
    gt::tab_style(locations = gt::cells_footnotes(),
                  style = gt::cell_text(font = gt::google_font('Open Sans'), size = px(12))) %>%
    # title
    gt::tab_style(locations = gt::cells_title('title'),
                  style = gt::cell_text(weight = 'bold', font = gt::google_font('Open Sans'), size = px(18))) %>%
    # subtitle
    gt::tab_style(locations = gt::cells_title('subtitle'),
                  style = gt::cell_text(font = gt::google_font('Open Sans'), size = px(14))) %>%
    # caption
    gt::tab_style(locations = gt::cells_source_notes(),
                  style = gt::cell_text(font = gt::google_font('Open Sans'), size = px(10))) %>%
    gt::cols_align(columns = gt::everything(), align = "left") %>%
    gt::tab_style(
      locations = gt::cells_column_spanners(),
      style = gt::cell_text(
        font = gt::google_font("Open Sans"),
        weight = 650,
        size = px(13)
      )
    ) %>%
    gt::tab_style(
      locations = gt::cells_row_groups(),
      style = list(
        gt::cell_text(
          font = gt::google_font("Open Sans"),
          weight = 650,
          size = px(14),
          color = "#ffffff"
        ),
        gt::cell_fill(
          color = "#3C3A40"
        )
      )
    ) %>%
    gt::tab_style(
      locations = gt::cells_source_notes(),
      style = gt::cell_text(
        font = gt::google_font("Almarai"),
        size = px(12)
      )
    ) %>%
    gt::tab_style(
      locations = gt::cells_footnotes(),
      style = gt::cell_text(
        font = gt::google_font("Almarai"),
        size = px(12)
      )
    ) %>%
    # uh this is kinda hacky but it works
    tab_spanner(columns = everything(), "toss_out_spanner_dev") %>%
    gt::tab_options(
      data_row.padding = 2,
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
      heading.align = 'left',
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
                  paste0("#", table_id, " .gt_col_heading {padding: 5px 5px 5px 25px;}"),
                  paste0("#", table_id, " .gt_row {padding: 5px 5px 5px 25px;}"),
                  paste0("#", table_id, " .gt_subtitle {padding-top:0px !important; padding-bottom: 4px !important;}"),
                  paste0("#", table_id, " .gt_heading {padding-bottom: 0px; padding-top: 6px;}"),
                  paste0("#", table_id, " .gt_column_spanner {text-decoration: underline;}"),
                  paste0("#", table_id, " #toss_out_spanner_dev {display: none;}"))
              )

}
