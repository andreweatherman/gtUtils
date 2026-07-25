#' gtUtils theme for `gt` tables
#'
#' The package's house theme. A warm cream background, an Almarai body,
#' Signika Negative titles, column labels and row groups, hairline separators
#' between rows, and gray row-group bands carrying cream labels. All columns are
#' centered.
#'
#' @details
#' The row separators are a single bottom border in gray (`#8A817C`) applied to
#' every body row except the last, so the table closes on the background rather
#' than a rule. The background is a warm cream (`#FFFDF5`), and horizontal rules
#' are otherwise set transparent. A table id is resolved (or generated) up front
#' so the `gt::opt_css()` block can pin the closing row border and heading
#' padding to this table alone.
#' `density` rescales the finished table, since this theme sets its sizes
#' directly rather than deriving them from a scale.
#'
#' @section Density:
#'
#' `density` scales the theme's type and row padding together. `"comfortable"`
#' leaves every size as the theme sets it, `"compact"` scales both down, and
#' `"social"` scales both up, to the scale [gt_save_crop()] and
#' [gt_social_crop()] export at.
#'
#' @param gt_object A `gt` table object to modify.
#' @param density Character. The type and padding scale. One of `"comfortable"`,
#'   `"compact"`, or `"social"`. See Density. Defaults to `"comfortable"`.
#' @param ... Additional arguments passed to `gt::tab_options`, applied last so
#'   they override anything the theme sets.
#'
#' @returns Returns a modified `gt` table with the theme applied.
#'
#' @section Figures:
#' \if{html}{\figure{gt_theme_gtutils.png}{options: width=100\%}}
#'
#' @examples
#' \dontrun{
#' library(gt)
#' gt(head(mtcars)) %>% gt_theme_gtutils()
#' gt(head(mtcars)) %>% gt_theme_gtutils(density = "compact")
#' }
#'
#' @import gt
#' @importFrom magrittr %>%
#' @export
gt_theme_gtutils <- function(gt_object,
                             density = c("comfortable", "compact", "social"),
                             ...) {
  .check_gt(gt_object)

  res <- .table_id(gt_object)
  gt_object <- res$object
  table_id <- res$id
  data <- gt_object[["_data"]]

  table <- gt_object %>%
    gt::opt_table_font(
      font = list(
        gt::google_font("Almarai"),
        gt::default_fonts()
      ),
      weight = 500
    ) %>%
    gt::tab_style(
      locations = gt::cells_title("title"),
      style = gt::cell_text(
        font = gt::google_font("Signika Negative"),
        weight = 650
      )
    ) %>%
    gt::tab_style(
      locations = gt::cells_title("subtitle"),
      style = gt::cell_text(
        font = gt::google_font("Signika Negative"),
        weight = 500
      )
    ) %>%
    gt::tab_style(
      locations = gt::cells_column_labels(
        columns = gt::everything()
      ),
      style = gt::cell_text(
        font = gt::google_font("Signika Negative"),
        weight = 650,
        size = px(14)
      )
    ) %>%
    gt::tab_style(
      locations = gt::cells_column_spanners(),
      style = gt::cell_text(
        font = gt::google_font("Signika Negative"),
        weight = 650,
        size = px(13)
      )
    ) %>%
    gt::tab_style(
      locations = gt::cells_row_groups(),
      style = list(
        gt::cell_text(
          font = gt::google_font("Signika Negative"),
          weight = 650,
          size = px(14),
          color = "#FFFDF5"
        ),
        gt::cell_fill(
          color = "#8A817C"
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
    gt::tab_style(
      locations = gt::cells_body(rows = 1:(nrow(data) - 1)),
      style = gt::cell_borders(sides = "bottom", color = "#8A817C")
    ) %>%
    gt::cols_align(
      align = "center",
      columns = gt::everything()
    ) %>%
    gt::tab_options(
      data_row.padding = 1,
      table_body.hlines.color = "transparent",
      # column_labels.border.top.style = 'solid',
      # column_labels.border.top.color = '#ffffff',
      # column_labels.border.top.width = px(0.5),
      column_labels.border.top.style = "none",
      column_labels.border.bottom.style = "solid",
      column_labels.border.bottom.width = px(1),
      column_labels.border.bottom.color = "black",
      row_group.border.top.style = "none",
      row_group.border.top.color = "black",
      row_group.border.bottom.width = px(1),
      row_group.border.bottom.color = "black",
      row_group.border.bottom.style = "solid",
      row_group.padding = px(1.5),
      heading.align = "left",
      heading.border.bottom.style = "none",
      table_body.border.top.style = "none",
      table.border.bottom.style = "none",
      table.border.top.style = "none",
      source_notes.border.lr.style = "none",
      table.background.color = "#FFFDF5",
      table.border.top.color = "#FFFDF5",
      table.border.right.color = "#FFFDF5",
      table.border.bottom.color = "#FFFDF5",
      table.border.left.color = "#FFFDF5",
      ...
    ) %>%
    gt::opt_css(c(
      paste0(
        "#",
        table_id,
        " tbody tr:last-child {border-bottom: 2px solid #FFFDF5;}"
      ),
      paste0(
        "#",
        table_id,
        " .gt_col_heading {padding-bottom: 2px; padding-top: 2px;}"
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
      ),
      paste0(
        "#",
        table_id,
        " .gt_column_spanner {padding-bottom: 2px;}"
      )
    ))

  .theme_scale_output(table, density)

}



