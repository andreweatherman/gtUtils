#' KenPom theme for `gt` tables
#'
#' Modeled on KenPom's tables. Helvetica Neue type, blue zebra-striped rows, blue
#' column labels and row-group bands set over a light-blue fill, and underlined
#' spanners. The heading is centered.
#'
#' @details
#' The striping is applied by row position, a pale blue (`#F2FAFD`) on odd rows
#' and a slightly deeper blue (`#e5ecf9`) on even rows, so it follows the order
#' the data is in. A thin black bottom border separates every body row except the
#' last. To force the spanner row to render so it can be underlined, the theme
#' adds a placeholder spanner and then hides it with `display: none` in the
#' `gt::opt_css()` block. A table id is resolved (or generated) up front so that
#' CSS binds to this table alone. `density` rescales the finished table, since
#' this theme sets its sizes directly rather than deriving them from a scale.
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
#' \if{html}{\figure{gt_theme_kenpom.png}{options: width=100\%}}
#'
#' @examples
#' \dontrun{
#' library(gt)
#' gt(head(mtcars)) %>% gt_theme_kenpom()
#' gt(head(mtcars)) %>% gt_theme_kenpom(density = "compact")
#' }
#'
#' @import gt
#' @importFrom magrittr %>%
#' @export
gt_theme_kenpom <- function(gt_object,
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
      paste0("#", table_id, " tbody tr:last-child {border-bottom: 2px solid #FFFFFF;}"),
      paste0("#", table_id, " .gt_col_heading {padding-bottom: 2px; padding-top: 2px;}"),
      paste0("#", table_id, " .gt_subtitle {padding-top:0px !important; padding-bottom: 4px !important;}"),
      paste0("#", table_id, " .gt_heading {padding-bottom: 0px; padding-top: 6px;}"),
      paste0("#", table_id, " .gt_column_spanner {text-decoration: underline;}"),
      paste0("#", table_id, " #toss_out_spanner_dev {display: none;}")
    ))

  .theme_scale_output(table, density)
}
