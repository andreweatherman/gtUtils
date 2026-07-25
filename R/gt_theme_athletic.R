#' The Athletic theme for `gt` tables
#'
#' Modeled on The Athletic's tables. A monospaced Spline Sans Mono body,
#' uppercase Work Sans labels and titles, dotted rules between rows, thin
#' vertical rules separating the columns, and a solid black row-group band with
#' knocked-out white labels. All columns are centered.
#'
#' @details
#' The row rules are a dotted top border applied to every body row, and the
#' column separators are a thin left border on every column except the first, so
#' the stub reads without a leading rule. Both are drawn with `gt::cell_borders()`
#' rather than table options. A table id is resolved (or generated) up front so
#' the `gt::opt_css()` block can pin the closing row border and heading padding
#' to this table alone.
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
#' \if{html}{\figure{gt_theme_athletic.png}{options: width=100\%}}
#'
#' @examples
#' \dontrun{
#' library(gt)
#' gt(head(mtcars)) %>% gt_theme_athletic()
#' gt(head(mtcars)) %>% gt_theme_athletic(density = "compact")
#' }
#'
#' @import gt
#' @importFrom magrittr %>%
#' @export
gt_theme_athletic <- function(gt_object,
                              density = c("comfortable", "compact", "social"),
                              ...) {
  .check_gt(gt_object)

  res <- .table_id(gt_object)
  gt_object <- res$object
  table_id <- res$id

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
        " tbody tr:last-child {border-bottom: 2px solid #FFFFFF;}"
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

  .theme_scale_output(table, density)
}
