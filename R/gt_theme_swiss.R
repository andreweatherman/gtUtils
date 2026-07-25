#' International Typographic Style theme for `gt` tables
#'
#' Builds the table on whitespace. There are no fills, no banding, and exactly
#' two rules in the whole table, one under the column labels and one closing the
#' body. Everything else is done with space and alignment.
#'
#' Alignment is strict. Text sits flush left, numbers flush right, and nothing is
#' centered.
#'
#' @section Density:
#'
#' `density` sets the type and padding scale together. `"comfortable"` uses a
#' 14px body with roomy rows, `"compact"` a 12px body with tight rows, and
#' `"social"` a 17px body with generous rows and a larger title, at the scale
#' [gt_save_crop()] and [gt_social_crop()] export at.
#'
#' @param gt_object A `gt` table object to modify.
#' @param accent Character. A hex color, used once, on the rule beneath the
#'   column labels. Defaults to `"#111111"`, which reads as no color at all.
#' @param density Character. The type and padding scale. One of `"comfortable"`,
#'   `"compact"`, or `"social"`. See Density. Defaults to `"comfortable"`.
#' @param ... Additional arguments passed to `gt::tab_options`, applied last so
#'   they override anything the theme sets.
#'
#' @returns Returns a modified `gt` table with the theme applied.
#'
#' @section Figures:
#' \if{html}{\figure{gt_theme_swiss.png}{options: width=100\%}}
#'
#' @examples
#' \dontrun{
#' library(gt)
#' gt(head(mtcars)) %>% gt_theme_swiss()
#' gt(head(iris)) %>% gt_theme_swiss(accent = "#D33A2C")
#' }
#'
#' @import gt
#' @importFrom magrittr %>%
#' @export
gt_theme_swiss <- function(gt_object, accent = "#111111",
                           density = c("comfortable", "social", "compact"),
                           ...) {

  .check_gt(gt_object)
  d <- .theme_density(density)

  ink <- "#111111"
  secondary <- "#6B6B6B" # 5.28:1 on white; #7A7A7A missed 4.5

  res <- .table_id(gt_object)
  gt_object <- res$object
  table_id <- res$id

  gt_object %>%
    gt::opt_table_font(
      font = list(gt::google_font("Archivo"), gt::default_fonts())
    ) %>%
    gt::tab_style(
      locations = gt::cells_body(),
      style = gt::cell_text(color = ink, size = gt::px(d$body), weight = 400)
    ) %>%
    gt::tab_style(
      locations = gt::cells_title("title"),
      style = gt::cell_text(weight = 700, size = gt::px(d$title + 4), color = ink)
    ) %>%
    gt::tab_style(
      locations = gt::cells_title("subtitle"),
      style = gt::cell_text(weight = 400, size = gt::px(d$subtitle), color = secondary)
    ) %>%
    gt::tab_style(
      locations = gt::cells_column_labels(),
      style = gt::cell_text(
        weight = 500, size = gt::px(d$label), color = ink, transform = "uppercase"
      )
    ) %>%
    gt::tab_style(
      locations = gt::cells_column_spanners(),
      style = gt::cell_text(
        weight = 500, size = gt::px(d$label), color = ink, transform = "uppercase"
      )
    ) %>%
    gt::tab_style(
      locations = gt::cells_row_groups(),
      style = gt::cell_text(
        weight = 500, size = gt::px(d$group), color = secondary, transform = "uppercase"
      )
    ) %>%
    gt::tab_style(
      locations = gt::cells_source_notes(),
      style = gt::cell_text(size = gt::px(d$source), color = secondary)
    ) %>%
    gt::tab_style(
      locations = gt::cells_footnotes(),
      style = gt::cell_text(size = gt::px(d$source), color = secondary)
    ) %>%
    gt::tab_options(
      table.background.color = "#FFFFFF",
      table.font.size = gt::px(d$body),
      # padding is the design here
      data_row.padding = gt::px(d$pad + 5),

      table.border.top.style = "none",
      table.border.bottom.style = "none",

      heading.align = "left",
      heading.border.bottom.style = "none",
      heading.padding = gt::px(d$pad + 4),

      column_labels.border.top.style = "none",
      column_labels.border.bottom.style = "solid",
      column_labels.border.bottom.width = gt::px(1),
      column_labels.border.bottom.color = accent,
      column_labels.padding = gt::px(d$pad + 2),

      # no row rules, space does the separating
      table_body.border.top.style = "none",
      table_body.hlines.style = "none",
      table_body.border.bottom.style = "solid",
      table_body.border.bottom.width = gt::px(1),
      table_body.border.bottom.color = ink,

      row_group.border.top.style = "none",
      row_group.border.bottom.style = "none",
      row_group.padding = gt::px(d$pad + 6),

      source_notes.border.lr.style = "none",
      source_notes.border.bottom.style = "none",
      source_notes.padding = gt::px(d$pad + 4),
      footnotes.border.bottom.style = "none",
      ...
    ) %>%
    gt::opt_css(c(
      .theme_tabular_nums(table_id),
      .theme_last_row_border(table_id, "#FFFFFF"),
      paste0("#", table_id, " .gt_col_heading, #", table_id,
             " .gt_column_spanner { letter-spacing: 0.12em; }"),
      paste0("#", table_id, " .gt_group_heading { letter-spacing: 0.12em; }"),
      paste0("#", table_id, " .gt_title { letter-spacing: -0.02em; padding-bottom: ",
             ceiling(d$pad / 2), "px !important; }"),
      paste0("#", table_id, " .gt_subtitle { padding-bottom: ", d$pad + 14, "px !important; }")
    ))
}
