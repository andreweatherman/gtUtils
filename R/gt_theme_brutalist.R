#' Heavy-rule display theme for `gt` tables
#'
#' Heavy black rules, a solid label bar, and a display face at a large size.
#'
#' The accent color is used in exactly one place, on the row-group labels.
#'
#' @section Density:
#'
#' `density` sets the type and padding scale together. `"comfortable"` uses a
#' 14px body with roomy rows, `"compact"` a 12px body with tight rows, and
#' `"social"` a 17px body with generous rows and a larger title, at the scale
#' [gt_save_crop()] and [gt_social_crop()] export at.
#'
#' @param gt_object A `gt` table object to modify.
#' @param accent Character. The theme's single accent color, used on the
#'   row-group labels. Defaults to `"#FF3B00"`.
#' @param density Character. The type and padding scale. One of `"comfortable"`,
#'   `"compact"`, or `"social"`. See Density. Defaults to `"comfortable"`.
#' @param ... Additional arguments passed to `gt::tab_options`, applied last so
#'   they override anything the theme sets.
#'
#' @returns Returns a modified `gt` table with the theme applied.
#'
#' @section Figures:
#' \if{html}{\figure{gt_theme_brutalist.png}{options: width=100\%}}
#'
#' @examples
#' \dontrun{
#' library(gt)
#' gt(head(mtcars)) %>% gt_theme_brutalist()
#' gt(head(mtcars)) %>% gt_theme_brutalist(accent = "#0033FF", density = "social")
#' }
#'
#' @import gt
#' @importFrom magrittr %>%
#' @export
gt_theme_brutalist <- function(gt_object, accent = "#FF3B00",
                               density = c("comfortable", "compact", "social"),
                               ...) {

  .check_gt(gt_object)
  d <- .theme_density(density)

  ink <- "#000000"

  res <- .table_id(gt_object)
  gt_object <- res$object
  table_id <- res$id

  gt_object %>%
    gt::opt_table_font(
      font = list(gt::google_font("Archivo"), gt::default_fonts()),
      weight = 500
    ) %>%
    gt::tab_style(
      locations = gt::cells_body(),
      style = gt::cell_text(color = ink, size = gt::px(d$body), weight = 500)
    ) %>%
    gt::tab_style(
      locations = gt::cells_title("title"),
      style = gt::cell_text(
        font = gt::google_font("Archivo Black"),
        size = gt::px(d$title + 6), color = ink, transform = "uppercase"
      )
    ) %>%
    gt::tab_style(
      locations = gt::cells_title("subtitle"),
      style = gt::cell_text(weight = 600, size = gt::px(d$subtitle), color = ink)
    ) %>%
    # label row is a solid black bar, knocked out white
    gt::tab_style(
      locations = gt::cells_column_labels(),
      style = gt::cell_text(
        weight = 700, size = gt::px(d$label + 1), color = "#FFFFFF",
        transform = "uppercase"
      )
    ) %>%
    gt::tab_style(
      locations = gt::cells_column_spanners(),
      style = gt::cell_text(
        weight = 700, size = gt::px(d$label + 1), color = ink, transform = "uppercase"
      )
    ) %>%
    gt::tab_style(
      locations = gt::cells_row_groups(),
      style = gt::cell_text(
        weight = 700, size = gt::px(d$group + 1), color = accent, transform = "uppercase"
      )
    ) %>%
    gt::tab_style(
      locations = gt::cells_source_notes(),
      style = gt::cell_text(size = gt::px(d$source), color = ink, weight = 500)
    ) %>%
    gt::tab_style(
      locations = gt::cells_footnotes(),
      style = gt::cell_text(size = gt::px(d$source), color = ink, weight = 500)
    ) %>%
    gt::tab_options(
      table.background.color = "#FFFFFF",
      column_labels.background.color = ink,
      table.font.size = gt::px(d$body),
      data_row.padding = gt::px(d$pad),

      table.border.top.style = "solid",
      table.border.top.width = gt::px(3),
      table.border.top.color = ink,
      table.border.bottom.style = "solid",
      table.border.bottom.width = gt::px(3),
      table.border.bottom.color = ink,
      table.border.left.style = "solid",
      table.border.left.width = gt::px(3),
      table.border.left.color = ink,
      table.border.right.style = "solid",
      table.border.right.width = gt::px(3),
      table.border.right.color = ink,

      heading.align = "left",
      heading.border.bottom.style = "solid",
      heading.border.bottom.width = gt::px(3),
      heading.border.bottom.color = ink,
      heading.padding = gt::px(d$pad + 2),

      column_labels.border.top.style = "none",
      column_labels.border.bottom.style = "none",
      column_labels.padding = gt::px(d$pad),

      table_body.border.top.style = "none",
      table_body.hlines.color = ink,
      table_body.hlines.width = gt::px(1),
      table_body.border.bottom.style = "none",

      row_group.border.top.style = "solid",
      row_group.border.top.width = gt::px(2),
      row_group.border.top.color = ink,
      row_group.border.bottom.style = "none",
      row_group.padding = gt::px(max(d$pad - 1, 3)),

      source_notes.border.lr.style = "none",
      source_notes.border.bottom.style = "none",
      source_notes.padding = gt::px(d$pad + 2),
      footnotes.border.bottom.style = "none",
      ...
    ) %>%
    gt::opt_css(c(
      .theme_tabular_nums(table_id),
      .theme_last_row_border(table_id, "#FFFFFF"),
      paste0("#", table_id, " .gt_col_heading { letter-spacing: 0.04em; }"),
      paste0("#", table_id, " .gt_group_heading { letter-spacing: 0.06em; }"),
      paste0("#", table_id, " .gt_title { letter-spacing: -0.02em; line-height: 1.05; ",
             "padding-bottom: ", ceiling(d$pad / 2), "px !important; }"),
      paste0("#", table_id, " .gt_subtitle { padding-bottom: ", d$pad + 2, "px !important; }")
    ))
}
