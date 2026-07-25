#' Data-terminal theme for `gt` tables
#'
#' A dense monospace theme in the register of a trading terminal. Dark background,
#' amber labels, a rule on every row, and tight padding.
#'
#' The monospace face has fixed-width digits already, so numeric columns line up
#' without any extra formatting.
#'
#' @section Density:
#'
#' `density` sets the type and padding scale together. `"comfortable"` uses a
#' 14px body with roomy rows, `"compact"` a 12px body with tight rows, and
#' `"social"` a 17px body with generous rows and a larger title, at the scale
#' [gt_save_crop()] and [gt_social_crop()] export at.
#'
#' @param gt_object A `gt` table object to modify.
#' @param accent Character. A hex color for the column labels, row groups, and
#'   the top rule. Defaults to amber `"#FFB86C"`; `"#7EE787"` gives a
#'   green-phosphor variant.
#' @param density Character. The type and padding scale. One of `"comfortable"`,
#'   `"compact"`, or `"social"`. See Density. Defaults to `"compact"`.
#' @param ... Additional arguments passed to `gt::tab_options`, applied last so
#'   they override anything the theme sets.
#'
#' @returns Returns a modified `gt` table with the theme applied.
#'
#' @section Figures:
#' \if{html}{\figure{gt_theme_terminal.png}{options: width=100\%}}
#'
#' @examples
#' \dontrun{
#' library(gt)
#' gt(head(mtcars, 12)) %>% gt_theme_terminal()
#' gt(head(airquality, 15)) %>% gt_theme_terminal(accent = "#7EE787")
#' }
#'
#' @seealso [pal_midnight] for a color scale that survives a dark ground.
#' @import gt
#' @importFrom magrittr %>%
#' @export
gt_theme_terminal <- function(gt_object, accent = "#FFB86C",
                              density = c("compact", "comfortable", "social"),
                              ...) {

  .check_gt(gt_object)
  d <- .theme_density(density)

  ground <- "#0F1115"
  primary <- "#C9D1D9"
  secondary <- "#7D8590"
  rule <- "#262B33"

  res <- .table_id(gt_object)
  gt_object <- res$object
  table_id <- res$id

  gt_object %>%
    gt::opt_table_font(
      font = list(gt::google_font("JetBrains Mono"), gt::default_fonts())
    ) %>%
    gt::tab_style(
      locations = gt::cells_body(),
      style = gt::cell_text(color = primary, size = gt::px(d$body))
    ) %>%
    gt::tab_style(
      locations = gt::cells_title("title"),
      style = gt::cell_text(
        weight = 700, size = gt::px(d$title - 2), color = primary,
        transform = "uppercase"
      )
    ) %>%
    gt::tab_style(
      locations = gt::cells_title("subtitle"),
      style = gt::cell_text(weight = 400, size = gt::px(d$subtitle - 1), color = secondary)
    ) %>%
    gt::tab_style(
      locations = gt::cells_column_labels(),
      style = gt::cell_text(
        weight = 700, size = gt::px(d$label), color = accent, transform = "uppercase"
      )
    ) %>%
    gt::tab_style(
      locations = gt::cells_column_spanners(),
      style = gt::cell_text(
        weight = 700, size = gt::px(d$label), color = accent, transform = "uppercase"
      )
    ) %>%
    gt::tab_style(
      locations = gt::cells_row_groups(),
      style = gt::cell_text(
        weight = 700, size = gt::px(d$group), color = accent, transform = "uppercase"
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
      table.background.color = ground,
      heading.background.color = ground,
      column_labels.background.color = ground,
      row_group.background.color = ground,
      stub.background.color = ground,
      source_notes.background.color = ground,
      table.font.size = gt::px(d$body),
      data_row.padding = gt::px(d$pad),

      table.border.top.style = "solid",
      table.border.top.width = gt::px(1),
      table.border.top.color = accent,
      table.border.bottom.style = "solid",
      table.border.bottom.width = gt::px(1),
      table.border.bottom.color = rule,

      heading.align = "left",
      heading.border.bottom.style = "none",
      heading.padding = gt::px(d$pad + 2),

      column_labels.border.top.style = "none",
      column_labels.border.bottom.style = "solid",
      column_labels.border.bottom.width = gt::px(1),
      column_labels.border.bottom.color = accent,
      column_labels.padding = gt::px(d$pad + 1),

      # rule on every row, it's a readout
      table_body.border.top.style = "none",
      table_body.hlines.color = rule,
      table_body.hlines.width = gt::px(1),
      table_body.border.bottom.style = "none",

      row_group.border.top.style = "solid",
      row_group.border.top.width = gt::px(1),
      row_group.border.top.color = rule,
      row_group.border.bottom.style = "none",
      row_group.padding = gt::px(max(d$pad, 3)),

      source_notes.border.lr.style = "none",
      source_notes.border.bottom.style = "none",
      source_notes.padding = gt::px(d$pad + 2),
      footnotes.border.bottom.style = "none",
      ...
    ) %>%
    gt::opt_css(c(
      .theme_tabular_nums(table_id),
      .theme_last_row_border(table_id, ground),
      paste0("#", table_id, " td, #", table_id, " th { line-height: 1.5; }"),
      paste0("#", table_id, " .gt_col_heading { letter-spacing: 0.08em; }"),
      paste0("#", table_id, " .gt_title { letter-spacing: 0.04em; padding-bottom: 2px !important; }"),
      paste0("#", table_id, " .gt_subtitle { padding-bottom: ", d$pad + 6, "px !important; }")
    ))
}
