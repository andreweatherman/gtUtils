#' Minimal-ink theme for `gt` tables
#'
#' A spare, Tufte-inspired look. An old-style serif on a warm white ground,
#' italic column labels, and a single hairline under them. There are no vertical
#' rules, no rules between rows, and no fills. A faint hairline closes the body.
#'
#' `accent` recolors the header hairline and the row-group labels.
#'
#' @section Density:
#'
#' `density` sets the type and padding scale together. `"comfortable"` uses a
#' 14px body with roomy rows, `"compact"` a 12px body with tight rows, and
#' `"social"` a 17px body with generous rows and a larger title, at the scale
#' [gt_save_crop()] and [gt_social_crop()] export at.
#'
#' @param gt_object A `gt` table object to modify.
#' @param accent Character. A hex color for the header hairline and the row-group
#'   labels. Defaults to `"#111111"`, near-black. A muted red or rust gives the
#'   Tufte marginal accent.
#' @param density Character. The type and padding scale. One of `"comfortable"`,
#'   `"compact"`, or `"social"`. See Density. Defaults to `"comfortable"`.
#' @param ... Additional arguments passed to `gt::tab_options`, applied last so
#'   they override anything the theme sets.
#'
#' @returns Returns a modified `gt` table with the theme applied.
#'
#' @section Figures:
#' \if{html}{\figure{gt_theme_tufte.png}{options: width=100\%}}
#'
#' @examples
#' \dontrun{
#' library(gt)
#'
#' gt(head(mtcars[c("mpg", "hp", "wt")], 8)) %>% gt_theme_tufte()
#'
#' # a muted rust accent on the hairline
#' gt(head(airquality, 8)) %>% gt_theme_tufte(accent = "#7B3F2B")
#' }
#'
#' @seealso [gt_theme_booktabs()] for a firmer, academic relative.
#' @import gt
#' @importFrom magrittr %>%
#' @export
gt_theme_tufte <- function(gt_object, accent = "#111111",
                           density = c("comfortable", "compact", "social"), ...) {

  .check_gt(gt_object)
  d <- .theme_density(density)

  ink <- "#111111"
  secondary <- "#6F6A60"
  hair <- "#C9C4B8"
  bg <- "#FFFFF8"

  res <- .table_id(gt_object)
  gt_object <- res$object
  table_id <- res$id

  serif <- function(...) gt::cell_text(font = gt::google_font("EB Garamond"), ...)

  gt_object %>%
    gt::opt_table_font(font = list(gt::google_font("EB Garamond"), gt::default_fonts())) %>%
    gt::tab_style(locations = gt::cells_body(),
                  style = gt::cell_text(color = ink, size = gt::px(d$body + 1))) %>%
    gt::tab_style(locations = gt::cells_title("title"),
                  style = serif(weight = 500, size = gt::px(d$title), color = ink)) %>%
    gt::tab_style(locations = gt::cells_title("subtitle"),
                  style = serif(weight = 400, style = "italic", size = gt::px(d$subtitle), color = secondary)) %>%
    # italic labels, de-emphasized
    gt::tab_style(locations = gt::cells_column_labels(),
                  style = serif(weight = 400, style = "italic", size = gt::px(d$label + 2), color = secondary)) %>%
    gt::tab_style(locations = gt::cells_column_spanners(),
                  style = serif(weight = 400, style = "italic", size = gt::px(d$label + 2), color = secondary)) %>%
    gt::tab_style(locations = gt::cells_row_groups(),
                  style = serif(weight = 600, style = "italic", size = gt::px(d$group + 2), color = accent)) %>%
    gt::tab_style(locations = gt::cells_source_notes(),
                  style = serif(style = "italic", size = gt::px(d$source + 1), color = secondary)) %>%
    gt::tab_style(locations = gt::cells_footnotes(),
                  style = serif(style = "italic", size = gt::px(d$source + 1), color = secondary)) %>%
    gt::tab_options(
      table.background.color = bg,
      table.font.size = gt::px(d$body + 1),
      data_row.padding = gt::px(d$pad),

      heading.align = "left",
      heading.border.bottom.style = "none",
      heading.padding = gt::px(d$pad),

      # no top rule
      table.border.top.style = "none",
      table.border.bottom.style = "none",
      column_labels.border.top.style = "none",
      # the one rule, a hairline under the labels
      column_labels.border.bottom.style = "solid",
      column_labels.border.bottom.width = gt::px(1),
      column_labels.border.bottom.color = accent,
      column_labels.padding = gt::px(max(d$pad - 1, 2)),

      # nothing between the rows, a faint hairline to close the body
      table_body.border.top.style = "none",
      table_body.hlines.style = "none",
      table_body.border.bottom.style = "solid",
      table_body.border.bottom.width = gt::px(1),
      table_body.border.bottom.color = hair,

      column_labels.vlines.style = "none",
      table_body.vlines.style = "none",
      stub.border.style = "none",

      # row groups get a label, no band and no rule
      row_group.border.top.style = "none",
      row_group.border.bottom.style = "none",
      row_group.padding = gt::px(max(d$pad - 1, 2)),

      source_notes.border.lr.style = "none",
      source_notes.border.bottom.style = "none",
      source_notes.padding = gt::px(d$pad),
      footnotes.border.bottom.style = "none",
      ...
    ) %>%
    gt::opt_css(c(
      .theme_tabular_nums(table_id),
      paste0("#", table_id, " .gt_sourcenote { padding-top: ", d$pad + 4, "px; }")
    ))
}
