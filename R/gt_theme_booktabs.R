#' Academic booktabs theme for `gt` tables
#'
#' Three horizontal rules and nothing else, the way LaTeX booktabs draws them. A
#' heavy rule sits above the column labels, a lighter one below them, and a heavy
#' rule closes the body. There are no vertical rules, no rules between data rows,
#' and no fills. The type is a Times-compatible serif.
#'
#' `accent` recolors the three rules and the row-group labels.
#'
#' @section Density:
#'
#' `density` sets the type and padding scale together. `"comfortable"` uses a
#' 14px body with roomy rows, `"compact"` a 12px body with tight rows, and
#' `"social"` a 17px body with generous rows and a larger title, at the scale
#' [gt_save_crop()] and [gt_social_crop()] export at.
#'
#' @param gt_object A `gt` table object to modify.
#' @param accent Character. A hex color for the three rules and the row-group
#'   labels. Defaults to `"#111111"`, near-black, for the standard black rules.
#' @param density Character. The type and padding scale. One of `"comfortable"`,
#'   `"compact"`, or `"social"`. See Density. Defaults to `"comfortable"`.
#' @param ... Additional arguments passed to `gt::tab_options`, applied last so
#'   they override anything the theme sets.
#'
#' @returns Returns a modified `gt` table with the theme applied.
#'
#' @section Figures:
#' \if{html}{\figure{gt_theme_booktabs.png}{options: width=100\%}}
#'
#' @examples
#' \dontrun{
#' library(gt)
#'
#' gt(head(mtcars[c("mpg", "hp", "wt")], 8)) %>% gt_theme_booktabs()
#'
#' # a regression table, with significance stars and a colored rule
#' fit <- lm(mpg ~ wt + hp, data = mtcars)
#' data.frame(
#'   Term = names(coef(fit)),
#'   Estimate = coef(fit),
#'   p = summary(fit)$coefficients[, 4]
#' ) %>%
#'   gt() %>%
#'   fmt_number(Estimate, decimals = 3) %>%
#'   gt_significance(Estimate, p) %>%
#'   gt_theme_booktabs(accent = "#1A3E6F")
#' }
#'
#' @seealso [gt_theme_tufte()] for a lighter, minimal-ink relative, and
#'   [gt_significance()] for significance notation.
#' @import gt
#' @importFrom magrittr %>%
#' @export
gt_theme_booktabs <- function(gt_object, accent = "#111111",
                              density = c("comfortable", "compact", "social"), ...) {

  .check_gt(gt_object)
  d <- .theme_density(density)

  ink <- "#111111"
  secondary <- "#5A5A5A"
  bg <- "#FFFFFF"

  res <- .table_id(gt_object)
  gt_object <- res$object
  table_id <- res$id

  serif <- function(...) gt::cell_text(font = gt::google_font("Tinos"), ...)

  gt_object %>%
    gt::opt_table_font(font = list(gt::google_font("Tinos"), gt::default_fonts())) %>%
    gt::tab_style(locations = gt::cells_body(),
                  style = gt::cell_text(color = ink, size = gt::px(d$body))) %>%
    gt::tab_style(locations = gt::cells_title("title"),
                  style = serif(weight = 700, size = gt::px(d$title), color = ink)) %>%
    gt::tab_style(locations = gt::cells_title("subtitle"),
                  style = serif(weight = 400, style = "italic", size = gt::px(d$subtitle), color = secondary)) %>%
    # column labels, same serif, no caps
    gt::tab_style(locations = gt::cells_column_labels(),
                  style = serif(weight = 700, size = gt::px(d$label + 1), color = ink)) %>%
    gt::tab_style(locations = gt::cells_column_spanners(),
                  style = serif(weight = 700, size = gt::px(d$label + 1), color = ink)) %>%
    # row groups as a bold italic subheading
    gt::tab_style(locations = gt::cells_row_groups(),
                  style = serif(weight = 700, style = "italic", size = gt::px(d$group + 1), color = accent)) %>%
    gt::tab_style(locations = gt::cells_source_notes(),
                  style = serif(size = gt::px(d$source), color = secondary)) %>%
    gt::tab_style(locations = gt::cells_footnotes(),
                  style = serif(size = gt::px(d$source), color = secondary)) %>%
    gt::tab_options(
      table.background.color = bg,
      table.font.size = gt::px(d$body),
      data_row.padding = gt::px(d$pad),

      heading.align = "left",
      heading.border.bottom.style = "none",
      heading.padding = gt::px(d$pad),

      # top rule, above the column labels
      table.border.top.style = "none",
      column_labels.border.top.style = "solid",
      column_labels.border.top.width = gt::px(2),
      column_labels.border.top.color = accent,
      # mid rule, under the column labels
      column_labels.border.bottom.style = "solid",
      column_labels.border.bottom.width = gt::px(1),
      column_labels.border.bottom.color = accent,
      column_labels.padding = gt::px(max(d$pad - 1, 2)),

      # nothing between the data rows
      table_body.border.top.style = "none",
      table_body.hlines.style = "none",
      # bottom rule, closing the body
      table_body.border.bottom.style = "solid",
      table_body.border.bottom.width = gt::px(2),
      table_body.border.bottom.color = accent,
      table.border.bottom.style = "none",

      column_labels.vlines.style = "none",
      table_body.vlines.style = "none",
      stub.border.style = "none",

      # a midrule above each row group
      row_group.border.top.style = "solid",
      row_group.border.top.width = gt::px(1),
      row_group.border.top.color = accent,
      row_group.border.bottom.style = "none",
      row_group.padding = gt::px(max(d$pad - 2, 2)),

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
