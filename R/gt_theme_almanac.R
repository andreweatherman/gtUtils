#' Record-book theme for `gt` tables
#'
#' The look of a printed statistical abstract. A slab body, narrow condensed
#' labels, tight rows, and row banding to walk the eye across a wide table.
#'
#' It is the only theme in the package that bands its rows.
#'
#' @section Density:
#'
#' `density` sets the type and padding scale together. `"comfortable"` uses a
#' 14px body with roomy rows, `"compact"` a 12px body with tight rows, and
#' `"social"` a 17px body with generous rows and a larger title, at the scale
#' [gt_save_crop()] and [gt_social_crop()] export at.
#'
#' @param gt_object A `gt` table object to modify.
#' @param accent Character. A hex color for the row-group labels and the rule
#'   above the table. Defaults to `"#8C2F1E"`.
#' @param density Character. The type and padding scale. One of `"comfortable"`,
#'   `"compact"`, or `"social"`. See Density. Defaults to `"compact"`.
#' @param stripe Character. A hex color for the banded rows. Pass `NA` to switch
#'   banding off and keep the rest of the theme. Defaults to `"#F1F1EF"`.
#' @param ... Additional arguments passed to `gt::tab_options`, applied last so
#'   they override anything the theme sets.
#'
#' @returns Returns a modified `gt` table with the theme applied.
#'
#' @section Figures:
#' \if{html}{\figure{gt_theme_almanac.png}{options: width=100\%}}
#'
#' @examples
#' \dontrun{
#' library(gt)
#' gt(head(mtcars, 12)) %>% gt_theme_almanac()
#'
#' # banding off, cooler accent
#' gt(head(airquality, 15)) %>% gt_theme_almanac(stripe = NA, accent = "#1F3A5F")
#' }
#'
#' @import gt
#' @importFrom magrittr %>%
#' @export
gt_theme_almanac <- function(gt_object, accent = "#8C2F1E",
                             density = c("compact", "comfortable", "social"),
                             stripe = "#F1F1EF", ...) {

  .check_gt(gt_object)
  d <- .theme_density(density)

  ink <- "#1A1A1A"
  secondary <- "#6B6B68"
  rule <- "#D8D8D4"

  res <- .table_id(gt_object)
  gt_object <- res$object
  table_id <- res$id

  gt_object %>%
    gt::opt_table_font(
      font = list(gt::google_font("Zilla Slab"), gt::default_fonts())
    ) %>%
    gt::opt_row_striping(row_striping = !is.na(stripe)) %>%
    gt::tab_style(
      locations = gt::cells_body(),
      style = gt::cell_text(color = ink, size = gt::px(d$body))
    ) %>%
    gt::tab_style(
      locations = gt::cells_title("title"),
      style = gt::cell_text(weight = 700, size = gt::px(d$title), color = ink)
    ) %>%
    gt::tab_style(
      locations = gt::cells_title("subtitle"),
      style = gt::cell_text(weight = 400, size = gt::px(d$subtitle), color = secondary)
    ) %>%
    gt::tab_style(
      locations = gt::cells_column_labels(),
      style = gt::cell_text(
        font = gt::google_font("Archivo Narrow"),
        weight = 700, size = gt::px(d$label + 1), color = ink, transform = "uppercase"
      )
    ) %>%
    gt::tab_style(
      locations = gt::cells_column_spanners(),
      style = gt::cell_text(
        font = gt::google_font("Archivo Narrow"),
        weight = 700, size = gt::px(d$label + 1), color = ink, transform = "uppercase"
      )
    ) %>%
    gt::tab_style(
      locations = gt::cells_row_groups(),
      style = gt::cell_text(
        font = gt::google_font("Archivo Narrow"),
        weight = 700, size = gt::px(d$group + 1), color = accent, transform = "uppercase"
      )
    ) %>%
    gt::tab_style(
      locations = gt::cells_source_notes(),
      style = gt::cell_text(
        font = gt::google_font("Archivo Narrow"),
        size = gt::px(d$source + 1), color = secondary
      )
    ) %>%
    gt::tab_style(
      locations = gt::cells_footnotes(),
      style = gt::cell_text(
        font = gt::google_font("Archivo Narrow"),
        size = gt::px(d$source + 1), color = secondary
      )
    ) %>%
    gt::tab_options(
      table.background.color = "#FFFFFF",
      row.striping.background_color = if (is.na(stripe)) "#FFFFFF" else stripe,
      table.font.size = gt::px(d$body),
      data_row.padding = gt::px(d$pad),

      table.border.top.style = "solid",
      table.border.top.width = gt::px(2),
      table.border.top.color = accent,
      table.border.bottom.style = "none",

      heading.align = "left",
      heading.border.bottom.style = "none",
      heading.padding = gt::px(d$pad + 2),

      column_labels.border.top.style = "none",
      column_labels.border.bottom.style = "solid",
      column_labels.border.bottom.width = gt::px(1.5),
      column_labels.border.bottom.color = ink,
      column_labels.padding = gt::px(d$pad + 1),

      # banding does the work, so no row rules
      table_body.border.top.style = "none",
      table_body.hlines.style = "none",
      table_body.border.bottom.style = "solid",
      table_body.border.bottom.width = gt::px(1),
      table_body.border.bottom.color = ink,

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
      .theme_last_row_border(table_id, "#FFFFFF"),
      paste0("#", table_id, " .gt_col_heading, #", table_id,
             " .gt_column_spanner { letter-spacing: 0.05em; }"),
      paste0("#", table_id, " .gt_group_heading { letter-spacing: 0.05em; }"),
      paste0("#", table_id, " .gt_subtitle { padding-bottom: ", d$pad + 6, "px !important; }"),
      paste0("#", table_id, " .gt_title { padding-bottom: ", ceiling(d$pad / 2), "px !important; }")
    ))
}
