#' Dark theme for `gt` tables
#'
#' A restrained dark theme. The background is a near-black instead of pure black,
#' which avoids the smearing pure black causes on OLED screens. Column labels sit
#' on a slightly raised band, rules are hairlines, and the body has no fills.
#'
#' Light text on a dark background reads as lighter weight than it is, so the
#' theme opens up the line height to compensate.
#'
#' @section Color scales on dark grounds:
#'
#' A green-to-red ramp built for white paper goes muddy on a near-black
#' background, as its mid-tones collapse toward the background. Use [pal_midnight]
#' with [gt_color_ranks()] instead.
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
#'   above the table. Defaults to `"#5B8DEF"`.
#' @param density Character. The type and padding scale. One of `"comfortable"`,
#'   `"compact"`, or `"social"`. See Density. Defaults to `"comfortable"`.
#' @param ... Additional arguments passed to `gt::tab_options`, applied last so
#'   they override anything the theme sets.
#'
#' @returns Returns a modified `gt` table with the theme applied.
#'
#' @section Figures:
#' \if{html}{\figure{gt_theme_midnight.png}{options: width=100\%}}
#'
#' @examples
#' \dontrun{
#' library(gt)
#' gt(head(mtcars)) %>% gt_theme_midnight()
#'
#' # color scales need lifting on a dark ground; see pal_midnight
#' gt(head(airquality, 10)) %>%
#'   gt_theme_midnight() %>%
#'   gt_color_ranks(Temp, palette = pal_midnight)
#' }
#'
#' @seealso [pal_midnight] for a color scale that survives a dark ground, and
#'   [gt_theme_terminal()] for a denser dark look.
#' @import gt
#' @importFrom magrittr %>%
#' @export
gt_theme_midnight <- function(gt_object, accent = "#5B8DEF",
                              density = c("comfortable", "compact", "social"),
                              ...) {

  .check_gt(gt_object)
  d <- .theme_density(density)

  ground <- "#0C0D10"
  surface <- "#16181D"
  primary <- "#E8E9ED"
  secondary <- "#9498A3"
  rule <- "#24272E"

  res <- .table_id(gt_object)
  gt_object <- res$object
  table_id <- res$id

  gt_object %>%
    gt::opt_table_font(
      font = list(gt::google_font("Chivo"), gt::default_fonts())
    ) %>%
    gt::tab_style(
      locations = gt::cells_body(),
      style = gt::cell_text(color = primary, size = gt::px(d$body))
    ) %>%
    gt::tab_style(
      locations = gt::cells_title("title"),
      style = gt::cell_text(weight = 700, size = gt::px(d$title), color = primary)
    ) %>%
    gt::tab_style(
      locations = gt::cells_title("subtitle"),
      style = gt::cell_text(weight = 400, size = gt::px(d$subtitle), color = secondary)
    ) %>%
    # sentence case; uppercase tracking is too loud on this ground
    gt::tab_style(
      locations = gt::cells_column_labels(),
      style = gt::cell_text(weight = 600, size = gt::px(d$label + 1), color = secondary)
    ) %>%
    gt::tab_style(
      locations = gt::cells_column_spanners(),
      style = gt::cell_text(weight = 600, size = gt::px(d$label + 1), color = secondary)
    ) %>%
    gt::tab_style(
      locations = gt::cells_row_groups(),
      style = gt::cell_text(weight = 700, size = gt::px(d$group), color = accent)
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
      column_labels.background.color = surface,
      row_group.background.color = ground,
      stub.background.color = ground,
      source_notes.background.color = ground,
      table.font.size = gt::px(d$body),
      data_row.padding = gt::px(d$pad),

      table.border.top.style = "solid",
      table.border.top.width = gt::px(2),
      table.border.top.color = accent,
      table.border.bottom.style = "none",

      heading.align = "left",
      heading.border.bottom.style = "none",
      heading.padding = gt::px(d$pad),

      column_labels.border.top.style = "none",
      column_labels.border.bottom.style = "solid",
      column_labels.border.bottom.width = gt::px(1),
      column_labels.border.bottom.color = rule,
      column_labels.padding = gt::px(max(d$pad - 1, 3)),

      table_body.border.top.style = "none",
      table_body.hlines.color = rule,
      table_body.hlines.width = gt::px(1),
      table_body.border.bottom.style = "solid",
      table_body.border.bottom.width = gt::px(1),
      table_body.border.bottom.color = rule,

      row_group.border.top.style = "solid",
      row_group.border.top.width = gt::px(1),
      row_group.border.top.color = rule,
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
      .theme_last_row_border(table_id, ground),
      # light on dark reads lighter than it is, so open the leading
      paste0("#", table_id, " td, #", table_id, " th { line-height: 1.55; }"),
      paste0("#", table_id, " .gt_subtitle { padding-bottom: ", d$pad + 8, "px !important; }"),
      paste0("#", table_id, " .gt_title { padding-bottom: ", ceiling(d$pad / 2), "px !important; }")
    ))
}

#' A rank palette for dark backgrounds
#'
#' A five-color green-to-red ramp with its luminance range lifted so it still
#' reads on a near-black background. The default palette in [gt_color_ranks()] is
#' built for white paper, and its mid-tones collapse into a dark ground. Pass this
#' instead when using [gt_theme_midnight()] or [gt_theme_terminal()].
#'
#' @format A character vector of five hex colors, running best to worst.
#'
#' @examples
#' \dontrun{
#' gt::gt(head(airquality, 10)) %>%
#'   gt_theme_midnight() %>%
#'   gt_color_ranks(Temp, palette = pal_midnight)
#' }
#'
#' @seealso [gt_theme_midnight()].
#' @export
pal_midnight <- c("#3FBF87", "#8FD9A8", "#D8D6A0", "#E8996B", "#E0645C")
