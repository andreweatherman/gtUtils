#' Saturated single-color theme for `gt` tables
#'
#' Colors the entire table surface instead of putting a colored detail on a
#' neutral background, tying the table to a single brand or publication color.
#'
#' Text, rules, and secondary type are all derived from `color`. The type color is
#' whichever of black or white measures higher contrast against the background,
#' and the rules are built by shifting the background itself, since gray laid over
#' a saturated ground looks washed out.
#'
#' @section Legibility:
#'
#' The secondary color used for column labels, the subtitle and source notes is
#' blended toward the type color until it clears a 4.5:1 contrast ratio against
#' the background, so it stays readable whatever `color` you pass.
#'
#' @section Density:
#'
#' `density` sets the type and padding scale together. `"comfortable"` uses a
#' 14px body with roomy rows, `"compact"` a 12px body with tight rows, and
#' `"social"` a 17px body with generous rows and a larger title, at the scale
#' [gt_save_crop()] and [gt_social_crop()] export at.
#'
#' @param gt_object A `gt` table object to modify.
#' @param color Character. The hex color the table is drenched in. Anything from a
#'   near-black to a mid-saturation brand color works, and very pale colors flip
#'   the type to dark automatically. Defaults to `"#123F5E"`.
#' @param density Character. The type and padding scale. One of `"comfortable"`,
#'   `"compact"`, or `"social"`. See Density. Defaults to `"comfortable"`.
#' @param ... Additional arguments passed to `gt::tab_options`, applied last so
#'   they override anything the theme sets.
#'
#' @returns Returns a modified `gt` table with the theme applied.
#'
#' @section Figures:
#' \if{html}{\figure{gt_theme_drench.png}{options: width=100\%}}
#'
#' @examples
#' \dontrun{
#' library(gt)
#' gt(head(mtcars)) %>% gt_theme_drench()
#'
#' # a brand color, with a matching export canvas
#' gt(head(mtcars)) %>%
#'   gt_theme_drench(color = "#4B1E78", density = "social") %>%
#'   gt_social_crop(bg = "#4B1E78")
#' }
#'
#' @seealso [gt_theme_midnight()] for a restrained dark background instead.
#' @import gt
#' @importFrom magrittr %>%
#' @export
gt_theme_drench <- function(gt_object, color = "#123F5E",
                            density = c("comfortable", "compact", "social"),
                            ...) {

  .check_gt(gt_object)
  d <- .theme_density(density)

  # everything derives from the ground so any hue holds together
  on_color <- .theme_on_color(color)
  dark_type <- identical(on_color, "#000000")

  # shift the ground rather than laying gray over it
  shade <- function(steps) gt::adjust_luminance(color, steps = steps)
  rule <- if (dark_type) shade(-0.6) else shade(0.9)
  surface <- if (dark_type) shade(0.5) else shade(-0.7)
  # a fixed luminance step fails on some hues, so blend until it clears 4.5:1
  secondary <- .theme_secondary_on(color, on_color, target = 4.5)

  res <- .table_id(gt_object)
  gt_object <- res$object
  table_id <- res$id

  gt_object %>%
    gt::opt_table_font(
      font = list(gt::google_font("Gabarito"), gt::default_fonts())
    ) %>%
    gt::tab_style(
      locations = gt::cells_body(),
      style = gt::cell_text(color = on_color, size = gt::px(d$body), weight = 500)
    ) %>%
    gt::tab_style(
      locations = gt::cells_title("title"),
      style = gt::cell_text(weight = 700, size = gt::px(d$title + 2), color = on_color)
    ) %>%
    gt::tab_style(
      locations = gt::cells_title("subtitle"),
      style = gt::cell_text(weight = 400, size = gt::px(d$subtitle), color = secondary)
    ) %>%
    gt::tab_style(
      locations = gt::cells_column_labels(),
      style = gt::cell_text(
        weight = 700, size = gt::px(d$label), color = secondary, transform = "uppercase"
      )
    ) %>%
    gt::tab_style(
      locations = gt::cells_column_spanners(),
      style = gt::cell_text(
        weight = 700, size = gt::px(d$label), color = secondary, transform = "uppercase"
      )
    ) %>%
    gt::tab_style(
      locations = gt::cells_row_groups(),
      style = list(
        gt::cell_text(weight = 700, size = gt::px(d$group), color = on_color,
                      transform = "uppercase"),
        gt::cell_fill(color = surface)
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
      table.background.color = color,
      heading.background.color = color,
      column_labels.background.color = color,
      row_group.background.color = color,
      stub.background.color = color,
      source_notes.background.color = color,
      table.font.size = gt::px(d$body),
      data_row.padding = gt::px(d$pad + 1),

      table.border.top.style = "none",
      table.border.bottom.style = "none",

      heading.align = "left",
      heading.border.bottom.style = "none",
      heading.padding = gt::px(d$pad + 2),

      column_labels.border.top.style = "none",
      column_labels.border.bottom.style = "solid",
      column_labels.border.bottom.width = gt::px(1),
      column_labels.border.bottom.color = rule,
      column_labels.padding = gt::px(d$pad + 1),

      table_body.border.top.style = "none",
      table_body.hlines.color = rule,
      table_body.hlines.width = gt::px(1),
      table_body.border.bottom.style = "none",

      row_group.border.top.style = "none",
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
      .theme_last_row_border(table_id, color),
      # light type on a saturated ground reads lighter than it is
      if (!dark_type) paste0("#", table_id, " td, #", table_id, " th { line-height: 1.55; }") else "",
      paste0("#", table_id, " .gt_col_heading, #", table_id,
             " .gt_column_spanner { letter-spacing: 0.08em; }"),
      paste0("#", table_id, " .gt_group_heading { letter-spacing: 0.06em; }"),
      paste0("#", table_id, " .gt_subtitle { padding-bottom: ", d$pad + 8, "px !important; }"),
      paste0("#", table_id, " .gt_title { padding-bottom: ", ceiling(d$pad / 2), "px !important; }")
    ))
}
