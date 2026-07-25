#' Broadcast-graphics theme for `gt` tables
#'
#' A solid color band carrying condensed uppercase labels, over tight rows, after
#' a television lower-third or a stadium scoreboard.
#'
#' `accent` sets the band itself, so one argument restyles the whole table to a
#' brand color. The label color is checked against the band and flipped to dark
#' if the accent is pale.
#'
#' @section Density:
#'
#' `density` sets the type and padding scale together. `"comfortable"` uses a
#' 14px body with roomy rows, `"compact"` a 12px body with tight rows, and
#' `"social"` a 17px body with generous rows and a larger title, at the scale
#' [gt_save_crop()] and [gt_social_crop()] export at.
#'
#' @param gt_object A `gt` table object to modify.
#' @param accent Character. A hex color for the header band. The label color
#'   adapts to it. Defaults to `"#0E1621"`.
#' @param density Character. The type and padding scale. One of `"comfortable"`,
#'   `"compact"`, or `"social"`. See Density. Defaults to `"compact"`.
#' @param ... Additional arguments passed to `gt::tab_options`, applied last so
#'   they override anything the theme sets.
#'
#' @returns Returns a modified `gt` table with the theme applied.
#'
#' @section Figures:
#' \if{html}{\figure{gt_theme_scoreboard.png}{options: width=100\%}}
#'
#' @examples
#' \dontrun{
#' library(gt)
#' gt(head(mtcars)) %>% gt_theme_scoreboard()
#'
#' # a brand color carries the whole table
#' gt(head(mtcars)) %>% gt_theme_scoreboard(accent = "#0F766E")
#' }
#'
#' @seealso [gt_spotlight()] for picking out a row, and [gt_fmt_rank()] for ordinals.
#' @import gt
#' @importFrom magrittr %>%
#' @export
gt_theme_scoreboard <- function(gt_object, accent = "#0E1621",
                                density = c("compact", "comfortable", "social"),
                                ...) {

  .check_gt(gt_object)
  d <- .theme_density(density)

  ink <- "#141719"
  rule <- "#E6E9ED"
  # a pale accent needs dark labels, so measure it
  on_accent <- .theme_on_color(accent)

  res <- .table_id(gt_object)
  gt_object <- res$object
  table_id <- res$id

  gt_object %>%
    gt::opt_table_font(
      font = list(gt::google_font("Barlow"), gt::default_fonts())
    ) %>%
    gt::tab_style(
      locations = gt::cells_body(),
      style = gt::cell_text(color = ink, size = gt::px(d$body), weight = 500)
    ) %>%
    gt::tab_style(
      locations = gt::cells_title("title"),
      style = gt::cell_text(
        font = gt::google_font("Barlow Condensed"),
        weight = 700, size = gt::px(d$title + 4), color = ink,
        transform = "uppercase"
      )
    ) %>%
    gt::tab_style(
      locations = gt::cells_title("subtitle"),
      style = gt::cell_text(
        font = gt::google_font("Barlow Condensed"),
        weight = 500, size = gt::px(d$subtitle + 1), color = "#5A6069"
      )
    ) %>%
    gt::tab_style(
      locations = gt::cells_column_labels(),
      style = gt::cell_text(
        font = gt::google_font("Barlow Condensed"),
        weight = 700, size = gt::px(d$label + 2), color = on_accent,
        transform = "uppercase"
      )
    ) %>%
    gt::tab_style(
      locations = gt::cells_column_spanners(),
      style = gt::cell_text(
        font = gt::google_font("Barlow Condensed"),
        weight = 700, size = gt::px(d$label + 2), color = on_accent,
        transform = "uppercase"
      )
    ) %>%
    gt::tab_style(
      locations = gt::cells_row_groups(),
      style = list(
        gt::cell_text(
          font = gt::google_font("Barlow Condensed"),
          weight = 700, size = gt::px(d$group + 1), color = accent,
          transform = "uppercase"
        ),
        gt::cell_fill(color = "#F2F4F6")
      )
    ) %>%
    gt::tab_style(
      locations = gt::cells_source_notes(),
      style = gt::cell_text(size = gt::px(d$source), color = "#5A6069")
    ) %>%
    gt::tab_style(
      locations = gt::cells_footnotes(),
      style = gt::cell_text(size = gt::px(d$source), color = "#5A6069")
    ) %>%
    gt::tab_options(
      table.background.color = "#FFFFFF",
      column_labels.background.color = accent,
      table.font.size = gt::px(d$body),
      data_row.padding = gt::px(d$pad),

      table.border.top.style = "none",
      table.border.bottom.style = "none",

      heading.align = "left",
      heading.border.bottom.style = "none",
      heading.padding = gt::px(d$pad + 1),

      column_labels.border.top.style = "none",
      column_labels.border.bottom.style = "none",
      column_labels.padding = gt::px(d$pad + 2),

      table_body.border.top.style = "none",
      table_body.hlines.color = rule,
      table_body.hlines.width = gt::px(1),
      table_body.border.bottom.style = "solid",
      table_body.border.bottom.width = gt::px(2),
      table_body.border.bottom.color = accent,

      row_group.border.top.style = "none",
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
             " .gt_column_spanner { letter-spacing: 0.06em; }"),
      paste0("#", table_id, " .gt_group_heading { letter-spacing: 0.06em; }"),
      paste0("#", table_id, " .gt_heading { letter-spacing: 0.01em; }"),
      paste0("#", table_id, " .gt_subtitle { padding-bottom: ", d$pad + 6, "px !important; }")
    ))
}
