#' Editorial serif theme for `gt` tables
#'
#' Sets a serif body under small letterspaced sans column labels, with hairline
#' rules and no fills anywhere. Row groups are an uppercase label over a rule
#' rather than a colored band.
#'
#' `accent` recolors the rule above the table and the row-group labels.
#'
#' @section Density:
#'
#' `density` sets the type and padding scale together. `"comfortable"` uses a
#' 14px body with roomy rows, `"compact"` a 12px body with tight rows, and
#' `"social"` a 17px body with generous rows and a larger title, at the scale
#' [gt_save_crop()] and [gt_social_crop()] export at.
#'
#' @param gt_object A `gt` table object to modify.
#' @param accent Character. A hex color for the rule above the table and for the
#'   row-group labels. Defaults to `"#A6081A"`.
#' @param density Character. The type and padding scale. One of `"comfortable"`,
#'   `"compact"`, or `"social"`. See Density. Defaults to `"comfortable"`.
#' @param paper Character. The table background. Either `"white"` for a warm
#'   off-white, `"salmon"` for the financial-press pink, or any hex color, in
#'   which case the hairline color is left neutral. Defaults to `"white"`.
#' @param ... Additional arguments passed to `gt::tab_options`, applied last so
#'   they override anything the theme sets.
#'
#' @returns Returns a modified `gt` table with the theme applied.
#'
#' @section Figures:
#' \if{html}{\figure{gt_theme_broadsheet.png}{options: width=100\%}}
#'
#' @examples
#' \dontrun{
#' library(gt)
#'
#' gt(head(mtcars[c("mpg", "hp", "wt")], 8)) %>% gt_theme_broadsheet()
#'
#' # financial-press pink, sized for an image export
#' gt(head(airquality, 8)) %>%
#'   gt_theme_broadsheet(paper = "salmon", accent = "#0F5257", density = "social")
#' }
#'
#' @seealso [gt_title_header()] for a richer header block, and [gt_legend_continuous()] for
#'   explaining a colored column.
#' @import gt
#' @importFrom magrittr %>%
#' @export
gt_theme_broadsheet <- function(gt_object, accent = "#A6081A",
                                density = c("comfortable", "compact", "social"),
                                paper = "white", ...) {

  .check_gt(gt_object)
  d <- .theme_density(density)

  # each paper preset carries a rule color matched to its ground
  stock <- switch(
    paper,
    white = list(bg = "#FBFAF7", rule = "#DEDAD2"),
    salmon = list(bg = "#FFF1E5", rule = "#EAD9C7"),
    list(bg = paper, rule = "#DEDAD2")
  )
  ink <- "#16130F"
  secondary <- "#5C574F"

  res <- .table_id(gt_object)
  gt_object <- res$object
  table_id <- res$id

  gt_object %>%
    # serif body
    gt::opt_table_font(
      font = list(gt::google_font("Source Serif 4"), gt::default_fonts())
    ) %>%
    gt::tab_style(
      locations = gt::cells_body(),
      style = gt::cell_text(color = ink, size = gt::px(d$body))
    ) %>%
    # title / subtitle
    gt::tab_style(
      locations = gt::cells_title("title"),
      style = gt::cell_text(
        font = gt::google_font("Newsreader"),
        weight = 600, size = gt::px(d$title), color = ink
      )
    ) %>%
    gt::tab_style(
      locations = gt::cells_title("subtitle"),
      style = gt::cell_text(
        font = gt::google_font("Newsreader"),
        weight = 400, style = "italic", size = gt::px(d$subtitle), color = secondary
      )
    ) %>%
    # small letterspaced sans labels
    gt::tab_style(
      locations = gt::cells_column_labels(),
      style = gt::cell_text(
        font = gt::google_font("Public Sans"),
        weight = 600, size = gt::px(d$label), color = secondary,
        transform = "uppercase"
      )
    ) %>%
    gt::tab_style(
      locations = gt::cells_column_spanners(),
      style = gt::cell_text(
        font = gt::google_font("Public Sans"),
        weight = 600, size = gt::px(d$label), color = secondary,
        transform = "uppercase"
      )
    ) %>%
    # row groups get a label over a rule, not a filled band
    gt::tab_style(
      locations = gt::cells_row_groups(),
      style = gt::cell_text(
        font = gt::google_font("Public Sans"),
        weight = 700, size = gt::px(d$group), color = accent,
        transform = "uppercase"
      )
    ) %>%
    gt::tab_style(
      locations = gt::cells_source_notes(),
      style = gt::cell_text(
        font = gt::google_font("Public Sans"),
        size = gt::px(d$source), color = secondary
      )
    ) %>%
    gt::tab_style(
      locations = gt::cells_footnotes(),
      style = gt::cell_text(
        font = gt::google_font("Public Sans"),
        size = gt::px(d$source), color = secondary
      )
    ) %>%
    gt::tab_options(
      table.background.color = stock$bg,
      table.font.size = gt::px(d$body),
      data_row.padding = gt::px(d$pad),

      # thin accent rule across the top
      table.border.top.style = "solid",
      table.border.top.width = gt::px(2),
      table.border.top.color = accent,
      table.border.bottom.style = "none",

      heading.align = "left",
      heading.border.bottom.style = "none",
      heading.padding = gt::px(d$pad),

      # the one heavy rule, under the column labels
      column_labels.border.top.style = "none",
      column_labels.border.bottom.style = "solid",
      column_labels.border.bottom.width = gt::px(1.5),
      column_labels.border.bottom.color = ink,
      column_labels.padding = gt::px(max(d$pad - 2, 2)),

      # hairlines between rows, firmer rule to close the body
      table_body.border.top.style = "none",
      table_body.hlines.color = stock$rule,
      table_body.hlines.width = gt::px(1),
      table_body.border.bottom.style = "solid",
      table_body.border.bottom.width = gt::px(1),
      table_body.border.bottom.color = ink,

      row_group.border.top.style = "solid",
      row_group.border.top.width = gt::px(1),
      row_group.border.top.color = ink,
      row_group.border.bottom.style = "none",
      row_group.padding = gt::px(max(d$pad - 3, 2)),

      source_notes.border.lr.style = "none",
      source_notes.border.bottom.style = "none",
      source_notes.padding = gt::px(d$pad),
      footnotes.border.bottom.style = "none",
      ...
    ) %>%
    gt::opt_css(c(
      .theme_tabular_nums(table_id),
      .theme_last_row_border(table_id, stock$bg),
      # no tab_options equivalent for letter-spacing
      paste0("#", table_id, " .gt_col_heading, #", table_id,
             " .gt_column_spanner { letter-spacing: 0.09em; }"),
      paste0("#", table_id, " .gt_row_group_first td { padding-top: ",
             max(d$pad - 2, 2), "px; }"),
      paste0("#", table_id, " .gt_group_heading { letter-spacing: 0.08em; }"),
      # the heading needs more air under it than the rows have between them
      paste0("#", table_id, " .gt_subtitle { padding-bottom: ", d$pad + 8, "px !important; }"),
      paste0("#", table_id, " .gt_title { padding-bottom: ", ceiling(d$pad / 2), "px !important; }"),
      # keep the source note off the closing rule
      paste0("#", table_id, " .gt_sourcenote { padding-top: ", d$pad + 4, "px; }")
    ))
}
