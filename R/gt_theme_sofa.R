#' SofaScore theme for `gt` tables
#'
#' Sofia Sans Condensed throughout on a warm cream (`"light"`) or dark navy
#' (`"dark"`) ground, after SofaScore, with bold column labels and bold row-group
#' labels. Column spanners are bold and underlined. Horizontal rules are hidden,
#' so the ground alone separates the rows.
#'
#' @section Density:
#'
#' `density` scales the theme's type and row padding together. `"comfortable"`
#' leaves every size as the theme sets it, `"compact"` scales both down, and
#' `"social"` scales both up, to the scale [gt_save_crop()] and
#' [gt_social_crop()] export at.
#'
#' @param gt_object A `gt` table object to modify.
#' @param style Character. The color scheme, `"light"` for a warm cream ground or
#'   `"dark"` for a dark navy ground. Defaults to `"light"`.
#' @param density Character. The type and padding scale. One of `"comfortable"`,
#'   `"compact"`, or `"social"`. See Density. Defaults to `"comfortable"`.
#' @param ... Additional arguments passed to `gt::tab_options`, applied last so
#'   they override anything the theme sets.
#'
#' @returns Returns a modified `gt` table with the theme applied.
#'
#' @details
#' The table background, its outer borders, and the last row's bottom border are
#' all painted in the chosen ground color, so the rows read as separated by space
#' rather than by rules. Row groups are closed with a black bottom border.
#'
#' @section Figures:
#' \if{html}{\figure{gt_theme_sofa_light.png}{options: width=100\%}}
#' \if{html}{\figure{gt_theme_sofa_dark.png}{options: width=100\%}}
#'
#' @examples
#' \dontrun{
#' library(gt)
#' gt(head(mtcars)) %>% gt_theme_sofa()
#' gt(head(mtcars)) %>% gt_theme_sofa(style = "dark")
#' }
#'
#' @import gt
#' @importFrom magrittr %>%
#' @export
gt_theme_sofa <- function(gt_object, style = "light",
                          density = c("comfortable", "compact", "social"),
                          ...) {

  .check_gt(gt_object)

  table_id <- subset(gt_object[['_options']], parameter == 'table_id')$value[[1]]

  if (is.na(table_id)) {
    table_id <- gt::random_id()
    opt_position <- which("table_id" %in% gt_object[["_options"]][["parameter"]])[[1]]
    gt_object[["_options"]][["value"]][[opt_position]] <- table_id
  }

  base_color <- if(style == "light") "#F0EAD6" else "#1c2632"

  gt_object %>%
    # cell body
    gt::tab_style(
      locations = gt::cells_body(),
      style = gt::cell_text(
        font = gt::google_font('Sofia Sans Condensed'),
        size = px(14)
      )
    ) %>%
    # col. headers
    gt::tab_style(
      locations = gt::cells_column_labels(),
      style = gt::cell_text(
        weight = 'bold',
        font = gt::google_font('Sofia Sans Condensed'),
        size = px(14)
      )
    ) %>%
    # group rows
    gt::tab_style(
      locations = gt::cells_row_groups(),
      style = gt::cell_text(
        weight = 'bold',
        font = gt::google_font('Sofia Sans Condensed'),
        size = px(14)
      )
    ) %>%
    # footnote
    gt::tab_style(
      locations = gt::cells_footnotes(),
      style = gt::cell_text(
        font = gt::google_font('Sofia Sans Condensed'),
        size = px(12)
      )
    ) %>%
    # title
    gt::tab_style(
      locations = gt::cells_title('title'),
      style = gt::cell_text(
        weight = 'bold',
        font = gt::google_font('Sofia Sans Condensed'),
        size = px(22)
      )
    ) %>%
    # subtitle
    gt::tab_style(
      locations = gt::cells_title('subtitle'),
      style = gt::cell_text(
        font = gt::google_font('Sofia Sans Condensed'),
        size = px(14)
      )
    ) %>%
    # spanner
    gt::tab_style(
      locations = gt::cells_column_spanners(),
      style = gt::cell_text(
        font = gt::google_font("Sofia Sans Condensed"),
        weight = 650,
        size = px(12)
      )
    ) %>%
    # caption
    gt::tab_style(
      locations = gt::cells_source_notes(),
      style = gt::cell_text(
        font = gt::google_font('Sofia Sans Condensed'),
        size = px(10)
      )
    ) %>%
    gt::tab_options(
      data_row.padding = 1,
      table_body.hlines.color = "transparent",
      # column_labels.border.top.style = 'solid',
      # column_labels.border.top.color = '#ffffff',
      # column_labels.border.top.width = px(0.5),
      column_labels.border.top.style = 'none',
      column_labels.border.bottom.style = 'none',
      row_group.border.top.style = "none",
      row_group.border.top.color = "black",
      row_group.border.bottom.width = px(1),
      row_group.border.bottom.color = "black",
      row_group.border.bottom.style = 'solid',
      row_group.padding = px(1.5),
      heading.align = 'left',
      heading.border.bottom.style = "none",
      table_body.border.top.style = "none",
      table.border.bottom.style = 'none',
      table.border.top.style = 'none',
      source_notes.border.lr.style = "none",
      table.background.color = base_color,
      table.border.top.color = base_color,
      table.border.right.color = base_color,
      table.border.bottom.color = base_color,
      table.border.left.color = base_color,
      ...
    ) %>%
    gt::opt_css(c(
      paste0(
        "#",
        table_id,
        " tbody tr:last-child {border-bottom: 2px solid ", base_color, ";}"
      ),
      paste0(
        "#",
        table_id,
        " .gt_col_heading {padding-bottom: 2px; padding-top: 2px;}"
      ),
      paste0(
        "#",
        table_id,
        " .gt_subtitle {padding-top:0px !important; padding-bottom: 4px !important;}"
      ),
      paste0(
        "#",
        table_id,
        " .gt_sourcenote {border-bottom-color: ", base_color, " !important;}"
      ),
      paste0(
        "#",
        table_id,
        " .gt_heading {padding-bottom: 0px; padding-top: 6px;}"
      ),
      paste0("#", table_id, " .gt_column_spanner {font-size: 12px; font-weight: bold; text-decoration: underline;}")
    )) %>%
    .theme_scale_output(density)
}
