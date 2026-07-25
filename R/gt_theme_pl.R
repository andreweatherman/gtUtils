#' Premier League theme for `gt` tables
#'
#' DM Sans throughout in the Premier League's deep purple (`#37003c`), with
#' muted-purple column labels and a purple rule bracketing the body. Row groups
#' render as white labels on a pale lilac fill, and column spanners are bold and
#' underlined.
#'
#' @section Density:
#'
#' `density` scales the theme's type and row padding together. `"comfortable"`
#' leaves every size as the theme sets it, `"compact"` scales both down, and
#' `"social"` scales both up, to the scale [gt_save_crop()] and
#' [gt_social_crop()] export at.
#'
#' @param gt_object A `gt` table object to modify.
#' @param density Character. The type and padding scale. One of `"comfortable"`,
#'   `"compact"`, or `"social"`. See Density. Defaults to `"comfortable"`.
#' @param ... Additional arguments passed to `gt::tab_options`, applied last so
#'   they override anything the theme sets.
#'
#' @returns Returns a modified `gt` table with the theme applied.
#'
#' @details
#' A purple rule closes the column labels and another opens the body, and each
#' body row is separated by a purple bottom border. The last body row's bottom
#' border is painted white so it does not double the rule that closes the table.
#'
#' @section Figures:
#' \if{html}{\figure{gt_theme_pl.png}{options: width=100\%}}
#'
#' @examples
#' \dontrun{
#' library(gt)
#' gt(head(mtcars)) %>% gt_theme_pl()
#' }
#'
#' @import gt
#' @importFrom magrittr %>%
#' @export
gt_theme_pl <- function(gt_object,
                        density = c("comfortable", "compact", "social"),
                        ...) {

  .check_gt(gt_object)

  res <- .table_id(gt_object)
  gt_object <- res$object
  table_id <- res$id
  data <- gt_object[["_data"]]

  gt_object %>%
    gt::tab_style(
      locations = gt::cells_body(
        columns = gt::everything()
      ),
      style = gt::cell_text(
        font = gt::google_font('DM Sans'),
        color = '#37003c',
        size = px(14)
      )
    ) %>%
    gt::tab_style(
      locations = gt::cells_column_labels(
        columns = gt::everything()
      ),
      style = gt::cell_text(
        font = gt::google_font('DM Sans'),
        color = '#87668a',
        weight = 650,
        size = px(13)
      )
    ) %>%
    gt::tab_style(
      locations = gt::cells_title('title'),
      style = gt::cell_text(
        font = gt::google_font('DM Sans'),
        weight = 650
      )
    ) %>%
    gt::tab_style(
      locations = gt::cells_title('subtitle'),
      style = gt::cell_text(
        font = gt::google_font('DM Sans'),
        weight = 500
      )
    ) %>%
    gt::tab_style(
      locations = gt::cells_column_spanners(),
      style = gt::cell_text(
        font = gt::google_font("DM Sans"),
        weight = 650,
        size = px(12),
        color = "#37003c"
      )
    ) %>%
    gt::tab_style(
      locations = gt::cells_row_groups(),
      style = list(
        gt::cell_text(
          font = gt::google_font("DM Sans"),
          weight = 650,
          size = px(12),
          color = "#ffffff"
        ),
        gt::cell_fill(
          color = "#C0BACA"
        )
      )
    ) %>%
    gt::tab_style(
      locations = gt::cells_footnotes(),
      style = gt::cell_text(
        font = gt::google_font("DM Sans"),
        size = px(12)
      )
    ) %>%
    gt::tab_style(
      locations = gt::cells_source_notes(),
      style = gt::cell_text(
        font = gt::google_font('DM Sans'),
        size = px(12)
      )
    ) %>%
    gt::tab_style(
      locations = gt::cells_body(rows = 1:(nrow(data) - 1)),
      style = gt::cell_borders(sides = "bottom", color = "#37003c")
    ) %>%
    gt::tab_style(
      locations = gt::cells_body(rows = 1),
      style = gt::cell_borders(sides = "top", color = "#37003c")
    ) %>%
    gt::tab_options(
      heading.align = "left",
      column_labels.border.top.style = "none",
      table.border.top.style = "none",
      table_body.border.top.style = "solid",
      table_body.border.top.width = px(1),
      table_body.border.top.color = "#37003c",
      table_body.border.bottom.color = "white",
      heading.border.bottom.style = "none",
      data_row.padding = px(2),
      row_group.padding = px(1.5),
      row_group.border.top.style = "none",
      row_group.border.bottom.width = px(1),
      row_group.border.bottom.color = "#37003c",
      row_group.border.bottom.style = 'solid',
      table.border.bottom.style = "none",
      source_notes.border.lr.style = "none",
      column_labels.border.bottom.style = "solid",
      column_labels.border.bottom.width = px(1),
      column_labels.border.bottom.color = "#37003c",
      ...
    ) %>%
    gt::opt_css(
      c(
      .theme_last_row_border(table_id, "#FFFFFF"),
      paste0("#", table_id,
             " .gt_col_heading
             {
              padding-bottom: 3px;
             }",
             "#", table_id,
             " .gt_heading
             {
              padding-bottom: 0px;
              padding-top: 6px
            }",
             "#", table_id,
             " .gt_subtitle
             {
              padding-top: 2px;
              padding-bottom: 6px;
            }",
             paste0("#", table_id, " .gt_column_spanner {font-size: 13px; font-weight: bold; padding-bottom: 2px;}"),
             "#", table_id,
             " .gt_sourcenote
             {
              line-height: 1.2
            }")
      ),
      add = TRUE
    ) %>%
    .theme_scale_output(density)

}
