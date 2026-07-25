#' NCAA theme for `gt` tables
#'
#' Open Sans throughout, with uppercase white column labels on a solid black band
#' and zebra-striped rows, after the NCAA. Row groups render as white labels on a
#' dark gray fill, column spanners are underlined, and every column is left
#' aligned with a 25px left indent on each row.
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
#' Column labels sit in a solid black band in white uppercase Open Sans, while
#' source notes and footnotes switch to Almarai. Rows are zebra-striped through
#' [gt::opt_row_striping()], horizontal rules are hidden, and each row and heading
#' carries a 25px left indent. The last body row's bottom border is painted white
#' so it does not double the rule that closes the table.
#'
#' @section Figures:
#' \if{html}{\figure{gt_theme_ncaa.png}{options: width=100\%}}
#'
#' @examples
#' \dontrun{
#' library(gt)
#' gt(head(mtcars)) %>% gt_theme_ncaa()
#' }
#'
#' @import gt
#' @importFrom magrittr %>%
#' @export
gt_theme_ncaa <- function(gt_object,
                          density = c("comfortable", "compact", "social"),
                          ...) {

  .check_gt(gt_object)

  table_id <- subset(gt_object[['_options']], parameter == 'table_id')$value[[1]]

  if (is.na(table_id)) {
    table_id <- gt::random_id()
    opt_position <- which("table_id" %in% gt_object[["_options"]][["parameter"]])[[1]]
    gt_object[["_options"]][["value"]][[opt_position]] <- table_id
  }

  gt_object %>%
    # cell body
    gt::tab_style(locations = gt::cells_body(),
                  style = gt::cell_text(font = gt::google_font('Open Sans'), size = px(14))) %>%
    # col. headers
    gt::tab_style(locations = gt::cells_column_labels(),
                  style = list(
                    gt::cell_text(font = gt::google_font('Open Sans'), size = px(14),
                                        transform = "uppercase", color = "white", align = "left"),
                    gt::cell_fill(color = "#000000"))
                  ) %>%
    # group rows
    gt::tab_style(locations = gt::cells_row_groups(),
                  style = gt::cell_text(weight = 'bold', font = gt::google_font('Open Sans'), size = px(14))) %>%
    # footnote
    gt::tab_style(locations = gt::cells_footnotes(),
                  style = gt::cell_text(font = gt::google_font('Open Sans'), size = px(12))) %>%
    # title
    gt::tab_style(locations = gt::cells_title('title'),
                  style = gt::cell_text(weight = 'bold', font = gt::google_font('Open Sans'), size = px(18))) %>%
    # subtitle
    gt::tab_style(locations = gt::cells_title('subtitle'),
                  style = gt::cell_text(font = gt::google_font('Open Sans'), size = px(14))) %>%
    # caption
    gt::tab_style(locations = gt::cells_source_notes(),
                  style = gt::cell_text(font = gt::google_font('Open Sans'), size = px(10))) %>%
    gt::cols_align(columns = gt::everything(), align = "left") %>%
    gt::tab_style(
      locations = gt::cells_column_spanners(),
      style = gt::cell_text(
        font = gt::google_font("Open Sans"),
        weight = 650,
        size = px(13)
      )
    ) %>%
    gt::tab_style(
      locations = gt::cells_row_groups(),
      style = list(
        gt::cell_text(
          font = gt::google_font("Open Sans"),
          weight = 650,
          size = px(14),
          color = "#ffffff"
        ),
        gt::cell_fill(
          color = "#3C3A40"
        )
      )
    ) %>%
    gt::tab_style(
      locations = gt::cells_source_notes(),
      style = gt::cell_text(
        font = gt::google_font("Almarai"),
        size = px(12)
      )
    ) %>%
    gt::tab_style(
      locations = gt::cells_footnotes(),
      style = gt::cell_text(
        font = gt::google_font("Almarai"),
        size = px(12)
      )
    ) %>%
    # uh this is kinda hacky but it works
    tab_spanner(columns = everything(), "toss_out_spanner_dev") %>%
    gt::tab_options(
      data_row.padding = 2,
      table_body.hlines.color = "transparent",
      column_labels.border.top.color = 'black',
      column_labels.border.top.width = px(1),
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
      table_body.border.bottom.color = "white",
      table.border.bottom.style = 'none',
      table.border.top.style = 'none',
      source_notes.border.lr.style = "none",
      ...
    ) %>%
    gt::opt_row_striping() %>%
    gt::opt_css(c(paste0("#", table_id, " tbody tr:last-child {border-bottom: 2px solid #FFFFFF;}"),
                  paste0("#", table_id, " .gt_col_heading {padding: 5px 5px 5px 25px;}"),
                  paste0("#", table_id, " .gt_row {padding: 5px 5px 5px 25px;}"),
                  paste0("#", table_id, " .gt_subtitle {padding-top:0px !important; padding-bottom: 4px !important;}"),
                  paste0("#", table_id, " .gt_heading {padding-bottom: 0px; padding-top: 6px;}"),
                  paste0("#", table_id, " .gt_column_spanner {text-decoration: underline;}"),
                  paste0("#", table_id, " #toss_out_spanner_dev {display: none;}"))
              ) %>%
    .theme_scale_output(density)

}
