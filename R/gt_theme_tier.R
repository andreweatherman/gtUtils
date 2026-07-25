#' Tier list theme for `gt` tables
#'
#' Oswald throughout on a near-black (`"dark"`) or white (`"light"`) ground, with
#' a bold title and every column center aligned. Rows are separated by thin black
#' borders and horizontal rules are otherwise hidden.
#'
#' @section Density:
#'
#' `density` scales the theme's type and row padding together. `"comfortable"`
#' leaves every size as the theme sets it, `"compact"` scales both down, and
#' `"social"` scales both up, to the scale [gt_save_crop()] and
#' [gt_social_crop()] export at.
#'
#' @param gt_object A `gt` table object to modify.
#' @param style Character. The color scheme, `"dark"` for a near-black ground or
#'   `"light"` for a white ground. Defaults to `"dark"`.
#' @param density Character. The type and padding scale. One of `"comfortable"`,
#'   `"compact"`, or `"social"`. See Density. Defaults to `"comfortable"`.
#' @param ... Additional arguments passed to `gt::tab_options`, applied last so
#'   they override anything the theme sets.
#'
#' @returns Returns a modified `gt` table with the theme applied.
#'
#' @details
#' Every body row except the last carries a black bottom border, and the last
#' row's border is painted in the ground color so it does not double the edge of
#' the table. Pairs with [gt_tiers()], which builds the tier rows themselves.
#'
#' @section Figures:
#' \if{html}{\figure{tier_list_example.png}{options: width=100\%}}
#'
#' @examples
#' \dontrun{
#' library(gt)
#' gt(head(mtcars)) %>% gt_theme_tier()
#' gt(head(mtcars)) %>% gt_theme_tier(style = "light")
#' }
#'
#' @import gt
#' @importFrom magrittr %>%
#' @export
gt_theme_tier <- function(gt_object, style = "dark",
                          density = c("comfortable", "compact", "social"),
                          ...) {

  .check_gt(gt_object)

  table_id <- subset(gt_object[['_options']], parameter == 'table_id')$value[[1]]

  if (is.na(table_id)) {
    table_id <- gt::random_id()
    opt_position <- which("table_id" %in% gt_object[["_options"]][["parameter"]])[[1]]
    gt_object[["_options"]][["value"]][[opt_position]] <- table_id
  }

  base_color <- if(style == "dark") "#1a1a17" else "#ffffff"
  data <- gt_object[['_data']]

  table <- gt_object %>%
    gt::opt_table_font(
      font = list(
        gt::google_font('Oswald'),
        gt::default_fonts()
      ),
      weight = 500
    ) %>%
    gt::tab_style(
      locations = gt::cells_title('title'),
      style = gt::cell_text(
        font = gt::google_font('Oswald'),
        weight = 650
      )
    ) %>%
    gt::tab_style(
      locations = gt::cells_title('subtitle'),
      style = gt::cell_text(
        font = gt::google_font('Oswald'),
        weight = 500
      )
    ) %>%
    gt::tab_style(
      locations = gt::cells_body(rows = 1:(nrow(data) - 1)),
      style = gt::cell_borders(sides = "bottom", color = "black")
    ) %>%
    gt::cols_align(
      align = 'center',
      columns = gt::everything()
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
      )
    ))

  .theme_scale_output(table, density)

}
