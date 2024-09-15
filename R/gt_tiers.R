#' Apply tier-based background colors and bold styling to a `gt` table
#'
#' This function applies background colors to rows based on tier levels and bolds
#' the tier column in a `gt` table. It iterates over the specified levels and
#' applies the corresponding colors to the rows that match each tier. The text color
#' for the `tier` column is determined using `gt:::ideal_fgnd_color` for better contrast.
#'
#' @param gt_object A `gt` table object to modify.
#' @param levels A character vector specifying the tier levels. These should match
#'   the factor levels in the `tier` column of the table.
#' @param colors A character vector of hex color codes corresponding to the tier levels.
#'   The length of `colors` must match the length of `levels`.
#'
#' @details
#' The function applies background colors based on the `levels` vector to the rows where
#' the `tier` column matches the level. It also bolds the text in the `tier` column for
#' clearer differentiation and uses `gt:::ideal_fgnd_color` to determine the appropriate
#' text color for the background.
#'
#' @returns Returns a modified `gt` table with tier-based background colors and bold text applied.
#'
#' @import gt
#' @export
gt_tiers <- function(gt_object, levels, colors, style = "dark", img_height = "55px") {

  fill_colors <- set_names(colors, levels)

  purrr::reduce(levels, function(gt_object, level) {
    bg_color <- fill_colors[[level]]
    text_color <- gt:::ideal_fgnd_color(bg_color)

    gt_object %>%
      tab_style(
        style = cell_fill(color = bg_color),
        locations = cells_body(
          rows = tier == !!level,
          columns = tier
        )
      ) %>%
      gt_theme_tier(style = "dark") %>%
      fmt_image(-tier, height = img_height) %>%
      sub_missing(missing_text = "") %>%
      cols_label(everything() ~ "") %>%
      tab_style(
        style = list(
          cell_text(weight = "bold", color = text_color)
        ),
        locations = cells_body(columns = tier)
      )
  }, .init = gt_object)
}
