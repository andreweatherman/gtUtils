#' Add stacked column headers with subtitles to a `gt` table
#'
#' This function creates stacked column headers in a `gt` table, where each column has a
#' customizable main heading and subtitle, along with options to set the colors and
#' font weights for both the heading and subtitle.
#'
#' @param gt_table A `gt` table object to modify.
#' @param heading_color Character. Color for the main heading text. Defaults to `"black"`.
#' @param subtitle_color Character. Color for the subtitle text. Defaults to `"#808080"`.
#' @param heading_weight Character. Font weight for the main heading. Defaults to `"bold"`.
#' @param subtitle_weight Character. Font weight for the subtitle. Defaults to `"normal"`.
#' @param ... Named arguments where each name corresponds to a column in the `gt` table
#'   and each value is a list containing two elements: `heading` (the main heading) and
#'   `subtitle` (the subtitle text). If a column is not included, it will use the column
#'   name as the heading and display a non-breaking space (`&nbsp;`) as the subtitle.
#'
#' @details
#' The function iterates over the columns of the `gt` table and applies a two-level header
#' for each column: a customizable main heading and a smaller, customizable subtitle. If no
#' subtitle or heading is provided for a column, the column name is used as the heading and
#' a default non-breaking space is used as the subtitle.
#'
#' @returns Returns a modified `gt` table with stacked headers and subtitles.
#'
#' @importFrom gt cols_label
#' @importFrom glue glue
#' @importFrom rlang sym
#' @importFrom htmltools HTML
#'
#' @examples
#' # mtcars %>%
#' #   head() %>%
#' #   gt() %>%
#' #   gt_column_subheaders(
#' #     mpg = list(heading = "Top", subtitle = "Bottom"),
#' #     hp = list(heading = "Horsepower", subtitle = "HP"),
#' #     heading_color = "blue", subtitle_color = "gray"
#' #   )
#'
#' @export
gt_column_subheaders <- function(gt_table,
                                 heading_color = "black",
                                 subtitle_color = "#808080",
                                 heading_weight = "bold",
                                 subtitle_weight = "normal",
                                 ...) {

  subheaders <- list(...)
  all_col_names <- colnames(gt_table[['_data']])

  for (col_name in all_col_names) {

    subtitle_info <- subheaders[[col_name]] %||% list(subtitle = "&nbsp;", heading = col_name)
    subtitle <- subtitle_info$subtitle
    new_header_title <- subtitle_info$heading

    label_html <- htmltools::HTML(glue(
      "<div style='line-height: 1.05; margin-bottom: -2px;'>
        <span style='font-size: 14px; font-weight: {heading_weight}; color: {heading_color};'>
          {new_header_title}
        </span>
        <br>
        <span style='font-size: 10px; font-weight: {subtitle_weight}; color: {subtitle_color};'>
          {subtitle}
        </span>
      </div>"
    ))

    gt_table <- gt_table %>%
      cols_label(!!sym(col_name) := label_html)
  }

  gt_table
}
