#' Add stacked column headers with subtitles to a `gt` table
#'
#' Replaces each column label with a two-line header: a main heading stacked over
#' a smaller subtitle. The heading and subtitle colors and font weights are set
#' through arguments, and the per-column heading and subtitle text is supplied
#' through `...`.
#'
#' @param gt_object A `gt` table object to modify.
#' @param heading_color Character. Color for the main heading text. Defaults to `"black"`.
#' @param subtitle_color Character. Color for the subtitle text. Defaults to `"#808080"`.
#' @param heading_weight Character. Font weight for the main heading. Defaults to `"bold"`.
#' @param subtitle_weight Character. Font weight for the subtitle. Defaults to `"normal"`.
#' @param heading_size Numeric. Font size of the main heading in pixels. Defaults to `14`.
#' @param subtitle_size Numeric. Font size of the subtitle in pixels. Defaults to `10`.
#' @param font Optional. A font family applied to both lines. It is set as a CSS
#'   `font-family` and is not imported, so it must be available on the machine
#'   rendering the table or loaded by the theme. Defaults to `NULL`.
#' @param ... Named arguments where each name is a column in the `gt` table and each
#'   value is a list with two elements: `heading` (the main heading) and `subtitle`
#'   (the subtitle text). A column left out of `...` uses its column name as the
#'   heading and a non-breaking space (`&nbsp;`) as the subtitle.
#' @param gt_table Deprecated. Use `gt_object`.
#'
#' @details
#' Every column in the table is relabeled, not only the ones named in `...`. The
#' loop walks the full set of column names, looks each one up in `...`, and builds
#' an HTML label holding the heading at `heading_size` over the subtitle at
#' `subtitle_size`, joined by a line break. A column with no entry in `...` falls back to its own name as the
#' heading and a non-breaking space as the subtitle, so the second line still
#' takes vertical space and the headers stay aligned. Labels are applied with
#' `gt::cols_label()`, so call this after any other label changes or they will be
#' overwritten.
#'
#' @returns Returns a modified `gt` table with stacked headers and subtitles.
#'
#' @importFrom gt cols_label
#' @importFrom glue glue
#' @importFrom rlang sym
#' @importFrom htmltools HTML
#'
#' @examples
#' \dontrun{
#' library(gt)
#'
#' mtcars %>%
#'   head() %>%
#'   gt() %>%
#'   gt_column_subheaders(
#'     mpg = list(heading = "Top", subtitle = "Bottom"),
#'     hp = list(heading = "Horsepower", subtitle = "HP"),
#'     heading_color = "blue", subtitle_color = "gray"
#'   )
#' }
#'
#' @importFrom rlang %||%
#' @export
gt_column_subheaders <- function(gt_object,
                                 heading_color = "black",
                                 subtitle_color = "#808080",
                                 heading_weight = "bold",
                                 subtitle_weight = "normal",
                                 heading_size = 14,
                                 subtitle_size = 10,
                                 font = NULL,
                                 ...,
                                 gt_table = NULL) {

  # deprecated argument, kept so old calls keep working
  if (!is.null(gt_table)) {
    cli::cli_warn(c(
      "The {.arg gt_table} argument of {.fn gt_column_subheaders} is deprecated.",
      "i" = "Use {.arg gt_object} instead."
    ))
    gt_object <- gt_table
  }

  .check_gt(gt_object)

  subheaders <- list(...)
  all_col_names <- colnames(gt_object[['_data']])
  font_css <- if (!is.null(font)) glue("font-family: '{font}';") else ""

  for (col_name in all_col_names) {

    subtitle_info <- subheaders[[col_name]] %||% list(subtitle = "&nbsp;", heading = col_name)
    subtitle <- subtitle_info$subtitle
    new_header_title <- subtitle_info$heading

    label_html <- htmltools::HTML(glue(
      "<div style='line-height: 1.05; margin-bottom: -2px;'>
        <span style='font-size: {heading_size}px; font-weight: {heading_weight}; color: {heading_color}; {font_css}'>
          {new_header_title}
        </span>
        <br>
        <span style='font-size: {subtitle_size}px; font-weight: {subtitle_weight}; color: {subtitle_color}; {font_css}'>
          {subtitle}
        </span>
      </div>"
    ))

    gt_object <- gt_object %>%
      cols_label(!!sym(col_name) := label_html)
  }

  gt_object
}
