#' Add colored indicator boxes to a gt table
#'
#' This function takes a gt table and adds colored boxes as indicators for
#' specific columns. By default, the values in the columns are assessed based on
#' the `indicator_vals` argument, where the first value represents "no"
#' (uncolored) and the second value represents "yes" (colored). Users can also
#' define custom rules using the `indicator_rule` argument.
#'
#' @param gt_object A gt table object.
#' @param key_columns A character vector of column names that should not be
#'   transformed. Defaults to NULL.
#' @param indicator_vals A numeric vector of length 2 representing the "no" and
#'   "yes" values (defaults to c(0, 1)).
#' @param indicator_rule A function that defines the rule for when the box
#'   should be colored. Defaults to checking if the value is equal to
#'   `indicator_vals[2]`.
#' @param color_yes A character string representing the color for "yes"
#'   indicators (default is "#FCCF10").
#' @param color_no A character string representing the color for "no" indicators
#'   (default is "#EEEEEE").
#' @param show_na_as_na Logical. Whether to display NA as NA (without coloring).
#'   Defaults to FALSE, which treats NA as "NO".
#' @param show_text Logical. Whether to display the text inside the indicator
#'   boxes. Defaults to FALSE.
#' @param show_only A character string ("yes", "no", or "NA") indicating if text
#'   should only be shown for "yes", "no", or "NA" values. Defaults to NULL
#'   (text for all values is shown).
#' @param per_column_formats A list where each key is a column name, and each
#'   value is a list of formatting options (`digits`, `format_type`, `suffix`)
#'   for that column.
#' @param color_na A character string representing the color for NA indicators.
#'   If not provided, defaults to `color_no`.
#' @param border_color A character string representing the color of the border
#'   around the boxes. Defaults to NULL (no border).
#' @param border_width Numeric. The width of the border in pixels. Defaults to
#'   0.25.
#' @param box_width Numeric. The default width of the box in pixels. Defaults to
#'   20.
#' @param box_height Numeric. The default height of the box in pixels. Defaults
#'   to 20.
#'
#' @returns A modified gt table where the specified columns are replaced with
#'   HTML to display colored boxes.
#'
#' @importFrom gt text_transform cells_body
#' @importFrom glue glue
#' @importFrom purrr reduce
#' @export
gt_indicator_boxes <- function(gt_object, key_columns = NULL,
                               indicator_vals = c(0, 1),
                               indicator_rule = function(x) x == indicator_vals[2],
                               color_yes = "#FCCF10", color_no = "#EEEEEE",
                               show_na_as_na = FALSE, show_text = FALSE,
                               show_only = NULL, per_column_formats = NULL,
                               color_na = NULL, border_color = NULL, border_width = 0.25,
                               box_width = 20, box_height = 20) {

  color_na <- color_na %||% color_no
  data <- gt_object[["_data"]]
  cols_to_transform <- setdiff(names(data), key_columns)

  border_style <- if (!is.null(border_color)) {
    glue::glue("border: {border_width}px solid {border_color};")
  } else {
    ""
  }

  format_value <- function(value, digits, format_type, suffix) {
    if (!is.null(digits)) {
      value <- round(value, digits)
    }

    if (format_type == "percent") {
      value <- value * 100
    }

    formatted_value <- switch(format_type,
                              "number" = formatC(value, format = "f", digits = digits),
                              "comma" = formatC(value, format = "f", big.mark = ",", digits = digits),
                              "currency" = paste0("$", formatC(value, format = "f", big.mark = ",", digits = digits)),
                              "percent" = paste0(formatC(value, format = "f", digits = digits), "%"),
                              as.character(value))

    return(paste0(formatted_value, suffix))
  }

  gt_object <- purrr::reduce(
    cols_to_transform,
    function(tbl, col_name) {
      col_format <- per_column_formats[[col_name]] %||% list()
      col_digits <- col_format$digits %||% NULL
      col_format_type <- col_format$format_type %||% "number"
      col_suffix <- col_format$suffix %||% ""

      if (show_text) {
        column_values <- data[[col_name]]
        formatted_values <- sapply(column_values, function(x) format_value(x, col_digits, col_format_type, col_suffix))
        max_width <- max(nchar(formatted_values))
      } else {
        max_width <- box_width
      }

      tbl %>%
        text_transform(
          locations = cells_body(columns = {{ col_name }}),
          fn = function(x) {
            numeric_x <- suppressWarnings(as.numeric(x))

            color <- if (length(formals(indicator_rule)) == 2) {
              ifelse(is.na(numeric_x), color_na,
                     ifelse(indicator_rule(numeric_x, col_name), color_yes, color_no))
            } else {
              ifelse(is.na(numeric_x), color_na,
                     ifelse(indicator_rule(numeric_x), color_yes, color_no))
            }

            color[is.na(color)] <- color_no
            text_color <- gt:::ideal_fgnd_color(color)

            formatted_value <- format_value(numeric_x, col_digits, col_format_type, col_suffix)

            if (show_text) {
              if (!is.null(show_only)) {
                if (show_only == "yes") {
                  text_content <- ifelse(color == color_yes, formatted_value, "")
                } else if (show_only == "no") {
                  text_content <- ifelse(color == color_no, formatted_value, "")
                } else if (show_only == "NA") {
                  text_content <- ifelse(is.na(numeric_x) & show_na_as_na, "NA", "")
                } else {
                  text_content <- formatted_value
                }
              } else {
                text_content <- ifelse(is.na(numeric_x) & !show_na_as_na, "", formatted_value)
              }
              box_width_final <- max_width * 10
            } else {
              text_content <- ""
              box_width_final <- box_width
            }

            glue::glue("<span style='display:inline-block; width:{box_width_final}px; height:{box_height}px; line-height:{box_height}px; background-color: {color}; color: {text_color}; vertical-align:middle; margin:4px 1px; font-size: 12px; font-weight: bold; text-align:center; {border_style}'>{text_content}</span>")
          }
        )
    },
    .init = gt_object
  )

  gt_object %>%
    cols_align(
      align = "center",
      columns = all_of(cols_to_transform)
    )
}
