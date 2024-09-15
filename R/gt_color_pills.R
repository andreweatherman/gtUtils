#' Add color pills to a `gt` table column
#'
#' This function adds color pills to a specified column in a `gt` table, allowing
#' the column's values to be filled with color based on ordinal ranking or continuous
#' values. You can provide a custom domain and palette, with optional `paletteer` support.
#'
#' @param gt_object A `gt` table object to modify.
#' @param columns A column or set of columns within the `gt` table to apply the color pills.
#' @param palette A color palette to use. If you want to use a palette from `paletteer`,
#'   specify the palette as `package::palette`. Defaults to `c("#C84630", "#5DA271")`.
#' @param fill_type Character. Either `"rank"` or `"continuous"`, determining whether to fill
#'   based on ordinal rank or continuous values.
#' @param rank_order Character. Either `"asc"` or `"desc"`, used when `fill_type` is `"rank"`.
#'   Defaults to `"desc"`.
#' @param digits Integer. The number of decimal places to round the values to. Defaults to `NULL`.
#' @param domain A numeric vector specifying the domain for continuous color scaling.
#'   If `NULL`, the domain will be automatically determined from the data. Defaults to `NULL`.
#' @param format_type Character. The format of the values in the column. Options are `"number"`,
#'   `"comma"`, `"currency"`, or `"percent"`. Defaults to `"number"`.
#' @param scale_percent Logical. Should the values be scaled to percent (multiplied by 100)? Defaults to `TRUE`.
#' @param suffix Optional. A character string to append as a suffix to the formatted value (e.g., "M", "K", "lbs"). Defaults to `""`.
#' @param reverse Logical. Should the color palette be reversed? Defaults to `FALSE`.
#' @param outline_color Optional. A hex color code for the border around the pills. Defaults to `NULL`.
#' @param outline_width Numeric. The width of the border around the pills in pixels. Defaults to `0.25`.
#' @param pal_type Character. Either `"discrete"` or `"continuous"`, used when applying
#'   `paletteer` palettes. Defaults to `"discrete"`.
#' @param pill_height Numeric. The height of each color pill in pixels. Defaults to `25`.
#' @param ... Additional arguments passed to `scales::col_numeric`.
#'
#' @import scales
#' @import gt
#' @import glue
#' @import paletteer
#' @importFrom magrittr %>%
#'
#' @returns Returns a modified `gt` table with color pills applied to the specified columns.
#' @export
gt_color_pills <- function(gt_object, columns, palette = c("#C84630", "#5DA271"),
                           fill_type = "continuous", rank_order = "desc",
                           digits = NULL, domain = NULL, format_type = "number",
                           scale_percent = TRUE, suffix = "", reverse = FALSE,
                           outline_color = NULL, outline_width = 0.25,
                           pal_type = "discrete", pill_height = 25, ...) {

  stopifnot(`Table must be of class 'gt_tbl'` = "gt_tbl" %in% class(gt_object))

  data <- gt_object[["_data"]]

  column_data <- data[[rlang::as_string(rlang::ensym(columns))]]

  if (fill_type == "rank") {
    ranked_column <- rank(column_data, na.last = "keep", ties.method = "average")

    if (rank_order == "desc") {
      ranked_column <- max(ranked_column, na.rm = TRUE) - ranked_column + 1
    }
  } else {
    ranked_column <- column_data
  }

  if (is.null(domain)) {
    domain <- range(ranked_column, na.rm = TRUE)
    warning("Domain not specified, defaulting to observed range within the specified column.", call. = FALSE)
  }

  pal <- if (grepl(x = palette[1], pattern = "::")) {
    paletteer::paletteer_d(palette = palette, direction = if (reverse) -1 else 1, type = pal_type) %>% as.character()
  } else {
    if (reverse) rev(palette) else palette
  }

  format_value <- function(value, digits, format_type) {
    if (format_type == "percent" && scale_percent) {
      value <- value * 100
    }

    if (!is.null(digits)) {
      value <- round(value, digits)
    }

    formatted_value <- switch(format_type,
                              "number" = formatC(value, format = "f", digits = digits),
                              "comma" = formatC(value, format = "f", big.mark = ",", digits = digits),
                              "currency" = paste0("$", formatC(value, format = "f", big.mark = ",", digits = digits)),
                              "percent" = paste0(formatC(value, format = "f", digits = digits), "%"),
                              as.character(value))

    return(paste0(formatted_value, suffix))
  }

  formatted_values <- sapply(column_data, function(x) format_value(x, digits, format_type))
  max_width <- max(nchar(formatted_values))

  generate_pill_html <- function(value, rank_value) {
    color <- scales::col_numeric(palette = pal, domain = domain)(rank_value)
    text_color <- gt:::ideal_fgnd_color(color)
    formatted_value <- format_value(as.numeric(value), digits, format_type)

    outline_style <- if (!is.null(outline_color)) glue::glue("border: {outline_width}px solid {outline_color};") else ""

    glue::glue("<span style='display: inline-block; width: {max_width}ch; padding-left: 3px; padding-right: 3px; height: {pill_height}px; line-height: {pill_height}px; background-color: {color}; color: {text_color}; border-radius: 10px; text-align: center; {outline_style}'>{formatted_value}</span>")
  }

  gt_object %>%
    text_transform(
      locations = cells_body(columns = {{ columns }}),
      fn = function(x) {
        mapply(generate_pill_html, x, ranked_column)
      }
    )
}
