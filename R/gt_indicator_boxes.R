#' Add colored indicator boxes to a `gt` table
#'
#' Replaces column values with colored boxes, filled when a value meets a rule
#' and left neutral otherwise. By default the box is colored when a value equals
#' the second element of `indicator_vals` (`1`) and left `color_no` when it equals
#' the first (`0`); supply `indicator_rule` for any other test. Name the columns
#' to convert with `columns`, or the ones to leave alone with `key_columns`.
#'
#' @param gt_object A `gt` table object to modify.
#' @param columns The columns to convert to boxes, using tidyselect. Defaults to
#'   `NULL`, which converts every column not named in `key_columns`.
#' @param key_columns The columns to leave untouched, using tidyselect. Every
#'   other column is converted. Kept for the inverted way of saying the same
#'   thing; give this or `columns`, not both. Defaults to `NULL`.
#' @param indicator_vals Numeric. A length-2 vector giving the "no" and "yes"
#'   values. Defaults to `c(0, 1)`.
#' @param indicator_rule A function deciding when a box is colored. It receives
#'   the column values, and optionally the column name as a second argument, and
#'   returns a logical vector. Defaults to testing equality with
#'   `indicator_vals[2]`.
#' @param color_yes Character. The fill for boxes meeting the rule. Defaults to
#'   `"#FCCF10"`.
#' @param color_no Character. The fill for boxes not meeting the rule. Defaults to
#'   `"#EEEEEE"`.
#' @param show_na_as_na Logical. Should `NA` be shown as `NA` rather than treated
#'   as "no"? Defaults to `FALSE`.
#' @param show_text Logical. Should the formatted value be printed inside the box?
#'   Defaults to `FALSE`.
#' @param show_only Character. Restrict printed text to one class of box, one of
#'   `"yes"`, `"no"`, or `"NA"`. Defaults to `NULL`, which prints text for all.
#' @param per_column_formats A named list keyed by column name, each element a
#'   list of formatting options (`digits`, `format_type`, `suffix`) for that
#'   column. Defaults to `NULL`.
#' @param color_na Character. The fill for `NA` boxes. Defaults to `NULL`, which
#'   uses `color_no`.
#' @param border_color Character. The border color around the boxes. Defaults to
#'   `NULL`, no border.
#' @param border_width Numeric. The border width in pixels. Defaults to `0.25`.
#' @param box_width Numeric. The box width in pixels, used when `show_text` is
#'   `FALSE`. Defaults to `20`.
#' @param box_height Numeric. The box height in pixels. Defaults to `20`.
#' @param text_size Numeric. The font size of the box text in pixels, used when
#'   `show_text` is `TRUE`. Defaults to `12`.
#' @param text_weight Character. The font weight of the box text. Defaults to
#'   `"bold"`.
#'
#' @details
#' Every column outside `key_columns` is replaced with an HTML span through
#' `gt::text_transform()`, and the transformed columns are then center-aligned.
#' The rule is applied to the numeric coercion of each column, so text values
#' become `NA`; `NA` cells take `color_na` (falling back to `color_no`) unless
#' `show_na_as_na` keeps them labeled `NA`. Box text is set to black or white,
#' whichever measures higher contrast against the fill.
#'
#' When `show_text` is `TRUE` the box widens to fit the widest formatted value in
#' the column; otherwise it is fixed at `box_width`. `indicator_rule` may accept a
#' second argument, the column name, which allows a different test per column.
#'
#' @returns Returns a modified `gt` table with the converted columns shown as
#'   colored boxes.
#'
#' @importFrom gt text_transform cells_body
#' @importFrom glue glue
#'
#' @examples
#' \dontrun{
#' library(gt)
#'
#' roster <- data.frame(
#'   player = c("A", "B", "C"),
#'   starter = c(1, 0, 1),
#'   injured = c(0, 0, 1),
#'   captain = c(1, 0, 0)
#' )
#'
#' gt(roster) %>% gt_indicator_boxes(key_columns = "player")
#'
#' # print the underlying values and draw a border
#' gt(roster) %>%
#'   gt_indicator_boxes(key_columns = "player", show_text = TRUE,
#'                      border_color = "#333333")
#' }
#'
#' @export
gt_indicator_boxes <- function(gt_object, columns = NULL, key_columns = NULL,
                               indicator_vals = c(0, 1),
                               indicator_rule = function(x) x == indicator_vals[2],
                               color_yes = "#FCCF10", color_no = "#EEEEEE",
                               show_na_as_na = FALSE, show_text = FALSE,
                               show_only = NULL, per_column_formats = NULL,
                               color_na = NULL, border_color = NULL, border_width = 0.25,
                               box_width = 20, box_height = 20,
                               text_size = 12, text_weight = "bold") {

  .check_gt(gt_object)

  color_na <- color_na %||% color_no
  data <- gt_object[["_data"]]

  # `columns` names what to convert, matching the rest of the package.
  # `key_columns` names what to leave alone, which is the older inverted form.
  cols_q <- rlang::enquo(columns)
  keys_q <- rlang::enquo(key_columns)
  if (!rlang::quo_is_null(cols_q) && !rlang::quo_is_null(keys_q)) {
    cli::cli_abort(c(
      "Give either {.arg columns} or {.arg key_columns}, not both.",
      "i" = "{.arg columns} names the columns to convert to boxes.",
      "i" = "{.arg key_columns} names the columns to leave alone."
    ))
  }
  cols_to_transform <- if (!rlang::quo_is_null(cols_q)) {
    names(dplyr::select(data, !!cols_q))
  } else if (!rlang::quo_is_null(keys_q)) {
    setdiff(names(data), names(dplyr::select(data, !!keys_q)))
  } else {
    names(data)
  }
  if (!length(cols_to_transform)) {
    cli::cli_abort("No columns left to convert to boxes.")
  }

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

    # no digits: format naturally, so a whole number does not gain trailing zeros
    big <- if (format_type %in% c("comma", "currency")) "," else ""
    core <- if (is.null(digits)) {
      format(value, trim = TRUE, big.mark = big, scientific = FALSE)
    } else {
      formatC(value, format = "f", big.mark = big, digits = digits)
    }

    formatted_value <- switch(format_type,
                              "currency" = paste0("$", core),
                              "percent" = paste0(core, "%"),
                              core)

    return(paste0(formatted_value, suffix))
  }

  gt_object <- Reduce(
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
            text_color <- .theme_on_color(color)

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

            glue::glue("<span style='display:inline-block; width:{box_width_final}px; height:{box_height}px; line-height:{box_height}px; background-color: {color}; color: {text_color}; vertical-align:middle; margin:4px 1px; font-size: {text_size}px; font-weight: {text_weight}; text-align:center; {border_style}'>{text_content}</span>")
          }
        )
    },
    cols_to_transform,
    init = gt_object
  )

  gt_object %>%
    cols_align(
      align = "center",
      columns = all_of(cols_to_transform)
    )
}
