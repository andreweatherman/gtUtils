#' Add a computed change column to a `gt` table
#'
#' Takes two numeric columns and inserts a new column holding the change between
#' them, signed and colored by direction. The change is `to` minus `from`, so a
#' later period as `to` and an earlier one as `from` gives a positive number when
#' the value has grown. The arithmetic, the formatting, and the sign coloring
#' happen in one call.
#'
#' `gt::fmt_number(force_sign = TRUE)` formats a difference you have already
#' worked out, and `gtExtras::gt_fa_rank_change()` handles movement in a rank
#' column. This computes the delta from two columns, formats it, and colors it by
#' sign together.
#'
#' @param gt_object A `gt` table object to modify.
#' @param from The starting column, a single numeric column.
#' @param to The ending column, a single numeric column. The change is
#'   `to - from`.
#' @param column_label Character. The label for the new column. Defaults to
#'   `"Change"`.
#' @param percent Logical. Should the change be shown as a percent of `from`
#'   rather than an absolute difference? Defaults to `FALSE`.
#' @param decimals Integer. The number of decimal places. Defaults to `1`.
#' @param arrows Logical. Should an up or down triangle lead the value in place
#'   of a sign? Defaults to `FALSE`.
#' @param color Logical. Should the values be colored by direction? Defaults to
#'   `TRUE`.
#' @param color_positive Character. The color for an increase. Defaults to a
#'   green, `"#1B7837"`.
#' @param color_negative Character. The color for a decrease. Defaults to a red,
#'   `"#B2182B"`.
#' @param color_neutral Optional. The color for no change. Defaults to `NULL`,
#'   which leaves zero the table's normal text color.
#' @param force_sign Logical. Should a plus be shown on an increase? Ignored when
#'   `arrows` is `TRUE`, since the arrow carries the direction. Defaults to `TRUE`.
#' @param after Optional. The column the new one is placed after, a position or a
#'   name. Defaults to `NULL`, which places it after `to`.
#'
#' @details
#' A row is left blank where either `from` or `to` is missing, and where a
#' percent change divides by a `from` of zero. When `arrows` is `TRUE` the value
#' is shown as a magnitude behind the triangle, so a decrease reads as a down
#' triangle in front of a positive number.
#'
#' @returns Returns a modified `gt` table with the change column added.
#'
#' @examples
#' \dontrun{
#' library(gt)
#'
#' revenue <- data.frame(
#'   Segment = c("Hardware", "Software", "Services"),
#'   Q1 = c(482, 331, 198),
#'   Q2 = c(515, 302, 246)
#' )
#'
#' # absolute change, colored by sign
#' gt(revenue) %>% gt_delta(Q1, Q2)
#'
#' # as a percent of Q1, with arrows
#' gt(revenue) %>% gt_delta(Q1, Q2, percent = TRUE, arrows = TRUE)
#' }
#'
#' @seealso [gt_scale_note()] for disclosing a divided scale.
#' @import gt
#' @importFrom magrittr %>%
#' @export
gt_delta <- function(gt_object, from, to, column_label = "Change",
                     percent = FALSE, decimals = 1, arrows = FALSE,
                     color = TRUE, color_positive = "#1B7837",
                     color_negative = "#B2182B", color_neutral = NULL,
                     force_sign = TRUE, after = NULL) {

  .check_gt(gt_object)

  data <- gt_object[["_data"]]
  from_col <- names(dplyr::select(data, {{ from }}))
  to_col <- names(dplyr::select(data, {{ to }}))

  if (length(from_col) != 1 || length(to_col) != 1) {
    cli::cli_abort("{.arg from} and {.arg to} must each select a single column.")
  }

  from_vals <- suppressWarnings(as.numeric(data[[from_col]]))
  to_vals <- suppressWarnings(as.numeric(data[[to_col]]))

  # to - from, positive means it grew
  delta <- to_vals - from_vals
  if (isTRUE(percent)) {
    delta <- (to_vals - from_vals) / from_vals
    delta[!is.finite(delta)] <- NA_real_
  }

  # the arrow carries direction, so drop the sign and format the magnitude
  signs <- sign(delta)
  mag <- if (isTRUE(arrows)) abs(delta) else delta
  signed <- force_sign && !isTRUE(arrows)

  body <- if (isTRUE(percent)) {
    gt::vec_fmt_percent(mag, decimals = decimals, force_sign = signed)
  } else {
    gt::vec_fmt_number(mag, decimals = decimals, force_sign = signed)
  }

  if (isTRUE(arrows)) {
    arrow <- ifelse(signs > 0, "\u25B2", ifelse(signs < 0, "\u25BC", ""))
    body <- ifelse(nzchar(arrow), paste0(arrow, " ", body), body)
  }

  # blank where there is no change to show
  body[is.na(delta)] <- ""

  new_name <- make.unique(c(names(data), column_label))[length(names(data)) + 1]
  after_col <- if (is.null(after)) to_col else {
    if (is.character(after)) after else names(data)[[as.integer(after)]]
  }

  # cols_add(.after =) errors on the last column, so append instead
  gt_object <- if (identical(after_col, names(data)[[length(names(data))]])) {
    rlang::inject(gt::cols_add(gt_object, "{new_name}" := !!body))
  } else {
    rlang::inject(gt::cols_add(gt_object, "{new_name}" := !!body, .after = !!after_col))
  }
  if (!identical(new_name, column_label)) {
    gt_object <- do.call(
      gt::cols_label, c(list(gt_object), stats::setNames(list(column_label), new_name))
    )
  }
  gt_object <- gt_object %>%
    gt::cols_align(align = "right", columns = tidyselect::all_of(new_name))

  if (isTRUE(color)) {
    paint <- function(gt, rows, col) {
      if (!length(rows) || is.null(col)) return(gt)
      gt %>% gt::tab_style(
        style = gt::cell_text(color = col),
        locations = gt::cells_body(columns = tidyselect::all_of(new_name), rows = rows)
      )
    }
    gt_object <- gt_object %>%
      paint(which(delta > 0), color_positive) %>%
      paint(which(delta < 0), color_negative) %>%
      paint(which(delta == 0), color_neutral)
  }

  gt_object
}
