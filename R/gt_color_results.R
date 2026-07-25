#' Color rows of a `gt` table by win, loss, or tie result
#'
#' Fills and recolors each row according to a result read from a column. Results
#' can be encoded as `"W"` and `"L"` or as `1` and `0`, with an optional tie
#' state. Rows holding none of the values are left unchanged.
#'
#' @param gt_object A `gt` table object to modify.
#' @param result_column The column holding the result indicators, as a bare
#'   column name or a string. Defaults to `"result"`.
#' @param win_color Character. The background fill for winning rows. Defaults to
#'   `"#5DA271"`.
#' @param loss_color Character. The background fill for losing rows. Defaults to
#'   `"#C84630"`.
#' @param tie_color Optional. The background fill for tie rows. When `NULL`, ties
#'   are not colored. Defaults to `NULL`.
#' @param wins_text_color Character. The text color for winning rows. Defaults to
#'   `"white"`.
#' @param loss_text_color Character. The text color for losing rows. Defaults to
#'   `"white"`.
#' @param tie_text_color Character. The text color for tie rows. Defaults to
#'   `"white"`.
#' @param tie_value The value in `result_column` marking a tie, used when
#'   `tie_color` is set. Defaults to `"T"`.
#' @param result_type Character. The encoding of `result_column`. Either `"wl"`
#'   for `"W"` and `"L"`, or `"binary"` for `1` and `0`. Defaults to `"wl"`.
#'
#' @details
#' A `gt::tab_style()` pass fills and recolors the body rows whose
#' `result_column` equals the win value, then the loss value, and then, when
#' `tie_color` is set, the `tie_value`. Under `result_type = "wl"` the win and
#' loss values compared are `"W"` and `"L"`; under `"binary"` they are `1` and
#' `0`. Any row matching none of the values keeps its existing styling.
#'
#' @returns Returns a modified `gt` table with winning and losing rows colored.
#'
#' @examples
#' \dontrun{
#' library(gt)
#'
#' results <- data.frame(
#'   game = paste("Game", 1:4),
#'   pts = c(88, 74, 102, 65),
#'   result = c("W", "L", "W", "L")
#' )
#'
#' gt(results) %>% gt_color_results()
#'
#' # binary encoding, with custom colors
#' results$result <- c(1, 0, 1, 0)
#' gt(results) %>%
#'   gt_color_results(result_type = "binary", win_color = "#1B7837",
#'                    loss_color = "#762A83")
#' }
#'
#' @export
gt_color_results <- function(gt_object,
                             result_column = 'result',
                             win_color = '#5DA271',
                             loss_color = '#C84630',
                             tie_color = NULL,
                             wins_text_color = 'white',
                             loss_text_color = 'white',
                             tie_text_color = 'white',
                             tie_value = 'T',
                             result_type = 'wl') {

  .check_gt(gt_object)

  if (result_type == 'binary') {
    win_condition <- 1
    loss_condition <- 0
  } else {
    win_condition <- 'W'
    loss_condition <- 'L'
  }

  data <- gt_object[['_data']]
  # accepts a bare column, a string, or any tidyselect that lands on one column
  result_column <- tryCatch(
    names(dplyr::select(data, {{ result_column }})),
    error = function(e) {
      cli::cli_abort(c(
        "{.arg result_column} must select one column in the table.",
        "x" = conditionMessage(e)
      ), call = rlang::caller_env(4))
    }
  )
  if (length(result_column) != 1) {
    cli::cli_abort(c(
      "{.arg result_column} must select exactly one column.",
      "x" = "It selected {length(result_column)} column{?s}."
    ))
  }
  col <- data[[result_column]]

  gt_object <- gt_object %>%
    gt::tab_style(
      locations = gt::cells_body(rows = which(col == win_condition)),
      style = list(gt::cell_fill(color = win_color), gt::cell_text(color = wins_text_color))
    ) %>%
    gt::tab_style(
      locations = gt::cells_body(rows = which(col == loss_condition)),
      style = list(gt::cell_fill(color = loss_color), gt::cell_text(color = loss_text_color))
    )

  # tie rows are only colored when a tie color is supplied
  if (!is.null(tie_color)) {
    gt_object <- gt_object %>%
      gt::tab_style(
        locations = gt::cells_body(rows = which(col == tie_value)),
        style = list(gt::cell_fill(color = tie_color), gt::cell_text(color = tie_text_color))
      )
  }

  gt_object

}
