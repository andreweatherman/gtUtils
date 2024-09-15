#' Color Rows in gt Table Based on Win/Loss Results
#'
#' This function styles rows in a `gt` table based on win/loss
#' results. It allows customization of colors and text for win and loss rows.
#' Users can specify the column with result data, customize colors, and choose
#' between traditional win/loss or binary result types.
#'
#' @param gt_object A gt table object.
#' @param result_column The name of the column in `data` that contains the
#'   result indicators. Default is 'result'.
#' @param win_color The background color for rows indicating a win. Default is
#'   '#5DA271' (green).
#' @param loss_color The background color for rows indicating a loss. Default is
#'   '#C84630' (red).
#' @param wins_text_color The text color for rows indicating a win. Default is
#'   'white'.
#' @param loss_text_color The text color for rows indicating a loss. Default is
#'   'white'.
#' @param result_type The type of result indicators used in the `result_column`.
#'   Options are 'wl' (default, for 'W' and 'L') or 'binary' (for 1 and 0).
#'
#' @return A `gt` table with styled rows based on the win/loss results.
#'
#' @export
gt_color_results <- function(gt_object,
                             result_column = 'result',
                             win_color = '#5DA271',
                             loss_color = '#C84630',
                             wins_text_color = 'white',
                             loss_text_color = 'white',
                             result_type = 'wl') {

  if(result_type == 'binary') {
    win_condition <- 1
    loss_condition <- 0
  } else {
    win_condition <- 'W'
    loss_condition <- 'L'
  }

  data <- gt_object[['_data']]

  gt_object %>%
    gt::tab_style(
      locations = gt::cells_body(rows = which(data[[result_column]] == win_condition)),
      style = list(gt::cell_fill(color = win_color), gt::cell_text(color = wins_text_color))
    ) %>%
    gt::tab_style(
      locations = gt::cells_body(rows = which(data[[result_column]] == loss_condition)),
      style = list(gt::cell_fill(color = loss_color), gt::cell_text(color = loss_text_color))
    )

}
