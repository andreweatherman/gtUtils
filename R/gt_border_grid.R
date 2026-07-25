#' Add a border grid to a `gt` table
#'
#' Draws borders between every column and every row of a `gt` table, giving it a
#' full grid, with an option to extend the borders around the column and row
#' labels.
#'
#' @param gt_object A `gt` table object to modify.
#' @param color Character. The border color. Defaults to `"black"`.
#' @param weight Numeric. The border thickness in pixels. Defaults to `1`.
#' @param include_labels Logical. Should the borders extend around the row and
#'   column labels? Defaults to `FALSE`.
#'
#' @details
#' Column borders are drawn with `gtExtras::gt_add_divider()` on every column but
#' the last, and the row borders are added as scoped CSS on the `.gt_row` top
#' border. A table id is resolved or generated first, since that CSS is keyed on
#' `#<table_id>`.
#'
#' @returns Returns a modified `gt` table with the grid borders applied.
#'
#' @examples
#' \dontrun{
#' library(gt)
#'
#' gt(head(mtcars)) %>% gt_border_grid()
#'
#' # heavier gray lines, including around the labels
#' gt(head(iris)) %>%
#'   gt_border_grid(color = "#BBBBBB", weight = 2, include_labels = TRUE)
#' }
#'
#' @importFrom gt opt_css
#' @importFrom gtExtras gt_add_divider
#' @export
gt_border_grid <- function(gt_object,
                           color = "black",
                           weight = 1,
                           include_labels = FALSE) {

  .check_gt(gt_object)

  res <- .table_id(gt_object)
  gt_object <- res$object
  table_id <- res$id

  gt_object %>%
    gtExtras::gt_add_divider(columns = -dplyr::last_col(),
                             color = color,
                             weight = px(weight),
                             include_labels = include_labels) %>%
    gt::opt_css(
      paste0("#", table_id, " .gt_row { border-top-color: ", color, ";}"),
      add = TRUE
    )
}

