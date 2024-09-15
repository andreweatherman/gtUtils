#' Add a border grid to a `gt` table
#'
#' This function adds column and row borders to a `gt` table, creating a
#' grid-like appearance. It can optionally include borders for column and row
#' labels.
#'
#' @param gt_object A `gt` table object to modify.
#' @param color A character string representing the color of the borders
#'   (default is `"black"`).
#' @param weight A numeric value specifying the thickness of the borders in
#'   pixels (default is `1`).
#' @param include_labels Logical. Whether to include borders around row and
#'   column labels (default is `FALSE`).
#'
#' @details The `gt_border_grid` function adds a grid to a `gt` table by
#' applying column and row borders. It uses the `gt_add_divider` function from
#' `gtExtras` to add column borders and applies custom CSS for row borders. The
#' function also generates a random table ID if one is not provided.
#'
#' @return A `gt` table object with the grid borders applied.
#'
#' @importFrom gt opt_css
#' @importFrom gtExtras gt_add_divider
#' @export
gt_border_grid <- function(gt_object,
                           color = "black",
                           weight = 1,
                           include_labels = FALSE) {

  table_id <- subset(gt_object[['_options']], parameter == 'table_id')$value[[1]]

  if (is.na(table_id)) {
    table_id <- gt::random_id()
    opt_position <- which("table_id" %in% gt_object[["_options"]][["parameter"]])[[1]]
    gt_object[["_options"]][["value"]][[opt_position]] <- table_id
  }

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

