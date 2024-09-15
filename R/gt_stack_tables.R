#' Stack multiple `gt` tables
#'
#' This function takes multiple `gt` tables and stacks them vertically into a single HTML output.
#' The stacked tables are displayed in the viewer.
#'
#' @param tables A list of `gt` table objects to stack.
#'
#' @details
#' This function stacks multiple `gt` tables vertically in a single HTML block and displays the stacked tables
#' in the viewer.
#'
#' @return Displays the stacked tables in the viewer.
#'
#' @importFrom htmltools div browsable
#' @export
gt_stack_tables <- function(tables = NULL) {

  stacked_tables <- htmltools::div(
    lapply(tables, function(table) {
      htmltools::div(table, style = "display: block; width: 100%;")
    })
  )

  htmltools::browsable(stacked_tables)
}
