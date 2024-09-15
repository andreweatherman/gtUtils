#' Bold rows in a GT table with optional filtering and highlighting
#'
#' `gt_bold_rows` applies bold styling to rows of a GT table. You can specify rows to bold through a
#' filter statement or by default, all rows are bolded. There is also an option to change the text
#' color and add a background highlight color to the bolded rows.
#'
#' @param gt_object A GT table object.
#' @param row A vector of row indices to bold, default is all rows.
#' @param text_color The color to be used for the text, default is 'black'.
#' @param highlight_color The color to be used for the row background highlight, default is 'white'.
#'        If `NULL`, no background color will be applied.
#' @param filter_statement A character string that contains a filter expression which is evaluated
#'        to determine which rows to bold. It should be a valid R expression as a string,
#'        for example, `"column_name > 5"`. Default is `NULL`, which means no filter is applied.
#'
#' @return A GT table object with the specified rows styled in bold text, and optionally with
#'         changed text color and background highlight.
#'
#' @export
gt_bold_rows <- function(gt_object,
                         row = NULL,
                         text_color = "black",
                         highlight_color = NULL,
                         filter_statement = NULL) {

  # extract data
  data <- gt_object[["_data"]]

  # get rows that correspond to filter statement if one is given
  rows_to_change <- if (!is.null(filter_statement)) {
    which(eval(parse(text = filter_statement), envir = data))
  } else if (!is.null(row)) {
    row
  } else {
    1:nrow(data)
  }

  highlight_color <- ifelse(is.null(highlight_color), "white", highlight_color)

  # apply styles
  table <- gt_object %>%
    gt::tab_style(
      locations = gt::cells_body(rows = rows_to_change),
      style = list(gt::cell_fill(color = highlight_color), gt::cell_text(
        color = text_color,
        weight = "bold"
      ))
    )

  return(table)

}
