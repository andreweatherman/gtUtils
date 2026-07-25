#' Bold rows in a `gt` table
#'
#' Bolds the body cells of chosen rows, with an option to recolor their text and
#' fill their background. Rows are chosen by index or by a filter expression; with
#' neither, every row is bolded.
#'
#' @param gt_object A `gt` table object to modify.
#' @param rows The rows to bold. Either an expression evaluated against the
#'   table's data, such as `mpg > 20`, or a numeric vector of row indices. If
#'   `NULL`, every row is bolded. Defaults to `NULL`.
#' @param row Deprecated. Use `rows`.
#' @param text_color Character. The text color for the bolded rows. Defaults to
#'   `"black"`.
#' @param highlight_color Character. The background fill for the bolded rows. Set
#'   to `NULL` for no fill. Defaults to `NULL`.
#' @param filter_statement Deprecated. Use `rows`, which takes the expression
#'   directly rather than as a string.
#'
#' @details
#' When `filter_statement` is supplied it is parsed and evaluated against the
#' table's underlying data, and the rows it matches are bolded. A `row` vector
#' takes over when no filter is given. The styling is applied with a single
#' `gt::tab_style()` over `gt::cells_body()`, so it covers every column of the
#' chosen rows.
#'
#' @returns Returns a modified `gt` table with the chosen rows bolded.
#'
#' @examples
#' \dontrun{
#' library(gt)
#'
#' # bold every row
#' gt(head(mtcars)) %>% gt_bold_rows()
#'
#' # bold and fill the rows above 20 mpg
#' gt(head(mtcars)) %>%
#'   gt_bold_rows(filter_statement = "mpg > 20", highlight_color = "#FFF3B0")
#' }
#'
#' @export
gt_bold_rows <- function(gt_object,
                         rows = NULL,
                         text_color = "black",
                         highlight_color = NULL,
                         row = NULL,
                         filter_statement = NULL) {

  .check_gt(gt_object)

  # extract data
  data <- gt_object[["_data"]]

  if (!is.null(row)) {
    lifecycle_warn("row", "rows")
    rows <- row
  }
  if (!is.null(filter_statement)) {
    lifecycle_warn("filter_statement", "rows")
  }

  rows_q <- rlang::enquo(rows)
  if (!rlang::quo_is_null(rows_q) && is.null(filter_statement)) {
    res <- rlang::eval_tidy(rows_q, data = data)
    idx <- if (is.logical(res)) which(res) else as.integer(res)
    idx <- idx[!is.na(idx) & idx >= 1 & idx <= nrow(data)]
    if (!length(idx)) {
      cli::cli_warn("{.arg rows} matched no rows; returning the table unchanged.")
      return(gt_object)
    }
    return(.bold_rows_apply(gt_object, idx, text_color, highlight_color))
  }

  rows_to_change <- if (!is.null(filter_statement)) {
    which(eval(parse(text = filter_statement), envir = data))
  } else {
    seq_len(nrow(data))
  }

  .bold_rows_apply(gt_object, rows_to_change, text_color, highlight_color)
}

# bold text, and a fill only when a highlight color is given
.bold_rows_apply <- function(gt_object, rows, text_color, highlight_color) {
  style <- list(gt::cell_text(color = text_color, weight = "bold"))
  if (!is.null(highlight_color)) {
    style <- c(list(gt::cell_fill(color = highlight_color)), style)
  }
  gt_object %>%
    gt::tab_style(
      locations = gt::cells_body(rows = rows),
      style = style
    )
}

# one warning per session per argument, so a loop does not spam
lifecycle_warn <- function(old, new) {
  key <- paste0("gtUtils_deprecated_", old)
  if (isTRUE(getOption(key))) return(invisible())
  options(stats::setNames(list(TRUE), key))
  cli::cli_warn(c(
    "{.arg {old}} is deprecated.",
    "i" = "Use {.arg {new}}, which takes an expression such as {.code mpg > 20} \\
           or a vector of row indices."
  ))
}
