#' Fill individual cells in a `gt` table by a condition
#'
#' Fills each cell in a block of columns that meets a condition and leaves the
#' rest alone. It is the per-cell counterpart to `gtExtras::gt_highlight_cols()`
#' and `gtExtras::gt_highlight_rows()`, which fill a whole column or a whole row.
#' The condition is tested against every cell on its own, so a scattered set of
#' cells can be filled in one call.
#'
#' A single `gt::tab_style()` with `gt::cells_body()` cannot do this, since it
#' reads `rows` and `columns` as a rectangle, every selected row crossed with
#' every selected column. Filling a diagonal, a checker pattern, or any scatter
#' otherwise takes a loop over the columns, one pass each. This wraps that loop.
#'
#' Each selected column is filled independently, so the pattern can be a
#' diagonal, a checker, or any scatter of cells.
#'
#' @param gt_object A `gt` table object to modify.
#' @param columns The block of columns to test, the body of the grid.
#' @param condition How a cell is chosen. Either a function or a right-hand-side
#'   formula applied to each column and returning a logical vector (`~ .x > 0.7`,
#'   `~ grepl("home", .x)`, `~ is.na(.x)`), or a logical matrix or data frame the
#'   same shape as `columns` for a mask computed ahead of time.
#' @param fill Character. The cell fill color. Defaults to `"#FFF3B0"`, a soft
#'   yellow.
#' @param text_color Optional. A hex color for the text in filled cells. Defaults
#'   to `NULL`, which leaves the text color alone.
#' @param bold Logical. Should filled cells be bolded? Defaults to `FALSE`.
#' @param ... Additional arguments passed to `gt::cell_text()` for the filled
#'   cells, such as `style = "italic"`.
#'
#' @details
#' The condition reads the underlying data rather than the rendered cell, so a
#' test like `~ grepl("home", .x)` sees whatever value the column holds, HTML and
#' all. A formula or function must return one logical per row, and `NA` counts as
#' not matched. For more than one color, layer the calls, one per color.
#'
#' @returns Returns a modified `gt` table with the matching cells filled.
#'
#' @examples
#' \dontrun{
#' library(gt)
#'
#' # a correlation matrix: flag strong pairs, but not the diagonal of ones
#' m <- round(cor(mtcars[, c("mpg", "disp", "hp", "wt", "qsec")]), 2)
#' cordf <- data.frame(var = rownames(m), m, row.names = NULL, check.names = FALSE)
#' cordf %>%
#'   gt(rowname_col = "var") %>%
#'   gt_highlight_cells(-var, ~ .x > 0.7 & .x < 1, fill = "#FFD1A9") %>%
#'   gt_highlight_cells(-var, ~ .x < -0.7, fill = "#A9D0FF")
#'
#' # a schedule matrix: home games one color, byes another
#' sched <- data.frame(
#'   team = c("Alpha", "Bravo", "Charlie"),
#'   wk1 = c("vs X", "@ Y", "BYE"),
#'   wk2 = c("@ W", "vs V", "@ U")
#' )
#' gt(sched) %>%
#'   gt_highlight_cells(c(wk1, wk2), ~ grepl("^vs", .x), fill = "#CCE7F5") %>%
#'   gt_highlight_cells(c(wk1, wk2), ~ .x == "BYE", fill = "#D9D9D9")
#' }
#'
#' @seealso `gtExtras::gt_highlight_cols()` and `gtExtras::gt_highlight_rows()`
#'   for whole-column and whole-row fills.
#' @import gt
#' @importFrom magrittr %>%
#' @export
gt_highlight_cells <- function(gt_object, columns, condition, fill = "#FFF3B0",
                               text_color = NULL, bold = FALSE, ...) {

  .check_gt(gt_object)

  data <- gt_object[["_data"]]
  cols <- names(dplyr::select(data, {{ columns }}))
  if (!length(cols)) {
    cli::cli_abort("{.arg columns} matched no columns.")
  }

  # resolve the condition to one logical vector per selected column
  if (is.matrix(condition) || is.data.frame(condition)) {
    mask <- as.data.frame(condition)
    if (ncol(mask) != length(cols)) {
      cli::cli_abort(c(
        "A logical {.arg condition} must have one column per selected column.",
        "x" = "Got {ncol(mask)} column{?s} for {length(cols)} selected column{?s}."
      ))
    }
    names(mask) <- cols
    mask[] <- lapply(mask, function(z) !is.na(z) & as.logical(z))
  } else {
    test <- rlang::as_function(condition)
    mask <- lapply(cols, function(cn) {
      out <- tryCatch(
        test(data[[cn]]),
        error = function(e) {
          cli::cli_abort(c(
            "{.arg condition} could not be applied to column {.val {cn}}.",
            "x" = conditionMessage(e),
            "i" = "Select only the columns the condition fits, e.g. drop a text \\
                   label column with {.code -label} or name the grid columns."
          ))
        }
      )
      if (length(out) != nrow(data)) {
        cli::cli_abort("{.arg condition} must return one value per row.")
      }
      !is.na(out) & as.logical(out)
    })
    names(mask) <- cols
  }

  # the fill, plus any text treatment
  text_args <- list(...)
  if (!is.null(text_color)) text_args$color <- text_color
  if (isTRUE(bold)) text_args$weight <- "bold"
  styles <- list(gt::cell_fill(color = fill))
  if (length(text_args)) styles <- c(styles, list(do.call(gt::cell_text, text_args)))

  # one pass per column. a single tab_style() fills the whole rectangle
  for (cn in cols) {
    rows <- which(mask[[cn]])
    if (!length(rows)) next
    gt_object <- gt_object %>%
      gt::tab_style(
        style = styles,
        locations = gt::cells_body(columns = tidyselect::all_of(cn), rows = rows)
      )
  }

  gt_object
}
