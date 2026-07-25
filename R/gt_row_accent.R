#' Put a colored bar on the leading edge of each row
#'
#' Draws a short color bar down the left edge of every row, keyed to a column in
#' the data such as a team, category, or brand color. It serves as a per-row key
#' without taking up a column.
#'
#' It draws the same bar [gt_spotlight()] uses for emphasis, applied per row here.
#' `gtExtras::gt_highlight_rows()` fills the whole row instead.
#'
#' @param gt_object A `gt` table object to modify.
#' @param column The column the color is keyed to. Either a column already
#'   holding colors, or a grouping column used together with `palette`.
#' @param palette Optional. Colors to map `column` onto. A named vector maps
#'   values explicitly, as `c(ACC = "#003366", SEC = "#B8232F")`. An unnamed
#'   vector is assigned across the sorted unique values and recycled. If `NULL`,
#'   `column` is taken to hold colors already. Defaults to `NULL`.
#' @param rows The rows to accent. Either an expression evaluated against the
#'   table's data, such as `conf == "ACC"` or `net <= 10`, or a numeric vector of
#'   row indices, or a single row number. If `NULL`, every row is accented.
#'   Defaults to `NULL`.
#' @param width Numeric. The bar width in pixels. Defaults to `4`.
#' @param side Character. Which edge the bar sits on, `"left"` or `"right"`.
#'   Defaults to `"left"`.
#' @param hide Logical. Should `column` be hidden once the bar is drawn? Usually
#'   what you want when it holds hex codes. Defaults to `TRUE`.
#' @param na_color Character. The color for rows where the key is missing.
#'   Defaults to `"transparent"`, which draws no bar.
#'
#' @details
#' The bar is drawn as a cell border on the leftmost rendered column, or on the
#' stub when the table has one, so it lines up with the row rather than sitting
#' inside a column of its own.
#'
#' Rows sharing a color are styled together rather than one at a time, so a long
#' table with a handful of categories stays cheap.
#'
#' @returns Returns a modified `gt` table with a color bar on each row.
#'
#' @examples
#' \dontrun{
#' library(gt)
#'
#' teams <- data.frame(
#'   team = c("Duke", "Kansas", "Auburn", "Houston"),
#'   conf = c("ACC", "B12", "SEC", "B12"),
#'   net = c(10, 20, 5, 1)
#' )
#'
#' # keyed to a grouping column
#' gt(teams) %>%
#'   gt_row_accent(conf, palette = c(ACC = "#003366", B12 = "#C8102E",
#'                                   SEC = "#B8232F"))
#'
#' # or straight from a column of colors
#' teams$color <- c("#003366", "#C8102E", "#B8232F", "#C8102E")
#' gt(teams) %>% gt_row_accent(color)
#'
#' # only some rows: by expression, by indices, or by one row number
#' gt(teams) %>% gt_row_accent(color, rows = net <= 10)
#' gt(teams) %>% gt_row_accent(color, rows = c(1, 3))
#' gt(teams) %>% gt_row_accent(color, rows = 2)
#' }
#'
#' @seealso [gt_spotlight()] for emphasis rather than a key.
#' @import gt
#' @importFrom magrittr %>%
#' @export
gt_row_accent <- function(gt_object, column, palette = NULL, rows = NULL,
                          width = 4, side = c("left", "right"), hide = TRUE,
                          na_color = "transparent") {

  .check_gt(gt_object)
  side <- match.arg(side)

  data <- gt_object[["_data"]]
  key_col <- names(dplyr::select(data, {{ column }}))
  if (length(key_col) != 1) {
    cli::cli_abort("{.arg column} must select exactly one column.")
  }

  keys <- data[[key_col]]

  colors <- if (is.null(palette)) {
    as.character(keys)
  } else if (!is.null(names(palette))) {
    unname(palette[as.character(keys)])
  } else {
    levs <- sort(unique(as.character(keys[!is.na(keys)])))
    unname(stats::setNames(rep_len(palette, length(levs)), levs)[as.character(keys)])
  }
  colors[is.na(colors)] <- na_color

  # rows: a data-masked expression, raw indices, or NULL for all
  rows_q <- rlang::enquo(rows)
  keep <- if (rlang::quo_is_null(rows_q)) {
    seq_len(nrow(data))
  } else {
    res <- rlang::eval_tidy(rows_q, data = data)
    idx <- if (is.logical(res)) which(res) else as.integer(res)
    idx[!is.na(idx) & idx >= 1 & idx <= nrow(data)]
  }

  if (!length(keep)) {
    cli::cli_warn("{.arg rows} matched no rows; returning the table unchanged.")
    return(gt_object)
  }

  # hide first, so the bar lands on whatever is leftmost afterwards
  if (isTRUE(hide)) {
    gt_object <- gt_object %>% gt::cols_hide(columns = tidyselect::all_of(key_col))
  }

  boxhead <- gt_object[["_boxhead"]]
  has_stub <- any(boxhead[["type"]] == "stub")
  edge_col <- boxhead[["var"]][boxhead[["type"]] == "default"][[1]]

  if (is.na(edge_col) && !has_stub) return(gt_object)

  # one style per distinct color rather than one per row
  for (col in unique(colors)) {
    if (identical(col, "transparent")) next
    at <- intersect(which(colors == col), keep)
    if (!length(at)) next
    loc <- if (has_stub) {
      gt::cells_stub(rows = at)
    } else {
      gt::cells_body(columns = tidyselect::all_of(edge_col), rows = at)
    }
    gt_object <- gt_object %>%
      gt::tab_style(
        style = gt::cell_borders(sides = side, color = col, weight = gt::px(width)),
        locations = loc
      )
  }

  gt_object
}
