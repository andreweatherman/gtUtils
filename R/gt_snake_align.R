#' Reshape a parallel frame to match a snaked table
#'
#' Lays a data frame out in side-by-side blocks the same way [gt_snake()] lays
#' out the table, so a frame you computed against the long data still lines up
#' after the snake. It is the escape hatch for a precomputed grid you do not want
#' to rebuild as a condition: a logical mask for [gt_highlight_cells()], a frame
#' of per-cell colors or text, anything the same shape as the snaked body.
#'
#' Most of the time you do not need this. Body-cell styling applied before
#' [gt_snake()] is carried through the reshape on its own, so coloring the long
#' grid and then snaking already lands the fills in the right place. Reach for
#' this only when the grid lives outside the table's styles.
#'
#' @param x A data frame or matrix with one row per row of the un-snaked table.
#' @param n_cols Integer. The number of blocks, matching the [gt_snake()] call.
#'   Defaults to `2`.
#' @param rows_per_col Integer. Rows in each block. Given this, `n_cols` is
#'   worked out from the data instead, exactly as [gt_snake()] does it. Defaults
#'   to `NULL`.
#' @param fill What to put in the trailing cells when the rows do not divide
#'   evenly. Defaults to `NA`, which `gt_highlight_cells()` reads as no match.
#'
#' @details
#' Pass the same `n_cols` (or `rows_per_col`) you passed to [gt_snake()]. Each
#' block's columns are suffixed with its block number, `_1`, `_2`, and so on, to
#' match the names [gt_snake()] gives the table. The result has
#' `ceiling(nrow(x) / n_cols)` rows and `ncol(x) * n_cols` columns, so it is the
#' shape [gt_highlight_cells()] expects for the block of snaked columns.
#'
#' @returns Returns a data frame of the reshaped blocks.
#'
#' @examples
#' \dontrun{
#' library(gt)
#'
#' wide <- head(mtcars[c("mpg", "hp", "wt", "qsec")], 8)
#'
#' # a logical grid the same shape as the data, built before the snake
#' mask <- wide > 20
#'
#' # after the snake, each column carries its block number as a suffix
#' snaked <- unlist(lapply(1:2, function(i) paste0(names(wide), "_", i)))
#'
#' wide %>%
#'   gt() %>%
#'   gt_snake(n_cols = 2) %>%
#'   gt_highlight_cells(tidyselect::all_of(snaked),
#'                      condition = gt_snake_align(mask, n_cols = 2),
#'                      fill = "#EDBD68")
#' }
#'
#' @seealso [gt_snake()], which reshapes the table itself and carries body
#'   styling through.
#' @export
gt_snake_align <- function(x, n_cols = 2, rows_per_col = NULL, fill = NA) {

  x <- as.data.frame(x)
  n <- nrow(x)

  if (!is.null(rows_per_col)) {
    per <- as.integer(rows_per_col)
    if (per < 1) cli::cli_abort("{.arg rows_per_col} must be at least 1.")
    n_cols <- ceiling(n / per)
  } else {
    n_cols <- as.integer(n_cols)
    if (n_cols < 1) cli::cli_abort("{.arg n_cols} must be at least 1.")
    per <- ceiling(n / n_cols)
  }

  if (n_cols < 2 || n == 0) return(x)

  do.call(cbind, lapply(seq_len(n_cols), function(i) {
    idx <- (i - 1) * per + seq_len(per)
    block <- x[idx, , drop = FALSE]
    block[idx > n, ] <- fill
    names(block) <- paste0(names(x), "_", i)
    rownames(block) <- NULL
    block
  }))
}
