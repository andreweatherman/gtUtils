#' Break long column labels across lines in a `gt` table
#'
#' Wraps a column label onto several short lines so a wide heading stops forcing a
#' narrow column wider than its data needs. "Strength of Schedule" over a
#' two-digit column becomes three stacked lines instead of one long one.
#'
#' The hand-rolled version is `gt::cols_label(x = gt::html("A<br>B"))`, written
#' out per column. This wraps at a width for you, and by default balances the
#' lines so they come out close to even rather than a long first line over a
#' short last one.
#'
#' @param gt_object A `gt` table object to modify.
#' @param columns The columns whose labels should wrap. Defaults to
#'   `gt::everything()`.
#' @param width Integer. The target line length in characters. Defaults to `12`.
#' @param balance Logical. Should the lines be evened out rather than filled
#'   greedily left to right? Defaults to `TRUE`.
#'
#' @details
#' A label of one word, or one already shorter than `width`, is left alone. The
#' wrap is on whitespace only, so a single long word is never split.
#'
#' @returns Returns a modified `gt` table with the selected labels wrapped.
#'
#' @examples
#' \dontrun{
#' library(gt)
#'
#' scores <- data.frame(
#'   name = c("Item A", "Item B"),
#'   sos = c(0.62, 0.48),
#'   adj = c(112.4, 98.1)
#' )
#'
#' gt(scores) %>%
#'   cols_label(sos = "Strength of Schedule", adj = "Adjusted Efficiency") %>%
#'   gt_wrap_labels(c(sos, adj), width = 10)
#' }
#'
#' @import gt
#' @importFrom magrittr %>%
#' @export
gt_wrap_labels <- function(gt_object, columns = gt::everything(), width = 12,
                           balance = TRUE) {

  .check_gt(gt_object)

  data <- gt_object[["_data"]]
  cols <- names(dplyr::select(data, {{ columns }}))
  if (!length(cols)) return(gt_object)

  boxhead <- gt_object[["_boxhead"]]

  # even the lines out: fill toward the mean length, not the max
  wrap_balanced <- function(words, width) {
    greedy <- strwrap(paste(words, collapse = " "), width = width)
    k <- length(greedy)
    if (k <= 1) return(paste(words, collapse = " "))
    target <- ceiling(sum(nchar(words) + 1) / k)
    lines <- character(0)
    cur <- ""
    for (w in words) {
      cand <- if (nzchar(cur)) paste(cur, w) else w
      if (nchar(cand) > target && nzchar(cur)) {
        lines <- c(lines, cur)
        cur <- w
      } else {
        cur <- cand
      }
    }
    if (nzchar(cur)) lines <- c(lines, cur)
    lines
  }

  labels <- list()
  for (col in cols) {
    pos <- match(col, boxhead$var)
    lab <- if (!is.na(pos)) boxhead$column_label[[pos]] else NULL
    text <- if (is.null(lab) || length(lab) == 0 || is.na(lab[[1]])) col else as.character(lab)[[1]]

    words <- strsplit(text, "\\s+")[[1]]
    if (length(words) <= 1) next  # nothing to wrap

    lines <- if (isTRUE(balance)) wrap_balanced(words, width) else strwrap(text, width = width)
    if (length(lines) <= 1) next

    labels[[col]] <- gt::html(paste(lines, collapse = "<br>"))
  }

  if (length(labels)) {
    gt_object <- do.call(gt::cols_label, c(list(gt_object), labels))
  }
  gt_object
}
