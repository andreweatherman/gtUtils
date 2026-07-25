#' Combine count columns into a single cell in a `gt` table
#'
#' Merges two or more count columns into one `"32-5"` style cell, optionally with
#' one of them shown as a share of the row total. Any set of tallies works, from
#' wins and losses to tests passed and failed.
#'
#' The counts are written into the first column and the rest are hidden, or the
#' last one is reused to carry the share, so the table ends up narrower than it
#' started.
#'
#' `gt::cols_merge()` will join columns with a pattern and
#' `gt::cols_merge_n_pct()` will pair a count with a percentage column you have
#' already built. The difference here is that the share is computed for you from
#' the counts themselves.
#'
#' @param gt_object A `gt` table object to modify.
#' @param columns The count columns to combine, in the order they should read.
#'   Two or more.
#' @param separator Character. The string placed between the counts. Defaults to
#'   `"-"`.
#' @param label Optional. A new label for the combined column. Defaults to
#'   `NULL`, which leaves the existing label alone.
#' @param share Logical. Should one of the counts be shown as a share of the row
#'   total? Defaults to `FALSE`.
#' @param share_of The column the share is computed for, either a position or a
#'   name. Defaults to `1`, the first column.
#' @param share_location Character. Where the share goes. Either `"inline"` to
#'   append it to the combined cell, or `"column"` to reuse the last of `columns`
#'   for it. Defaults to `"inline"`.
#' @param share_decimals Integer. The number of decimal places for the share.
#'   Defaults to `1`.
#' @param share_label Character. The label for the share column when
#'   `share_location` is `"column"`. Defaults to `"%"`.
#' @param share_prefix Character. The string placed before an inline share.
#'   Defaults to `" ("`.
#' @param share_suffix Character. The string placed after an inline share.
#'   Defaults to `")"`.
#' @param ... Additional arguments passed to `gt::vec_fmt_percent`.
#'
#' @details
#' A row is left alone if any of its counts are missing, so a partial tally is
#' never shown. The share is also left blank where the counts sum to zero.
#'
#' @returns Returns a modified `gt` table with the counts combined.
#'
#' @examples
#' \dontrun{
#' library(gt)
#'
#' suites <- data.frame(
#'   Suite = c("Parser", "Renderer", "Exporter"),
#'   Passed = c(142, 98, 211),
#'   Failed = c(8, 2, 17)
#' )
#'
#' # renders as "142-8"
#' gt(suites) %>% gt_fmt_tally(c(Passed, Failed), label = "Result")
#'
#' # renders as "142-8 (94.7%)"
#' gt(suites) %>% gt_fmt_tally(c(Passed, Failed), share = TRUE)
#'
#' # the failure rate instead, in its own column
#' gt(suites) %>%
#'   gt_fmt_tally(c(Passed, Failed), share = TRUE, share_of = "Failed",
#'                share_location = "column", share_label = "Fail rate")
#'
#' # three counts, as in a league table
#' league <- data.frame(
#'   Club = c("Arsenal", "Chelsea"),
#'   W = c(26, 18), D = c(6, 10), L = c(6, 10)
#' )
#'
#' gt(league) %>% gt_fmt_tally(c(W, D, L), label = "W-D-L")
#' }
#'
#' @seealso [gt_fmt_rank()] for ordinal formatting.
#' @import gt
#' @importFrom magrittr %>%
#' @export
gt_fmt_tally <- function(gt_object, columns, separator = "-", label = NULL,
                         share = FALSE, share_of = 1,
                         share_location = c("inline", "column"),
                         share_decimals = 1, share_label = "%",
                         share_prefix = " (", share_suffix = ")", ...) {

  .check_gt(gt_object)
  share_location <- match.arg(share_location)

  data <- gt_object[["_data"]]
  cols <- names(dplyr::select(data, {{ columns }}))

  if (length(cols) < 2) {
    cli::cli_abort("{.arg columns} must select at least two columns.")
  }

  vals <- lapply(cols, function(nm) suppressWarnings(as.numeric(data[[nm]])))
  names(vals) <- cols

  # one at a time, or format() pads them to a common width
  as_text <- function(x) {
    vapply(x, function(v) {
      if (is.na(v)) NA_character_ else format(v, trim = TRUE, scientific = FALSE)
    }, character(1))
  }

  parts <- lapply(vals, as_text)
  tally <- do.call(paste, c(parts, sep = separator))

  # a partial tally is worse than none
  incomplete <- Reduce(`|`, lapply(vals, is.na))
  tally[incomplete] <- NA_character_

  share_str <- NULL
  if (isTRUE(share)) {
    pos <- if (is.character(share_of)) match(share_of, cols) else as.integer(share_of)
    if (is.na(pos) || pos < 1 || pos > length(cols)) {
      cli::cli_abort("{.arg share_of} must name or index one of {.val {cols}}.")
    }
    total <- Reduce(`+`, vals)
    prop <- vals[[pos]] / total
    prop[!is.finite(prop)] <- NA_real_
    share_str <- gt::vec_fmt_percent(prop, decimals = share_decimals, ...)
    share_str[is.na(prop)] <- NA_character_
  }

  display <- if (isTRUE(share) && share_location == "inline") {
    ifelse(is.na(tally) | is.na(share_str), tally,
           paste0(tally, share_prefix, share_str, share_suffix))
  } else {
    tally
  }

  relabel <- function(gt, col, lab) {
    do.call(gt::cols_label, c(list(gt), stats::setNames(list(lab), col)))
  }

  # the tally goes in the first column
  gt_object <- gt_object %>%
    gt::text_transform(
      locations = gt::cells_body(columns = tidyselect::all_of(cols[[1]])),
      fn = function(x) ifelse(is.na(display), x, display)
    )

  # the last column either carries the share or gets hidden along with the rest
  if (isTRUE(share) && share_location == "column") {
    carrier <- cols[[length(cols)]]
    gt_object <- gt_object %>%
      gt::text_transform(
        locations = gt::cells_body(columns = tidyselect::all_of(carrier)),
        fn = function(x) ifelse(is.na(share_str), x, share_str)
      )
    gt_object <- relabel(gt_object, carrier, share_label)
    spent <- cols[-c(1, length(cols))]
  } else {
    spent <- cols[-1]
  }

  if (length(spent)) {
    gt_object <- gt_object %>% gt::cols_hide(columns = tidyselect::all_of(spent))
  }

  if (!is.null(label)) {
    gt_object <- relabel(gt_object, cols[[1]], label)
  }

  gt_object
}
