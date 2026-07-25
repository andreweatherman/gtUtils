#' Add a 538-style caption to a `gt` table
#'
#' Adds a caption block beneath the table with a rule under its top line, in the
#' style of FiveThirtyEight tables. The `top_caption` renders as a footnote with
#' a border drawn under it, and the `bottom_caption` renders as a right-aligned
#' source note below it. Both accept markdown.
#'
#' @param gt_object A `gt` table object to modify.
#' @param top_caption Character. Text rendered as a footnote beneath the table,
#'   with a rule drawn under it. Accepts markdown. Defaults to `NULL`, which
#'   draws the rule with no text above it.
#' @param bottom_caption Character. Text rendered as a source note below the top
#'   caption, aligned by `align`. Accepts markdown. Defaults to `NULL`, which
#'   shows only the top caption.
#' @param rule_color Optional. A hex color for the rule under the top caption.
#'   When `NULL`, the color is taken from the rendered table so it tracks the
#'   theme. Defaults to `NULL`.
#' @param rule_width Numeric. The rule width in pixels. Defaults to `1`.
#' @param size Numeric. The font size of the top caption in pixels. Defaults to
#'   `12`.
#' @param align Character. The alignment of the bottom caption. Defaults to
#'   `"right"`.
#' @param ... Additional arguments. Currently unused.
#'
#' @details
#' The top caption is attached with `gt::tab_footnote()` on the column labels,
#' which places it in the table footer. Scoped CSS then hides the footnote mark
#' and draws a bottom border under the footnote, so it reads as a ruled caption
#' rather than a numbered note. When `rule_color` is `NULL` the border color is
#' taken from the first text color found in the rendered table (via
#' `gt:::as.tags.gt_tbl()`), so it tracks the theme across light and dark color
#' modes, and falls back to a neutral gray when the render carries no color. The
#' rules are keyed on the table id. If the table has none, one is generated and
#' assigned.
#'
#' Apply the theme before this function. The rule color is read off the rendered
#' table, so calling it first on a dark theme borrows a near-black (`#333333`)
#' and the rule disappears into the background. Pass `rule_color` to sidestep the
#' question entirely.
#'
#' @returns Returns a modified `gt` table with the styled captions.
#'
#' @examples
#' \dontrun{
#' library(gt)
#'
#' gt(head(mtcars[c("mpg", "hp", "wt")], 6)) %>%
#'   gt_538_caption(
#'     top_caption = "Fuel economy and power",
#'     bottom_caption = "Source: *1974 Motor Trend* road tests"
#'   )
#' }
#'
#' @import gt
#' @importFrom magrittr %>%
#'
#' @export
gt_538_caption <- function(gt_object, top_caption = NULL, bottom_caption = NULL,
                           rule_color = NULL, rule_width = 1, size = 12,
                           align = "right", ...) {

  .check_gt(gt_object)

  if (is.null(top_caption) && is.null(bottom_caption)) {
    cli::cli_abort(c(
      "Nothing to caption.",
      "i" = "Pass {.arg top_caption}, {.arg bottom_caption}, or both.",
      "i" = "{.arg top_caption} sits above the rule, {.arg bottom_caption} below it."
    ))
  }

  ## grab footnote text color to use for diff. color modes
  extract_color_hex <- function(css_string) {
    m <- regmatches(css_string,
                    gregexpr("(?<=color:\\s)#[A-Fa-f0-9]{6}", css_string, perl = TRUE))
    unique(unlist(m))
  }

  # rule color: the one passed, else the first text color in the rendered table,
  # else a neutral gray when the render carries no color to borrow
  auto_color <- extract_color_hex(as.character(gt:::as.tags.gt_tbl(gt_object)))
  footnote_color <- if (!is.null(rule_color)) {
    rule_color
  } else if (length(auto_color)) {
    auto_color[[1]]
  } else {
    "#8A8A8A"
  }

  res <- .table_id(gt_object)
  gt_object <- res$object
  table_id <- res$id

  table <- gt_object
  if (!is.null(top_caption)) {
    table <- table %>%
      gt::tab_footnote(locations = gt::cells_column_labels(),
                       footnote = gt::md(top_caption))
  }

  table <- table %>%
    gt::opt_css(c(
      paste0("#", table_id, " .gt_footnote {
              border-bottom-style: solid;
              border-bottom-width: ", rule_width, "px;
              border-bottom-color: ", footnote_color, ";
              font-size: ", size, "px;}"),
      paste0("#", table_id, " .gt_footnote_marks {display: none !important;}"),
      paste0("#", table_id, " .gt_sourcenote {text-align: ", align, ";}")
    ))

  if (!is.null(bottom_caption)) {
    table <- table %>%
      gt::tab_source_note(source_note = gt::md(bottom_caption))
  }

  return(table)
}
