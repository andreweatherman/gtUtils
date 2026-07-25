#' Style a column as margin notes
#'
#' Styles a text column as commentary instead of data. It applies a muted
#' color, italics, a hairline separating the notes from the figures, a blank
#' column label, and a width constraint so the prose wraps instead of stretching
#' the table.
#'
#' @param gt_object A `gt` table object to modify.
#' @param columns The column or columns to treat as margin notes.
#' @param width Numeric. The column width in pixels, which is what makes the prose
#'   wrap. Pass `NULL` to leave the width alone. Defaults to `220`.
#' @param label Character. The column label. Defaults to `""`, since a note column
#'   rarely needs one; pass a string to keep a heading.
#' @param italic Logical. Should the notes be italicized? Defaults to `TRUE`.
#' @param color Optional. A hex color for the text. If `NULL`, a muted color is
#'   derived from the table background and checked for legibility. Defaults to
#'   `NULL`.
#' @param size Character. The font size, as a CSS size. Defaults to `"0.92em"`,
#'   slightly smaller than the body and scaling with whatever size the theme sets.
#' @param rule Logical. Should a hairline be drawn on the left edge, separating
#'   the notes from the data? Defaults to `TRUE`.
#' @param rule_color Optional. A hex color for that hairline. Derived from the
#'   table background when `NULL`. Defaults to `NULL`.
#' @param align Character. The text alignment. Defaults to `"left"`.
#'
#' @details
#' A text column with no width set runs to a single long line, so `width` is what
#' wraps the prose. On a three-row example, that is the difference between a table
#' 625 pixels wide and one 482 pixels wide.
#'
#' Colors are derived from the table background instead of being hard-coded, so
#' the treatment also reads correctly on a dark theme. That means the theme has
#' to be applied first. An unthemed table reports a white background, and the
#' notes come out too dark to read once a dark theme lands on top. Pass `color`
#' and `rule_color` to fix them regardless of order.
#'
#' @returns Returns a modified `gt` table with the selected columns styled as
#'   margin notes.
#'
#' @examples
#' \dontrun{
#' library(gt)
#'
#' quarterly <- data.frame(
#'   Line = c("Revenue", "Cost of sales", "Operating expenses"),
#'   Actual = c(4820, 2110, 1360),
#'   Budget = c(4500, 2000, 1400),
#'   Comment = c("Enterprise renewals landed a quarter early.",
#'               "Freight costs above plan; contract renegotiated in Q3.",
#'               "Headcount hiring paused from February.")
#' )
#'
#' gt(quarterly) %>%
#'   gt_theme_broadsheet() %>%
#'   gt_marginalia(Comment)
#'
#' # keep a heading, widen it, and drop the italics
#' gt(quarterly) %>%
#'   gt_marginalia(Comment, label = "Commentary", width = 280, italic = FALSE)
#' }
#'
#' @seealso [gt_cutline()] for a labeled break between rows.
#' @import gt
#' @importFrom magrittr %>%
#' @export
gt_marginalia <- function(gt_object, columns, width = 220, label = "",
                          italic = TRUE, color = NULL, size = "0.92em",
                          rule = TRUE, rule_color = NULL, align = "left") {

  .check_gt(gt_object)

  cols_quo <- rlang::enquo(columns)
  col_names <- names(dplyr::select(gt_object[["_data"]], !!cols_quo))
  if (!length(col_names)) {
    cli::cli_abort("{.arg columns} matched no columns.")
  }

  # muted tone off the table background, so dark themes work
  opt <- gt_object[["_options"]]
  bg <- opt$value[opt$parameter == "table_background_color"]
  bg <- if (!length(bg) || is.na(bg[[1]])) "#FFFFFF" else as.character(bg[[1]])
  ink <- .theme_on_color(bg)
  if (is.null(color)) color <- .theme_secondary_on(bg, ink, target = 4.5)
  if (is.null(rule_color)) rule_color <- .theme_mix(ink, bg, 0.18)

  out <- gt_object %>%
    gt::cols_align(align = align, columns = !!cols_quo) %>%
    gt::tab_style(
      style = gt::cell_text(
        color = color, size = size,
        style = if (isTRUE(italic)) "italic" else "normal"
      ),
      locations = gt::cells_body(columns = !!cols_quo)
    )

  if (!is.null(width)) {
    # cols_width() wants two-sided formulas
    w <- if (is.numeric(width)) gt::px(width) else width
    out <- out %>%
      gt::cols_width(.list = lapply(col_names, function(cn) {
        rlang::new_formula(rlang::sym(cn), w)
      }))
  }

  if (!is.null(label)) {
    out <- out %>%
      gt::cols_label(.list = stats::setNames(as.list(rep(label, length(col_names))), col_names))
  }

  if (isTRUE(rule)) {
    out <- out %>%
      gt::tab_style(
        style = gt::cell_borders(sides = "left", color = rule_color, weight = gt::px(1)),
        locations = gt::cells_body(columns = !!cols_quo)
      )
  }

  out
}
