#' Set one font family across a whole `gt` table
#'
#' Applies a single font family to every styleable part of a `gt` table in one
#' call: the title and subtitle, the stubhead, column spanners and labels, row
#' groups, the stub, the body, footnotes, and source notes. Setting the same font
#' through `gt::tab_options()` means naming each `*_font` option in turn.
#'
#' @param gt_object A `gt` table object to modify.
#' @param font_family Character. The font family to apply to the whole table.
#' @param from_google_font Logical. Should the font be pulled from Google Fonts
#'   through `gt::google_font()`? `FALSE` treats `font_family` as a font already
#'   installed on the local machine. Defaults to `TRUE`.
#' @param weight The font weight applied to every part, passed to
#'   `gt::cell_text()`. Either a keyword such as `"bold"` or a numeric weight.
#'   Defaults to `NULL`, which leaves the weight alone.
#' @param style Character. The font style applied to every part, one of
#'   `"normal"`, `"italic"`, or `"oblique"`. Defaults to `NULL`, which leaves the
#'   style alone.
#' @param gt_table Deprecated. Use `gt_object`.
#'
#' @details
#' The font is applied with a single `gt::tab_style()` over a list of cell
#' locations covering each part of the table. Summary and grand-summary cells are
#' not included, so a font set here does not reach them. When `from_google_font`
#' is `TRUE` the family is wrapped in `gt::google_font()`, which adds the import
#' so the font renders in an exported table without relying on it being installed
#' locally.
#'
#' @returns Returns a modified `gt` table with the font family applied to every
#'   covered part.
#'
#' @examples
#' \dontrun{
#' library(gt)
#'
#' gt(head(mtcars)) %>% gt_set_font("Oswald")
#'
#' # use a font already installed locally
#' gt(head(iris)) %>% gt_set_font("Helvetica", from_google_font = FALSE)
#' }
#'
#' @export
gt_set_font <- function(gt_object,
                        font_family,
                        from_google_font = TRUE,
                        weight = NULL,
                        style = NULL,
                        gt_table = NULL) {

  # deprecated argument, kept so old calls keep working
  if (!is.null(gt_table)) {
    cli::cli_warn(c(
      "The {.arg gt_table} argument of {.fn gt_set_font} is deprecated.",
      "i" = "Use {.arg gt_object} instead."
    ))
    gt_object <- gt_table
  }

  .check_gt(gt_object)

  family <- if (isTRUE(from_google_font)) gt::google_font(font_family) else font_family
  cell <- gt::cell_text(font = family, weight = weight, style = style)

  gt_object %>%
    gt::tab_style(
      style = cell,
      locations = list(
        gt::cells_title(),
        gt::cells_stubhead(),
        gt::cells_column_spanners(spanners = gt::everything()),
        gt::cells_column_labels(columns = gt::everything()),
        gt::cells_row_groups(groups = gt::everything()),
        gt::cells_stub(rows = gt::everything()),
        gt::cells_body(columns = gt::everything()),
        #gt::cells_summary(columns = gt::everything(), rows = gt::everything(), groups = gt::everything()),
        #gt::cells_grand_summary(columns = gt::everything(), rows = gt::everything()),
        #gt::cells_stub_summary(rows = gt::everything(), groups = gt::everything()),
        #gt::cells_stub_grand_summary(rows = gt::everything()),
        gt::cells_footnotes(),
        gt::cells_source_notes()
      )
    )
}
