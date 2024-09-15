#' 538 Caption Theme
#'
#' Adds a top-line caption below the table with a line underneath like
#' 538 tables. The `top_caption` is rendered as a footnote, and `bottom_caption`
#' is rendered as a source note with markdown support.
#'
#' @returns A `gt_tbl` with the styled captions
#' @param gt_object An existing gt table object of class `gt_tbl`.
#' @param top_caption The caption to render as a footnote (can be markdown).
#' @param bottom_caption The caption to render as a source note (can be markdown).
#' @param ... Optional additional arguments to `gt::table_options()`.
#'
#' @import gt
#' @importFrom magrittr %>%
#'
#' @export
gt_538_caption <- function(gt_object, top_caption, bottom_caption = NULL, ...) {

  stopifnot(`'gt_object' must be a 'gt_tbl', have you accidentally passed raw data?` = "gt_tbl" %in%
              class(gt_object))

  table_id <- subset(gt_object[['_options']], parameter == 'table_id')$value[[1]]

  ## grab footnote text color to use for diff. color modes
  extract_color_hex <- function(css_string) {
    stringr::str_extract_all(css_string, "(?<=color:\\s)#([A-Fa-f0-9]{6})") %>%
      unlist() %>%
      unique()
  }

  footnote_color <- extract_color_hex(as.character(gt:::as.tags.gt_tbl(gt_object)))[[1]]

  if (is.na(table_id)) {
    table_id <- gt::random_id()
    opt_position <- which("table_id" %in% gt_object[["_options"]][["parameter"]])[[1]]
    gt_object[["_options"]][["value"]][[opt_position]] <- table_id
  }

  table <- gt_object %>%
    gt::tab_footnote(locations = gt::cells_column_labels(), footnote = gt::md(top_caption)) %>%
    gt::opt_css(c(
      paste0("#", table_id, " .gt_footnote {
              border-bottom-style: solid;
              border-bottom-width: 1px;
              border-bottom-color: ", footnote_color, ";
              font-size: 12px;}"),
      paste0("#", table_id, " .gt_footnote_marks {display: none !important;}"),
      paste0("#", table_id, " .gt_sourcenote {text-align: right;}")
    ))

  if (!is.null(bottom_caption)) {
    table <- table %>%
      gt::tab_source_note(source_note = gt::md(bottom_caption))
  }

  return(table)
}
