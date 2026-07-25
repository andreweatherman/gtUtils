#' Style and relabel missing values in a `gt` table
#'
#' Highlights missing cells and can relabel them in the same call.
#' `gt::sub_missing()` substitutes text only, and only for real `NA` values.
#'
#' Alongside real `NA`, this also catches values that are literally the string
#' `"NA"`, a common artifact of reading a CSV and a frequent reason
#' `sub_missing()` appears to do nothing. Widen `na_strings` to catch other
#' placeholders such as `"-"` or `"N/A"`.
#'
#' @param gt_object A `gt` table object to modify.
#' @param columns The column or columns to check. Defaults to all of them.
#' @param fill Character. A hex color for the cell fill behind missing values.
#'   Defaults to `"#F0F0F0"`.
#' @param text_color Optional. A hex color for the text of missing values.
#'   Defaults to `NULL`.
#' @param bold Logical. Should missing values be bolded? Defaults to `FALSE`.
#' @param italic Logical. Should missing values be italicized? Defaults to `FALSE`.
#' @param missing_text Optional. Replacement text for missing values, such as
#'   `"--"` or `"Not reported"`. Defaults to `NULL`, which leaves the text alone.
#' @param na_strings A character vector of strings to treat as missing alongside
#'   real `NA`. Defaults to `"NA"`.
#' @param ignore_case Logical. Should `na_strings` be matched case-insensitively?
#'   Defaults to `FALSE`.
#' @param ... Additional arguments passed to `gt::cell_text`.
#'
#' @returns Returns a modified `gt` table with missing values styled.
#'
#' @examples
#' \dontrun{
#' library(gt)
#'
#' gt(head(airquality, 10)) %>% gt_highlight_na(c(Ozone, Solar.R))
#'
#' # relabel as well as highlight
#' gt(head(airquality, 10)) %>%
#'   gt_highlight_na(c(Ozone, Solar.R), missing_text = "not recorded",
#'                   italic = TRUE, fill = "#FFF8E1")
#'
#' # also catch placeholder strings left behind by a CSV import
#' gt(head(airquality, 10)) %>%
#'   gt_highlight_na(everything(), na_strings = c("NA", "N/A", "-"))
#' }
#'
#' @seealso [gt_outliers()] for flagging values that are present but suspect.
#' @import gt
#' @importFrom magrittr %>%
#' @export
gt_highlight_na <- function(gt_object, columns = gt::everything(),
                            fill = "#F0F0F0", text_color = NULL,
                            bold = FALSE, italic = FALSE,
                            missing_text = NULL, na_strings = "NA",
                            ignore_case = FALSE, ...) {

  .check_gt(gt_object)

  data <- gt_object[["_data"]]
  col_names <- names(dplyr::select(data, {{ columns }}))

  # real NA plus any configured missing strings
  is_missing <- function(v) {
    cv <- trimws(as.character(v))
    cmp <- na_strings
    if (ignore_case) {
      cv <- tolower(cv)
      cmp <- tolower(cmp)
    }
    is.na(v) | (cv %in% cmp)
  }

  # build the style once
  text_args <- c(
    list(
      color = text_color,
      weight = if (bold) "bold" else NULL,
      style = if (italic) "italic" else NULL
    ),
    list(...)
  )
  text_args <- text_args[!vapply(text_args, is.null, logical(1))]

  styles <- list()
  if (!is.null(fill)) styles <- c(styles, list(gt::cell_fill(color = fill)))
  if (length(text_args)) styles <- c(styles, list(do.call(gt::cell_text, text_args)))

  for (col in col_names) {
    na_rows <- which(is_missing(data[[col]]))
    if (length(na_rows) == 0) next

    if (length(styles)) {
      gt_object <- gt_object %>%
        gt::tab_style(
          style = styles,
          locations = gt::cells_body(columns = dplyr::all_of(col), rows = na_rows)
        )
    }

    if (!is.null(missing_text)) {
      gt_object <- gt_object %>%
        gt::text_transform(
          locations = gt::cells_body(columns = dplyr::all_of(col), rows = na_rows),
          fn = function(x) rep(missing_text, length(x))
        )
    }
  }

  gt_object
}
