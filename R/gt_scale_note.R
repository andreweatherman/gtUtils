#' Scale columns and disclose the scaling
#'
#' Divides the selected columns and records the scaling in the same call, so the
#' numbers and the disclosure cannot drift apart.
#'
#' Scaling is applied through `gt::fmt_number()` and its `scale_by` argument, so
#' the underlying data is left untouched.
#'
#' @param gt_object A `gt` table object to modify.
#' @param columns The column or columns to scale.
#' @param divisor Numeric. The amount to divide by. Defaults to `1000`.
#' @param note Optional. The disclosure text. If `NULL`, it is derived from
#'   `divisor`, giving "Figures in thousands." for `1e3`, "Figures in millions."
#'   for `1e6`, and so on. Divisors without a common name fall back to
#'   "Figures divided by 2,500." Defaults to `NULL`.
#' @param where Character. Where the disclosure goes. One of `"source_note"`,
#'   `"label"` to append a suffix to the column labels instead, or `"both"`.
#'   Defaults to `"source_note"`.
#' @param label_suffix Optional. The suffix appended to column labels when `where`
#'   includes `"label"`. If `NULL`, it is derived from `divisor` (for example,
#'   `"(000s)"`). Defaults to `NULL`.
#' @param decimals Integer. The number of decimal places for the scaled values.
#'   Defaults to `0`.
#' @param ... Additional arguments passed to `gt::fmt_number`.
#'
#' @returns Returns a modified `gt` table with the columns scaled and the scaling
#'   disclosed.
#'
#' @examples
#' \dontrun{
#' library(gt)
#'
#' revenue <- data.frame(
#'   Segment = c("Cloud", "Devices", "Services"),
#'   FY24 = c(4820000, 2110000, 1360000),
#'   FY23 = c(4100000, 2260000, 1180000)
#' )
#'
#' # values render as 4.8, 2.1, 1.4 with "Figures in millions." beneath
#' gt(revenue) %>% gt_scale_note(c(FY24, FY23), divisor = 1e6, decimals = 1)
#'
#' # disclose in the column labels instead
#' gt(revenue) %>% gt_scale_note(c(FY24, FY23), divisor = 1e3, where = "label")
#' }
#'
#' @import gt
#' @importFrom magrittr %>%
#' @export
gt_scale_note <- function(gt_object, columns, divisor = 1000, note = NULL,
                          where = c("source_note", "label", "both"),
                          label_suffix = NULL, decimals = 0, ...) {

  .check_gt(gt_object)
  where <- match.arg(where)
  if (!is.numeric(divisor) || length(divisor) != 1 || divisor == 0) {
    cli::cli_abort("{.arg divisor} must be a single non-zero number.")
  }

  col_names <- names(dplyr::select(gt_object[["_data"]], {{ columns }}))
  if (!length(col_names)) cli::cli_abort("{.arg columns} matched no columns.")

  named <- list(`1e+03` = c("thousands", "(000s)"),
                `1e+06` = c("millions", "(millions)"),
                `1e+09` = c("billions", "(billions)"),
                `1e+12` = c("trillions", "(trillions)"))
  key <- format(divisor, scientific = TRUE)
  match_name <- named[[key]]

  if (is.null(note)) {
    note <- if (!is.null(match_name)) {
      paste0("Figures in ", match_name[[1]], ".")
    } else {
      paste0("Figures divided by ", format(divisor, big.mark = ",", scientific = FALSE), ".")
    }
  }
  if (is.null(label_suffix)) {
    label_suffix <- if (!is.null(match_name)) match_name[[2]] else
      paste0("(\u00f7", format(divisor, big.mark = ",", scientific = FALSE), ")")
  }

  out <- gt_object %>%
    gt::fmt_number(columns = {{ columns }}, scale_by = 1 / divisor,
                   decimals = decimals, ...)

  if (where %in% c("source_note", "both")) {
    out <- out %>% gt::tab_source_note(source_note = note)
  }
  if (where %in% c("label", "both")) {
    current <- gt_object[["_boxhead"]]
    labs <- lapply(col_names, function(cn) {
      lab <- current$column_label[current$var == cn][[1]]
      paste0(as.character(lab), " ", label_suffix)
    })
    out <- out %>% gt::cols_label(.list = stats::setNames(labs, col_names))
  }

  out
}
