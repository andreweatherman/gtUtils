#' Add significance stars to a `gt` table
#'
#' Appends significance notation to an estimate column based on a column of
#' p-values, then writes the matching legend into a source note. The p-value
#' column is hidden by default, since the stars stand in for it. This is the usual
#' convention for regression tables in the social and medical sciences.
#'
#' The legend is generated from the same `levels` and `symbols` used to place the
#' stars, so changing one updates the other.
#'
#' @param gt_object A `gt` table object to modify.
#' @param columns The estimate column or columns to annotate.
#' @param p_columns The column or columns holding the p-values, paired
#'   positionally with `columns`. Supply one p-value column per estimate column.
#' @param levels A numeric vector of significance thresholds, in ascending order
#'   (strictest first). Defaults to `c(0.01, 0.05, 0.1)`.
#' @param symbols A character vector of notation for each level. Must be the same
#'   length as `levels`. Defaults to `c("***", "**", "*")`.
#' @param superscript Logical. Should the stars be rendered as superscript?
#'   Defaults to `TRUE`.
#' @param size Character. The size of the stars, as a CSS size. Defaults to `"0.7em"`.
#' @param legend Logical. Should the legend be added as a source note? Defaults
#'   to `TRUE`.
#' @param legend_text Optional. Custom legend text. If `NULL`, the legend is built
#'   from `levels` and `symbols`. Defaults to `NULL`.
#' @param hide_p Logical. Should the p-value columns be hidden once the stars are
#'   applied? Defaults to `TRUE`.
#'
#' @details
#' Each value takes the notation for the strictest threshold it satisfies, so with
#' the defaults a p-value of 0.004 gets `***` and not `*`. Values that meet none
#' of the thresholds are left alone, as are `NA` p-values.
#'
#' @returns Returns a modified `gt` table with significance notation applied.
#'
#' @examples
#' \dontrun{
#' library(gt)
#'
#' fit <- lm(mpg ~ wt + hp + factor(cyl), data = mtcars)
#' results <- data.frame(
#'   Term = rownames(summary(fit)$coefficients),
#'   Estimate = summary(fit)$coefficients[, 1],
#'   SE = summary(fit)$coefficients[, 2],
#'   p = summary(fit)$coefficients[, 4]
#' )
#'
#' gt(results) %>%
#'   fmt_number(c(Estimate, SE), decimals = 3) %>%
#'   gt_significance(Estimate, p)
#'
#' # daggers instead of stars, at a single threshold
#' gt(results) %>%
#'   gt_significance(Estimate, p, levels = 0.05, symbols = "†",
#'                   legend_text = "† p < .05")
#' }
#'
#' @seealso [gt_fmt_rank()], which uses the same superscript approach for
#'   ordinal suffixes.
#' @import gt
#' @importFrom magrittr %>%
#' @export
gt_significance <- function(gt_object, columns, p_columns,
                            levels = c(0.01, 0.05, 0.1),
                            symbols = c("***", "**", "*"),
                            superscript = TRUE, size = "0.7em",
                            legend = TRUE, legend_text = NULL,
                            hide_p = TRUE) {

  .check_gt(gt_object)
  if (length(levels) != length(symbols)) {
    cli::cli_abort("{.arg levels} and {.arg symbols} must be the same length.")
  }
  if (is.unsorted(levels)) {
    cli::cli_abort("{.arg levels} must be in ascending order, strictest first.")
  }

  data <- gt_object[["_data"]]
  est_cols <- names(dplyr::select(data, {{ columns }}))
  p_cols <- names(dplyr::select(data, {{ p_columns }}))

  if (!length(est_cols)) cli::cli_abort("{.arg columns} matched no columns.")
  if (length(p_cols) != length(est_cols)) {
    cli::cli_abort(c(
      "{.arg p_columns} must pair with {.arg columns}.",
      "x" = "Got {length(est_cols)} estimate column{?s} and {length(p_cols)} p-value column{?s}."
    ))
  }

  # strictest satisfied level wins
  stars <- function(p) {
    out <- rep("", length(p))
    for (i in rev(seq_along(levels))) {
      out[!is.na(p) & p < levels[[i]]] <- symbols[[i]]
    }
    out
  }

  # Reduce, not a for loop: text_transform stores fn and calls it at render, so a
  # loop variable would be the last pair's by then and every estimate column would
  # take the last p-value column's stars
  gt_object <- Reduce(function(tbl, i) {
    marks <- stars(suppressWarnings(as.numeric(data[[p_cols[[i]]]])))
    tbl %>%
      gt::text_transform(
        locations = gt::cells_body(columns = tidyselect::all_of(est_cols[[i]])),
        fn = function(x) {
          m <- rep_len(marks, length(x))
          mark <- ifelse(
            nzchar(m),
            if (superscript) {
              paste0("<sup style='font-size:", size, ";'>", m, "</sup>")
            } else m,
            ""
          )
          paste0(x, mark)
        }
      )
  }, seq_along(est_cols), init = gt_object)

  if (isTRUE(hide_p)) {
    gt_object <- gt_object %>% gt::cols_hide(columns = tidyselect::all_of(p_cols))
  }

  if (isTRUE(legend)) {
    if (is.null(legend_text)) {
      legend_text <- paste(
        paste0(symbols, " p < ", levels),
        collapse = ", "
      )
    }
    gt_object <- gt_object %>% gt::tab_source_note(source_note = gt::html(legend_text))
  }

  gt_object
}
