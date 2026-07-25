#' Flag outlying values in a `gt` table
#'
#' Marks cells that fall outside a threshold, either beyond an interquartile
#' fence, beyond a number of standard deviations, or outside bounds you supply.
#'
#' `gt::data_color()` shades a continuous scale; this flags a value as unusual.
#' Thresholds are computed separately for each column, so every column is judged
#' against its own distribution.
#'
#' @param gt_object A `gt` table object to modify.
#' @param columns The column or columns to test. Non-numeric columns are skipped.
#' @param method Character. How to decide what counts as an outlier. One of
#'   `"iqr"`, `"sd"`, or `"bounds"`. Defaults to `"iqr"`.
#' @param threshold Numeric. The cutoff used by `"iqr"` and `"sd"`. If `NULL`, the
#'   convention for the method is used: `1.5` for `"iqr"` and `3` for `"sd"`.
#'   Defaults to `NULL`.
#' @param bounds A length-2 numeric vector giving `c(lower, upper)`, required when
#'   `method` is `"bounds"`. Use `NA` for an open end, such as `c(NA, 100)`.
#'   Defaults to `NULL`.
#' @param side Character. Which tail to flag. One of `"both"`, `"high"`, or
#'   `"low"`. Defaults to `"both"`.
#' @param fill Optional. A hex color for the cell fill behind flagged values.
#'   Defaults to `NULL`, which applies no fill.
#' @param color Optional. A hex color for flagged text. If `NULL`, a warning red
#'   is used, swapped for a readable alternative when it would not have enough
#'   contrast against `fill`. Defaults to `NULL`.
#' @param bold Logical. Should flagged values be bolded? Defaults to `TRUE`.
#' @param symbol Optional. A marker appended to flagged values, such as `"†"`.
#'   Defaults to `NULL`.
#' @param note Optional. A source note describing the rule that was applied. Pass
#'   `TRUE` for wording generated from `method` and `threshold`, a string for your
#'   own, or `NULL` for no note. Defaults to `NULL`.
#'
#' @details
#' The default rule is the interquartile fence, not standard deviations, because
#' an SD fence is built from a spread that the outlier itself inflates. On
#' `c(10.2, 10.4, 10.1, 19.8, 10.3, 10.0)` the mean is 11.8 and the standard
#' deviation 3.92, so a three-SD fence reaches 23.6 and misses the 19.8. The
#' quartiles barely move, so the IQR fence stops at 10.75 and catches it. The
#' masking is worst in small samples. Use `method = "sd"` if you want it anyway.
#'
#' @returns Returns a modified `gt` table with outlying values marked.
#'
#' @examples
#' \dontrun{
#' library(gt)
#'
#' assays <- data.frame(
#'   Sample = paste0("S", 1:6),
#'   Run1 = c(10.2, 10.4, 10.1, 19.8, 10.3, 10.0),
#'   Run2 = c(9.9, 10.1, 10.3, 10.2, 2.1, 10.4)
#' )
#'
#' # the default fence catches both the high and the low reading
#' gt(assays) %>% gt_outliers(c(Run1, Run2), note = TRUE)
#'
#' # an explicit acceptance range, flagging only the high side
#' gt(assays) %>%
#'   gt_outliers(c(Run1, Run2), method = "bounds", bounds = c(9, 11),
#'               side = "high", fill = "#FDECEA", symbol = "†")
#'
#' # works on wider data too
#' gt(head(airquality, 12)) %>% gt_outliers(c(Ozone, Wind, Temp))
#' }
#'
#' @seealso [gt_highlight_na()] for missing values, and [gt_spotlight()] for
#'   drawing attention to whole rows.
#' @import gt
#' @importFrom magrittr %>%
#' @export
gt_outliers <- function(gt_object, columns, method = c("iqr", "sd", "bounds"),
                        threshold = NULL, bounds = NULL,
                        side = c("both", "high", "low"),
                        fill = NULL, color = NULL, bold = TRUE,
                        symbol = NULL, note = NULL) {

  .check_gt(gt_object)
  method <- match.arg(method)
  side <- match.arg(side)
  if (is.null(threshold)) threshold <- if (method == "sd") 3 else 1.5
  if (method == "bounds") {
    if (is.null(bounds) || length(bounds) != 2 || !is.numeric(bounds)) {
      cli::cli_abort("{.arg bounds} must be a length-2 numeric vector when {.arg method} is {.val bounds}.")
    }
  }

  data <- gt_object[["_data"]]
  col_names <- names(dplyr::select(data, {{ columns }}))
  if (!length(col_names)) cli::cli_abort("{.arg columns} matched no columns.")

  numeric_cols <- col_names[vapply(col_names, function(cn) is.numeric(data[[cn]]), logical(1))]
  if (!length(numeric_cols)) {
    cli::cli_warn("No numeric columns among {.arg columns}; nothing to flag.")
    return(gt_object)
  }

  # keep the text readable if a fill was given
  if (is.null(color)) {
    color <- if (is.null(fill)) "#B3261E" else {
      ink <- .theme_on_color(fill)
      if (.theme_contrast("#B3261E", fill) >= 4.5) "#B3261E" else ink
    }
  }

  flagged_any <- FALSE
  for (cn in numeric_cols) {
    v <- data[[cn]]
    lims <- switch(
      method,
      sd = {
        m <- mean(v, na.rm = TRUE); s <- stats::sd(v, na.rm = TRUE)
        if (is.na(s) || s == 0) c(-Inf, Inf) else c(m - threshold * s, m + threshold * s)
      },
      iqr = {
        q <- stats::quantile(v, c(0.25, 0.75), na.rm = TRUE, names = FALSE)
        iq <- q[[2]] - q[[1]]
        if (is.na(iq) || iq == 0) c(-Inf, Inf) else c(q[[1]] - threshold * iq, q[[2]] + threshold * iq)
      },
      bounds = c(if (is.na(bounds[[1]])) -Inf else bounds[[1]],
                 if (is.na(bounds[[2]])) Inf else bounds[[2]])
    )

    low <- !is.na(v) & v < lims[[1]]
    high <- !is.na(v) & v > lims[[2]]
    hit <- switch(side, both = low | high, high = high, low = low)
    if (!any(hit)) next
    flagged_any <- TRUE
    rows <- which(hit)

    style <- list(gt::cell_text(color = color, weight = if (isTRUE(bold)) "bold" else NULL))
    if (!is.null(fill)) style <- c(style, list(gt::cell_fill(color = fill)))

    gt_object <- gt_object %>%
      gt::tab_style(
        style = style,
        locations = gt::cells_body(columns = tidyselect::all_of(cn), rows = rows)
      )

    if (!is.null(symbol)) {
      gt_object <- gt_object %>%
        gt::text_transform(
          locations = gt::cells_body(columns = tidyselect::all_of(cn), rows = rows),
          fn = function(x) paste0(x, symbol)
        )
    }
  }

  if (!flagged_any) return(gt_object)

  if (!is.null(note) && !identical(note, FALSE)) {
    txt <- if (isTRUE(note)) {
      tail_txt <- switch(side, both = "", high = " (high side only)", low = " (low side only)")
      switch(
        method,
        sd = paste0("Marked values fall more than ", threshold,
                     " standard deviation", if (threshold == 1) "" else "s",
                     " from the column mean", tail_txt, "."),
        iqr = paste0("Marked values fall outside ", threshold,
                     " \u00d7 IQR of the column quartiles", tail_txt, "."),
        bounds = paste0("Marked values fall outside ",
                        format(bounds[[1]]), "\u2013", format(bounds[[2]]), tail_txt, ".")
      )
    } else as.character(note)
    gt_object <- gt_object %>% gt::tab_source_note(source_note = txt)
  }

  gt_object
}
