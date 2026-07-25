#' Add color pills to `gt` table columns
#'
#' Renders values as rounded color pills, filled from a palette by either the raw
#' value or the ordinal rank. Accepts a plain vector of hex colors or a
#' `paletteer` palette, and an explicit domain or one taken from the data.
#' [gt_color_ranks()] fills the whole cell background instead of a pill.
#'
#' @param gt_object A `gt` table object to modify.
#' @param columns The columns to fill with pills, using tidyselect.
#' @param rows The rows to fill. Either an expression evaluated against the
#'   table's data, such as `mpg > 20`, or a numeric vector of row indices. Rows
#'   left out keep their raw value. If `NULL`, every row is filled. Defaults to
#'   `NULL`.
#' @param palette A vector of hex colors, or a `paletteer` palette given as
#'   `package::palette`. Defaults to `c("#C84630", "#5DA271")`.
#' @param fill_type Character. Either `"rank"` or `"continuous"`, choosing whether
#'   color follows ordinal rank or the raw values. Defaults to `"continuous"`.
#' @param rank_order Character. Either `"asc"` or `"desc"`, used when `fill_type`
#'   is `"rank"`. Defaults to `"desc"`.
#' @param digits Integer. Decimal places to round the printed value to. Defaults
#'   to `NULL`.
#' @param domain Numeric. A length-2 vector giving the value range mapped onto the
#'   palette. If `NULL`, the observed range of the column is used and a warning is
#'   issued. Defaults to `NULL`.
#' @param format_type Character. How to format the printed value. One of
#'   `"number"`, `"comma"`, `"currency"`, or `"percent"`. Defaults to `"number"`.
#' @param scale_percent Logical. When `format_type` is `"percent"`, should values
#'   be multiplied by 100? Defaults to `TRUE`.
#' @param suffix Character. A string appended to each formatted value, such as
#'   `"M"`, `"K"`, or `"lbs"`. Defaults to `""`.
#' @param reverse Logical. Should the palette be reversed? Defaults to `FALSE`.
#' @param outline_color Optional. A hex color for a border around each pill.
#'   Defaults to `NULL`, no border.
#' @param outline_width Numeric. The border width in pixels. Defaults to `0.25`.
#' @param pal_type Character. Which `paletteer` registry to look a
#'   `package::palette` string up in, `"discrete"` or `"continuous"`. The other
#'   registry is tried as a fallback, so this rarely needs setting. Defaults to
#'   `"discrete"`.
#' @param pill_height Numeric. The height of each pill in pixels. Defaults to `25`.
#' @param text_color Optional. A hex color for the pill text. When `NULL`, the
#'   text color is chosen for contrast against each pill's fill. Defaults to
#'   `NULL`.
#' @param na_color Optional. A hex color for the pill drawn over a missing value.
#'   When `NULL`, a missing value is left blank with no pill. Defaults to `NULL`.
#' @param ... Additional arguments passed to `scales::col_numeric`.
#'
#' @details
#' Pills are drawn as HTML spans through `gt::text_transform()`, so they survive
#' `gtsave()`. The fill is mapped with `scales::col_numeric()` over `domain`, and
#' the text is set to black or white, whichever measures higher contrast against
#' that fill, unless `text_color` is set. A missing value takes an `na_color`
#' pill, or is left blank when `na_color` is `NULL`.
#'
#' Selecting several columns maps them all onto **one** `domain`, taken from the
#' selection as a whole when `domain` is unset, so their colors stay comparable.
#' Pill width is worked out **per column**, in `ch` units, so each column's pills
#' line up with each other rather than with the widest value in the selection.
#' With `fill_type = "rank"`, each column is ranked against itself.
#'
#' When `fill_type` is `"rank"`, ranks are computed with `rank()` using averaged
#' ties, then flipped when `rank_order` is `"desc"` so the top value anchors the
#' high end of the palette. Leaving `domain` unset falls back to the observed
#' range and warns, since the color mapping then depends on the data present.
#'
#' @import scales
#' @import gt
#' @import glue
#' @import paletteer
#' @importFrom magrittr %>%
#'
#' @returns Returns a modified `gt` table with color pills in the selected columns.
#'
#' @seealso [gt_color_ranks()] for filling the whole cell, and
#'   [gt_legend_continuous()] for a legend explaining the scale.
#'
#' @examples
#' \dontrun{
#' library(gt)
#'
#' gt(head(mtcars)) %>% gt_color_pills(mpg, domain = c(10, 35))
#'
#' # several columns on one shared domain
#' gt(head(mtcars)) %>% gt_color_pills(c(disp, hp), domain = c(50, 500))
#'
#' # tidyselect works too
#' gt(head(mtcars)) %>% gt_color_pills(where(is.numeric), domain = c(0, 500))
#'
#' # only the rows that clear a threshold; the rest keep their raw value
#' gt(head(mtcars)) %>% gt_color_pills(mpg, rows = mpg > 20, domain = c(10, 35))
#'
#' # color by rank rather than value, with a paletteer palette
#' gt(head(mtcars)) %>%
#'   gt_color_pills(hp, fill_type = "rank", palette = "viridis::mako",
#'                  digits = 0)
#' }
#'
#' @export
gt_color_pills <- function(gt_object, columns, rows = NULL,
                           palette = c("#C84630", "#5DA271"),
                           fill_type = "continuous", rank_order = "desc",
                           digits = NULL, domain = NULL, format_type = "number",
                           scale_percent = TRUE, suffix = "", reverse = FALSE,
                           outline_color = NULL, outline_width = 0.25,
                           pal_type = "discrete", pill_height = 25,
                           text_color = NULL, na_color = NULL, ...) {

  .check_gt(gt_object)

  data <- gt_object[["_data"]]
  cols <- names(dplyr::select(data, {{ columns }}))
  if (!length(cols)) {
    cli::cli_abort("{.arg columns} matched no columns.")
  }

  # rows: a data-masked expression, raw indices, or NULL for all
  rows_q <- rlang::enquo(rows)
  keep <- if (rlang::quo_is_null(rows_q)) {
    seq_len(nrow(data))
  } else {
    res <- rlang::eval_tidy(rows_q, data = data)
    idx <- if (is.logical(res)) which(res) else as.integer(res)
    idx <- idx[!is.na(idx) & idx >= 1 & idx <= nrow(data)]
    if (!length(idx)) {
      cli::cli_warn("{.arg rows} matched no rows; returning the table unchanged.")
      return(gt_object)
    }
    idx
  }

  # rank per column, so each is ranked against itself rather than the block
  vals <- lapply(cols, function(cn) suppressWarnings(as.numeric(data[[cn]])))
  names(vals) <- cols
  scaled <- lapply(vals, function(v) {
    if (fill_type != "rank") return(v)
    r <- rank(v, na.last = "keep", ties.method = "average")
    if (rank_order == "desc") max(r, na.rm = TRUE) - r + 1 else r
  })

  # one domain across the selected columns, so their colors stay comparable
  if (is.null(domain)) {
    domain <- range(unlist(scaled, use.names = FALSE), na.rm = TRUE)
    cli::cli_warn(c(
      "No {.arg domain} given, so the colors span the observed range \\
       ({.val {domain[[1]]}} to {.val {domain[[2]]}}).",
      "i" = "Set {.arg domain} to compare colors across tables or columns."
    ))
  }

  pal <- .resolve_palette(palette, pal_type)
  if (isTRUE(reverse)) pal <- rev(pal)

  format_value <- function(value, digits, format_type) {
    if (format_type == "percent" && scale_percent) {
      value <- value * 100
    }

    if (!is.null(digits)) {
      value <- round(value, digits)
    }

    # no digits: format naturally, so a whole number does not gain trailing zeros
    big <- if (format_type %in% c("comma", "currency")) "," else ""
    core <- if (is.null(digits)) {
      format(value, trim = TRUE, big.mark = big, scientific = FALSE)
    } else {
      formatC(value, format = "f", big.mark = big, digits = digits)
    }

    formatted_value <- switch(format_type,
                              "currency" = paste0("$", core),
                              "percent" = paste0(core, "%"),
                              core)

    return(paste0(formatted_value, suffix))
  }

  ramp <- scales::col_numeric(palette = pal, domain = domain, ...)

  generate_pill_html <- function(value, rank_value, max_width) {
    # a missing value gets an na_color pill, or no pill when na_color is NULL
    if (is.na(rank_value)) {
      if (is.null(na_color)) return("")
      color <- na_color
      formatted_value <- ""
    } else {
      color <- ramp(rank_value)
      formatted_value <- format_value(as.numeric(value), digits, format_type)
    }

    tc <- if (!is.null(text_color)) text_color else .theme_on_color(color)

    outline_style <- if (!is.null(outline_color)) glue::glue("border: {outline_width}px solid {outline_color};") else ""

    glue::glue("<span style='display: inline-block; width: {max_width}ch; padding-left: 3px; padding-right: 3px; height: {pill_height}px; line-height: {pill_height}px; background-color: {color}; color: {tc}; border-radius: 10px; text-align: center; {outline_style}'>{formatted_value}</span>")
  }

  # one pass per column. width is per column, so each column's pills line up with
  # each other rather than with the widest value in the whole selection
  out <- Reduce(function(tbl, cn) {
    v <- scaled[[cn]][keep]
    widths <- nchar(vapply(vals[[cn]][keep], function(z) format_value(z, digits, format_type),
                           character(1)))
    w <- if (length(widths)) max(widths, na.rm = TRUE) else 1
    tbl %>%
      text_transform(
        locations = cells_body(columns = tidyselect::all_of(cn), rows = keep),
        fn = function(x) mapply(generate_pill_html, x, v, MoreArgs = list(max_width = w))
      )
  }, cols, init = gt_object)

  .record_scale(out, cols, palette, domain, reverse, pal_type)
}
