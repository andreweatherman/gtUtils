#' Color ranked columns of a `gt` table
#'
#' A shorthand around `gt::data_color()` for filling columns that already hold
#' rank values, where 1 is best. It takes either a plain vector of hex colors or a
#' `paletteer` palette, and either an explicit domain or one derived from the
#' data. Values are colored as they are; no ranking is computed for you.
#'
#' The whole cell background is filled. [gt_color_pills()] does the same job with
#' rounded pill spans instead.
#'
#' @param gt_object A `gt` table object to modify.
#' @param columns The column or columns to color.
#' @param rows The rows to color. Either an expression evaluated against the
#'   table's data, such as `mpg > 20`, or a numeric vector of row indices. Rows
#'   left out keep their default background. If `NULL`, every row is colored.
#'   Defaults to `NULL`.
#' @param palette A color palette to use. If you want a palette from `paletteer`,
#'   specify it as `package::palette`. Defaults to a five-color green-to-red ramp.
#' @param domain A length-2 numeric vector giving the value range mapped onto the
#'   palette. If `NULL`, the range is taken from the selected columns and shared
#'   across all of them, so rank 1 and the largest rank present anchor the two
#'   ends of the palette. Defaults to `NULL`.
#' @param reverse Logical. Should the palette be reversed? Defaults to `FALSE`.
#' @param na_color Character. The color used for `NA` values. Defaults to `"white"`.
#' @param autocolor_text Logical. Should the text color be set automatically for
#'   contrast against the fill? Defaults to `TRUE`.
#' @param pal_type Character. Either `"discrete"` or `"continuous"`, used when
#'   applying `paletteer` palettes. Defaults to `"discrete"`.
#' @param ... Additional arguments passed to `gt::data_color`.
#'
#' @returns Returns a modified `gt` table with the selected columns filled by value.
#'
#' @examples
#' \dontrun{
#' library(gt)
#'
#' ranked <- data.frame(
#'   model = rownames(head(mtcars, 6)),
#'   mpg_rank = rank(-head(mtcars, 6)$mpg),
#'   hp_rank = rank(head(mtcars, 6)$hp)
#' )
#'
#' gt(ranked) %>% gt_color_ranks(c(mpg_rank, hp_rank))
#'
#' # a paletteer palette, reversed
#' gt(ranked) %>%
#'   gt_color_ranks(mpg_rank, palette = "viridis::mako", pal_type = "continuous",
#'                  reverse = TRUE)
#' }
#'
#' @seealso [gt_legend_continuous()] for explaining the scale, and [gt_color_pills()]
#'   for a pill treatment instead of a full-cell fill.
#' @import gt
#' @importFrom magrittr %>%
#' @export
gt_color_ranks <- function(gt_object, columns, rows = NULL,
                           palette = c("#3D8B6E", "#9DC5A7", "#EDE0CC", "#DB9070", "#BE4D3A"),
                           domain = NULL, reverse = FALSE, na_color = "white",
                           autocolor_text = TRUE, pal_type = "discrete", ...) {

  .check_gt(gt_object)

  pal <- .resolve_palette(palette, pal_type)

  # rows: a data-masked expression, raw indices, or NULL for all
  rows_q <- rlang::enquo(rows)
  keep <- if (rlang::quo_is_null(rows_q)) {
    seq_len(nrow(gt_object[["_data"]]))
  } else {
    res <- rlang::eval_tidy(rows_q, data = gt_object[["_data"]])
    idx <- if (is.logical(res)) which(res) else as.integer(res)
    idx <- idx[!is.na(idx) & idx >= 1 & idx <= nrow(gt_object[["_data"]])]
    if (!length(idx)) {
      cli::cli_warn("{.arg rows} matched no rows; returning the table unchanged.")
      return(gt_object)
    }
    idx
  }

  # shared domain across the selected cols
  if (is.null(domain)) {
    col_names <- names(dplyr::select(gt_object[["_data"]], {{ columns }}))
    vals <- suppressWarnings(as.numeric(unlist(gt_object[["_data"]][col_names], use.names = FALSE)))
    domain <- range(vals, na.rm = TRUE)
  }

  out <- gt_object %>%
    gt::data_color(
      columns = {{ columns }},
      rows = keep,
      palette = pal,
      domain = domain,
      na_color = na_color,
      reverse = reverse,
      autocolor_text = autocolor_text,
      ...
    )

  .record_scale(out, names(dplyr::select(gt_object[["_data"]], {{ columns }})),
                palette, domain, reverse, pal_type)
}
