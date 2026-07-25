#' Draw percentile bars in a `gt` table
#'
#' Draws a percentile as a filled track with a circular marker at the tip of the
#' fill, the value printed inside it. The bar, the marker, and the number are all
#' CSS, so the whole thing stays vector at any export scale.
#'
#' `gtExtras::gt_plt_percentile()` covers percentiles as a ggplot dot plot
#' rendered to a raster image. This is the filled-bar-and-marker treatment
#' instead.
#'
#' @param gt_object A `gt` table object to modify.
#' @param columns The columns holding percentiles.
#' @param rows The rows to draw bars in. Either an expression evaluated against
#'   the table's data, such as `pct > 50`, or a numeric vector of row indices.
#'   Rows left out keep their raw value. If `NULL`, every row is drawn. Defaults
#'   to `NULL`.
#' @param reverse Logical. Should the palette be reversed? Defaults to `FALSE`.
#' @param pal_type Character. Which `paletteer` registry to look a
#'   `package::palette` string up in, `"discrete"` or `"continuous"`. The other
#'   registry is tried as a fallback, so this rarely needs setting. Defaults to
#'   `"discrete"`.
#' @param domain A length-2 numeric vector giving the percentile range. Defaults
#'   to `c(0, 100)`.
#' @param scale How to handle a column stored as 0-1 proportions rather than
#'   0-100 percentiles. `"auto"` maps a column whose values all fall in `[0, 1]`
#'   onto `domain`, so `0.72` is treated and printed as `72` against the default
#'   range. `"none"` leaves the values untouched. A number multiplies every value
#'   by it, for any other conversion. Defaults to `"auto"`.
#' @param palette Colors mapped across `domain` and used for both the fill and
#'   the marker. Defaults to a blue-grey-red ramp.
#' @param track_color Character. The color of the unfilled track. Defaults to
#'   `"#E9E9E9"`.
#' @param track_height Numeric. The track thickness in pixels. Defaults to `6`.
#' @param marker_size Numeric. The marker diameter in pixels. Defaults to `22`.
#' @param text_color Character. The color of the number inside the marker.
#'   Defaults to `"#FFFFFF"`.
#' @param font_size Numeric. The size of the number in pixels. If `NULL`, it is
#'   set from `marker_size`. Defaults to `NULL`.
#' @param ring_color Character. A ring drawn around the marker, which separates it
#'   from the fill. `NULL` for none. Defaults to `NULL`.
#' @param ring_width Numeric. The ring thickness in pixels. Defaults to `2`.
#' @param full_track Logical. Should the track run the full width, or stop at the
#'   marker? Defaults to `TRUE`.
#' @param na_label Character. What to show for a missing percentile, such as
#'   `"Not qualified"` or `"NR"`. The track is drawn in two segments with the
#'   label centered between them, so the row keeps its rhythm and cannot be read
#'   as a percentile of zero. `NULL` draws an unbroken empty track with no text.
#'   Defaults to an em dash.
#' @param na_track_color Character. The track color for missing rows. If `NULL`,
#'   `track_color` is used. Defaults to `NULL`.
#' @param na_text_color Character. The color of `na_label`. Defaults to
#'   `"#9A9A9A"`.
#' @param decimals Integer. Decimal places on the number. Defaults to `0`.
#' @param width Numeric. The column width in pixels. Defaults to `220`.
#'
#' @details
#' The track is inset by half the marker so that a value at either extreme keeps
#' the marker inside the cell rather than hanging over the edge. The fill and the
#' marker take the same color, mapped from the value.
#'
#' @returns Returns a modified `gt` table with percentile bars.
#'
#' @examples
#' \dontrun{
#' library(gt)
#'
#' metrics <- data.frame(
#'   metric = c("Barrel %", "Exit velocity", "Chase rate", "Whiff %"),
#'   pct = c(94, 41, 72, 18)
#' )
#'
#' gt(metrics) %>% gt_percentile_bar(pct)
#'
#' # the same values stored 0-1 render identically under the default scale = "auto"
#' metrics$pct <- metrics$pct / 100
#' gt(metrics) %>% gt_percentile_bar(pct)
#'
#' # a row that did not qualify
#' metrics$pct[2] <- NA
#' gt(metrics) %>% gt_percentile_bar(pct, na_label = "Not qualified")
#' }
#'
#' @import gt
#' @importFrom magrittr %>%
#' @export
gt_percentile_bar <- function(gt_object, columns, rows = NULL,
                              domain = c(0, 100), scale = "auto",
                              palette = c("#3661AD", "#C9C9C9", "#D22D49"),
                              reverse = FALSE, pal_type = "discrete",
                              track_color = "#E9E9E9", track_height = 6,
                              marker_size = 22, text_color = "#FFFFFF",
                              font_size = NULL, ring_color = NULL, ring_width = 2,
                              full_track = TRUE, na_label = "\u2014",
                              na_track_color = NULL, na_text_color = "#9A9A9A",
                              decimals = 0, width = 220) {

  .check_gt(gt_object)

  data <- gt_object[["_data"]]
  cols <- names(dplyr::select(data, {{ columns }}))
  if (!length(cols)) return(gt_object)

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

  if (is.null(font_size)) font_size <- round(marker_size * 0.5, 1)

  # bring a 0-1 column onto the domain. only when every value fits [0, 1] and the
  # domain reaches past 1, which is what separates a proportion from a percentile
  rescale <- function(v) {
    if (is.numeric(scale)) return(v * scale)
    if (identical(scale, "none")) return(v)
    nn <- v[!is.na(v)]
    if (length(nn) && all(nn >= 0 & nn <= 1) && domain[[2]] > 1) {
      domain[[1]] + v * (domain[[2]] - domain[[1]])
    } else {
      v
    }
  }

  pal <- .resolve_palette(palette, pal_type)
  if (isTRUE(reverse)) pal <- rev(pal)
  ramp <- scales::col_numeric(pal, domain = domain)
  r <- track_height / 2
  half <- marker_size / 2

  na_track <- if (is.null(na_track_color)) track_color else na_track_color
  row_h <- marker_size + 4

  # missing gets an empty track, not a stray "NA". a label breaks the track in two
  # so it does not read as a percentile of zero
  na_cell <- function() {
    seg <- sprintf("<div style=\"flex:1; height:%.2fpx; border-radius:%.2fpx; background:%s;\"></div>",
                   track_height, r, na_track)
    inner <- if (is.null(na_label)) {
      seg
    } else {
      paste0(
        seg,
        sprintf(paste0("<span style=\"font-size:%.1fpx; color:%s; letter-spacing:0.06em;",
                       " white-space:nowrap; line-height:1;\">%s</span>"),
                font_size, na_text_color, na_label),
        seg
      )
    }
    sprintf(paste0("<div style=\"display:flex; align-items:center; gap:8px;",
                   " height:%.2fpx; padding:0 %.2fpx;\">%s</div>"),
            row_h, half, inner)
  }

  cell_for <- function(v) {
    if (is.na(v)) return(na_cell())
    frac <- (v - domain[[1]]) / (domain[[2]] - domain[[1]])
    frac <- max(0, min(1, frac))
    col <- ramp(max(domain[[1]], min(domain[[2]], v)))

    # the track is inset by half a marker so an extreme value stays in the cell
    at <- sprintf("calc(%.2fpx + %.4f * (100%% - %.2fpx))", half, frac, marker_size)

    track <- sprintf(
      paste0("<div style=\"position:absolute; top:50%%; transform:translateY(-50%%);",
             " left:%.2fpx; right:%.2fpx; height:%.2fpx; border-radius:%.2fpx;",
             " background:%s;\"></div>"),
      half, half, track_height, r, track_color
    )

    fill <- sprintf(
      paste0("<div style=\"position:absolute; top:50%%; transform:translateY(-50%%);",
             " left:%.2fpx; width:calc(%s - %.2fpx); height:%.2fpx;",
             " border-radius:%.2fpx; background:%s;\"></div>"),
      half, at, half, track_height, r, col
    )

    ring <- if (!is.null(ring_color)) {
      sprintf(" box-shadow:0 0 0 %.2fpx %s;", ring_width, ring_color)
    } else ""

    marker <- sprintf(
      paste0("<div style=\"position:absolute; top:50%%; left:%s;",
             " transform:translate(-50%%,-50%%); width:%.2fpx; height:%.2fpx;",
             " border-radius:50%%; background:%s; color:%s; font-size:%.1fpx;",
             " font-weight:700; line-height:%.2fpx; text-align:center;%s\">%s</div>"),
      at, marker_size, marker_size, col, text_color, font_size, marker_size, ring,
      formatC(v, format = "f", digits = decimals)
    )

    paste0(
      "<div style=\"position:relative; width:100%; height:", row_h, "px;\">",
      if (isTRUE(full_track)) track else "",
      fill, marker,
      "</div>"
    )
  }

  # Reduce, not a for loop: text_transform stores fn and calls it at render, so a
  # loop variable would be the last column's by then and every column would draw
  # the same bars
  gt_object <- Reduce(function(tbl, nm) {
    v <- rescale(suppressWarnings(as.numeric(data[[nm]])))[keep]
    cells <- vapply(v, cell_for, character(1))
    tbl %>%
      gt::text_transform(
        locations = gt::cells_body(columns = tidyselect::all_of(nm), rows = keep),
        fn = function(x) cells
      )
  }, cols, init = gt_object)

  if (!is.null(width)) {
    fs <- lapply(cols, function(nm) {
      rlang::new_formula(rlang::sym(nm), rlang::expr(gt::px(!!width)))
    })
    gt_object <- gt::cols_width(gt_object, .list = fs)
  }

  .record_scale(gt_object, cols, palette, domain, reverse, pal_type)
}
