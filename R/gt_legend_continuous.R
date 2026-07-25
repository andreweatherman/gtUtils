#' Add a color legend to a `gt` table
#'
#' Draws a legend for a column colored with [gt_color_ranks()] or
#' `gt::data_color()`. Pass it the same `columns` and `palette` you colored with
#' and the legend will match the table, including the domain it takes from the
#' data. For a hand-built key of labeled swatches, use [gt_legend_discrete()].
#'
#' The bar can be a smooth gradient, a stepped ramp, or separated blocks. The
#' title can sit on any side of it, and the title and bound labels are styled
#' independently through `title_style` and `labels_style`.
#'
#' @section Styling:
#'
#' `title_style` and `labels_style` are named lists following the same convention
#' as [gt_grid()] and [gt_title_header()]. Recognized keys are `font` (a Google
#' font name), `size`, `color`, `weight`, `italic`, `spacing` (letter spacing),
#' `transform` (such as `"uppercase"`), and `align`, plus `line_height`,
#' `margin_top`, `margin_bottom`, `padding_top`, and `padding_bottom`. Any length
#' takes a number, read as pixels, or a CSS string.
#'
#' @param gt_object A `gt` table object to modify.
#' @param columns The column or columns the legend describes, used to derive
#'   `domain`. Defaults to `NULL`, which takes the columns from an earlier
#'   coloring call, or requires `domain` to be given instead. See Picking up the
#'   scale.
#' @param palette A color palette to use. If you want a palette from `paletteer`,
#'   specify it as `package::palette`. Defaults to the same five-color
#'   green-to-red ramp as [gt_color_ranks()].
#' @param domain A length-2 numeric vector giving the value range the palette
#'   spans. If `NULL`, the range is taken from `columns` the same way
#'   [gt_color_ranks()] takes it, so the two agree. Defaults to `NULL`.
#' @param reverse Logical. Should the palette be reversed? Set this to match the
#'   `reverse` used when coloring. Defaults to `FALSE`.
#' @param pal_type Character. Either `"discrete"` or `"continuous"`, used when
#'   applying `paletteer` palettes. Defaults to `"discrete"`.
#' @param type Character. The appearance of the bar. One of `"continuous"` for a
#'   smooth ramp, `"steps"` for `n_bins` flat steps that touch, or `"blocks"` for
#'   `n_bins` separated swatches. Defaults to `"continuous"`.
#' @param n_bins Integer. The number of bins when `type` is `"steps"` or
#'   `"blocks"`. Defaults to `5`.
#' @param labels The tick labels. If `NULL`, the two ends of `domain` are used.
#'   The string `"edges"` uses the `n_bins + 1` bin boundaries. Otherwise pass any
#'   character vector, which is spread evenly. Defaults to `NULL`.
#' @param digits Integer. The number of decimal places used when deriving labels
#'   from `domain`. Defaults to `0`.
#' @param title Optional. A caption for the legend. Defaults to `NULL`.
#' @param title_position Character. Which side of the bar the title sits on. One
#'   of `"top"`, `"bottom"`, `"left"`, or `"right"`. Defaults to `"top"`.
#' @param title_style A named list styling the title. See Styling. Defaults to an
#'   empty list.
#' @param labels_style A named list styling the bound labels. See Styling.
#'   Defaults to an empty list.
#' @param labels_position Character. One of `"bottom"`, `"top"`, or `"none"` to
#'   omit the labels. Defaults to `"bottom"`.
#' @param location Character. Where to place the legend. `"bottom"` adds it as a
#'   source note; `"top"` puts it in the header, keeping any existing title and
#'   subtitle. Defaults to `"bottom"`.
#' @param align Character. The horizontal alignment of the whole legend. One of
#'   `"center"`, `"left"`, or `"right"`. Defaults to `"center"`.
#' @param width Numeric. The width of the bar in pixels. Defaults to `200`.
#' @param height Numeric. The height of the bar in pixels. Defaults to `10`.
#' @param border_color Optional. A hex color for a border around the bar, or
#'   around each block when `type` is `"blocks"`. Defaults to `NULL`.
#' @param border_width Numeric. The border width in pixels. Defaults to `1`.
#' @param radius Numeric. The corner radius of the bar in pixels. Defaults to `2`.
#' @param gap Numeric. The gap between the bar and its labels, in pixels.
#'   Defaults to `3`.
#' @param title_gap Numeric. The gap between the title and the bar, in pixels.
#'   Defaults to `4`.
#' @param block_gap Numeric. The gap between blocks when `type` is `"blocks"`, in
#'   pixels. Defaults to `2`.
#'
#' @section Picking up the scale:
#'
#' [gt_color_ranks()], [gt_color_pills()] and [gt_percentile_bar()] record the
#' `columns`, `palette`, `domain`, `reverse` and `pal_type` they used on the
#' table. This function reads any of those the caller did not supply, so a legend
#' can be added with no arguments and cannot disagree with the cells it explains:
#'
#' ```r
#' gt(df) %>%
#'   gt_color_ranks(net, palette = "viridis::mako", domain = c(-10, 12)) %>%
#'   gt_legend_continuous()
#' ```
#'
#' Anything passed explicitly wins over the recorded value. With several coloring
#' calls, the most recent one is the one recorded. The record is an attribute on
#' the table and survives the rest of a pipeline, apart from [gt_snake()], which
#' rebuilds the table from its data; add the legend before snaking, or pass the
#' arguments directly.
#'
#' @details
#' The bar is drawn as a series of solid color segments instead of a CSS
#' gradient. `gt`'s inline-CSS path, used by the RStudio Viewer, Quarto and R
#' Markdown, strips gradient backgrounds, and the legend would disappear. Segment
#' colors come from `scales::col_numeric`, the same function `gt::data_color()`
#' uses, so the ramp matches the cells exactly.
#'
#' [gt_title_header()] calls `gt::tab_header()`, which replaces the whole header.
#' With `location = "top"`, call [gt_title_header()] first or it will overwrite
#' the legend.
#'
#' @returns Returns a modified `gt` table with the legend added.
#'
#' @examples
#' \dontrun{
#' library(gt)
#'
#' # the legend matches whatever gt_color_ranks() drew
#' gt(head(mtcars[c("mpg", "hp", "wt")], 8)) %>%
#'   gt_color_ranks(columns = mpg) %>%
#'   gt_legend_continuous(columns = mpg, title = "Miles per gallon")
#'
#' # discrete blocks, labeled at every bin edge, title to the left
#' gt(head(airquality, 10)) %>%
#'   gt_color_ranks(columns = Temp) %>%
#'   gt_legend_continuous(
#'     columns = Temp, type = "blocks", n_bins = 5, labels = "edges",
#'     title = "Temp (F)", title_position = "left",
#'     title_style = list(weight = 600, transform = "uppercase",
#'                        spacing = "0.08em", size = "10px"),
#'     labels_style = list(size = "9px", color = "#999999")
#'   )
#' }
#'
#' @seealso [gt_color_ranks()], and [gt_legend_discrete()] for a discrete key.
#' @import gt
#' @importFrom magrittr %>%
#' @export
gt_legend_continuous <- function(gt_object, columns = NULL,
                            palette = c("#3D8B6E", "#9DC5A7", "#EDE0CC", "#DB9070", "#BE4D3A"),
                            domain = NULL, reverse = FALSE, pal_type = "discrete",
                            type = c("continuous", "steps", "blocks"), n_bins = 5,
                            labels = NULL, digits = 0,
                            title = NULL,
                            title_position = c("top", "bottom", "left", "right"),
                            title_style = list(), labels_style = list(),
                            labels_position = c("bottom", "top", "none"),
                            location = c("bottom", "top"),
                            align = c("center", "left", "right"),
                            width = 200, height = 10,
                            border_color = NULL, border_width = 1, radius = 2,
                            gap = 3, title_gap = 4, block_gap = 2) {

  .check_gt(gt_object)
  type <- match.arg(type)
  title_position <- match.arg(title_position)
  labels_position <- match.arg(labels_position)
  location <- match.arg(location)
  align <- match.arg(align)
  if (n_bins < 1) cli::cli_abort("{.arg n_bins} must be at least 1.")

  # a coloring call earlier in the pipe leaves its scale on the table. use it for
  # anything the caller did not give, so the legend cannot drift from the cells
  cols_quo <- rlang::enquo(columns)
  rec <- .recorded_scale(gt_object)
  if (!is.null(rec)) {
    if (missing(palette)) palette <- rec$palette
    if (missing(reverse)) reverse <- rec$reverse
    if (missing(pal_type)) pal_type <- rec$pal_type
    if (is.null(domain)) domain <- rec$domain
    if (rlang::quo_is_null(cols_quo) && !is.null(rec$columns)) {
      cols_quo <- rlang::new_quosure(rlang::expr(tidyselect::all_of(!!rec$columns)))
    }
  }

  pal <- .resolve_palette(palette, pal_type)
  if (isTRUE(reverse)) pal <- rev(pal)

  if (is.null(domain)) {
    if (rlang::quo_is_null(cols_quo)) {
      cli::cli_abort(c(
        "Need either {.arg columns} or {.arg domain} to know what the legend spans.",
        "i" = "Pass the same {.arg columns} you colored with, or color the table \\
               with {.fn gt_color_ranks}, {.fn gt_color_pills} or \\
               {.fn gt_percentile_bar} first and the legend will pick the scale up."
      ))
    }
    col_names <- names(dplyr::select(gt_object[["_data"]], !!cols_quo))
    vals <- suppressWarnings(as.numeric(unlist(gt_object[["_data"]][col_names], use.names = FALSE)))
    if (all(is.na(vals))) {
      cli::cli_abort("No numeric values found in {.arg columns}; pass {.arg domain} directly.")
    }
    domain <- range(vals, na.rm = TRUE)
  }
  if (length(domain) != 2 || !is.numeric(domain)) {
    cli::cli_abort("{.arg domain} must be a length-2 numeric vector.")
  }

  build_css <- function(s) .style_css(s)
  drop_null <- function(x) {
    if (is.null(x)) return(list())
    x[!vapply(x, is.null, logical(1))]
  }
  s_title <- utils::modifyList(list(size = "11px", color = "#666666"), drop_null(title_style))
  s_labels <- utils::modifyList(list(size = "10px", color = "#666666"), drop_null(labels_style))

  # labels
  fmt <- function(x) format(round(x, digits), trim = TRUE, big.mark = ",")
  if (is.null(labels)) {
    labels <- fmt(domain)
  } else if (identical(labels, "edges")) {
    labels <- fmt(seq(domain[1], domain[2], length.out = n_bins + 1))
  }
  labels <- as.character(labels)

  # the bar
  ramp <- scales::col_numeric(palette = pal, domain = c(0, 1))
  seg_border <- if (!is.null(border_color)) {
    sprintf("border:%spx solid %s;", border_width, border_color)
  } else ""

  if (type == "continuous") {
    cols <- ramp(seq(0, 1, length.out = 60))
    segs <- paste0(sprintf("<span style=\"flex:1 0 auto; background-color:%s;\"></span>", cols),
                   collapse = "")
    bar <- sprintf(
      paste0("<div style=\"display:flex; width:%spx; height:%spx; border-radius:%spx;",
             " overflow:hidden; %s\">%s</div>"),
      width, height, radius, seg_border, segs
    )
  } else if (type == "steps") {
    cols <- ramp(seq(0.5 / n_bins, 1 - 0.5 / n_bins, length.out = n_bins))
    segs <- paste0(sprintf("<span style=\"flex:1 0 auto; background-color:%s;\"></span>", cols),
                   collapse = "")
    bar <- sprintf(
      paste0("<div style=\"display:flex; width:%spx; height:%spx; border-radius:%spx;",
             " overflow:hidden; %s\">%s</div>"),
      width, height, radius, seg_border, segs
    )
  } else {
    cols <- ramp(seq(0.5 / n_bins, 1 - 0.5 / n_bins, length.out = n_bins))
    segs <- paste0(sprintf(
      "<span style=\"flex:1 0 auto; height:%spx; background-color:%s; border-radius:%spx; %s\"></span>",
      height, cols, radius, seg_border
    ), collapse = "")
    bar <- sprintf("<div style=\"display:flex; width:%spx; gap:%spx;\">%s</div>",
                   width, block_gap, segs)
  }

  labels_html <- if (labels_position == "none" || length(labels) == 0) {
    NULL
  } else if (length(labels) == 1) {
    sprintf("<div style=\"width:%spx; %s text-align:center;\">%s</div>",
            width, build_css(s_labels), labels)
  } else {
    sprintf(
      "<div style=\"display:flex; width:%spx; justify-content:space-between; %s\">%s</div>",
      width, build_css(s_labels), paste0(sprintf("<span>%s</span>", labels), collapse = "")
    )
  }

  # stack the bar and its labels
  bar_group <- paste0(
    sprintf("<div style=\"display:flex; flex-direction:column; gap:%spx;\">", gap),
    if (identical(labels_position, "top")) paste0(labels_html, bar) else paste0(bar, labels_html),
    "</div>"
  )

  title_html <- if (!is.null(title)) {
    sprintf("<div style=\"%s\">%s</div>", build_css(s_title), title)
  } else NULL

  legend_html <- if (is.null(title_html)) {
    bar_group
  } else if (title_position %in% c("top", "bottom")) {
    sprintf(
      "<div style=\"display:flex; flex-direction:column; align-items:%s; gap:%spx;\">%s</div>",
      switch(align, left = "flex-start", right = "flex-end", "center"), title_gap,
      if (title_position == "top") paste0(title_html, bar_group) else paste0(bar_group, title_html)
    )
  } else {
    sprintf(
      "<div style=\"display:flex; flex-direction:row; align-items:center; gap:%spx;\">%s</div>",
      title_gap,
      if (title_position == "left") paste0(title_html, bar_group) else paste0(bar_group, title_html)
    )
  }

  legend_html <- sprintf(
    "<div style=\"display:flex; justify-content:%s;\">%s</div>",
    switch(align, left = "flex-start", right = "flex-end", "center"), legend_html
  )

  # google font import. has to run after the target cell exists or gt drops it
  fonts <- unique(unlist(c(s_title$font, s_labels$font)))
  apply_fonts <- function(x, loc) {
    for (f in fonts) {
      x <- gt::tab_style(x, style = gt::cell_text(font = gt::google_font(f)), locations = loc)
    }
    x
  }

  if (location == "bottom") {
    out <- gt_object %>% gt::tab_source_note(source_note = gt::html(legend_html))
    if (length(fonts)) out <- apply_fonts(out, gt::cells_source_notes())
    return(out)
  }

  # top: the subtitle slot keeps the order title > subtitle > legend
  heading <- gt_object[["_heading"]]
  has <- function(x) !is.null(x) && length(x) && nzchar(as.character(x))
  old_title <- if (has(heading$title)) as.character(heading$title) else NULL
  old_subtitle <- if (has(heading$subtitle)) as.character(heading$subtitle) else NULL

  if (is.null(old_title)) {
    out <- gt_object %>% gt::tab_header(title = gt::html(legend_html))
    if (length(fonts)) out <- apply_fonts(out, gt::cells_title("title"))
    return(out)
  }

  spacer <- if (is.null(old_subtitle)) "" else "<div style=\"height:4px;\"></div>"
  out <- gt_object %>%
    gt::tab_header(
      title = gt::html(old_title),
      subtitle = gt::html(paste0(old_subtitle, spacer, legend_html))
    )
  if (length(fonts)) out <- apply_fonts(out, gt::cells_title("subtitle"))
  out
}
