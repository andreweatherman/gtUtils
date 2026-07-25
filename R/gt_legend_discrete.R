#' Add a discrete color key to a `gt` table
#'
#' Draws a legend of labeled color swatches, the categorical counterpart to
#' [gt_legend_continuous()], which draws a continuous ramp. Give it the colors and
#' labels you used elsewhere in the table, such as the fills from
#' [gt_highlight_cells()] or [gt_indicator_boxes()], and it renders a color key.
#' After [gt_tiers()], which records its mapping on the table, `key_info` can be
#' left out entirely.
#'
#' The key can sit above the table or below it, run in a row or a column, and
#' take square, rounded, or circular swatches. An optional heading and subtitle
#' can ride along above the swatches, each styled through its own list.
#'
#' The key is passed as either a named vector, `c("Home" = "#cce7f5", "Bye" =
#' "#eeeeee")`, where the names are the labels, or a data frame with a `color`
#' column and a `label` column.
#'
#' @section Styling:
#'
#' `heading_style`, `subtitle_style`, and `label_style` are named lists following
#' the same convention as [gt_grid()] and [gt_title_header()]. Recognized keys are
#' `font` (a Google font name), `size`, `color`, `weight`, `italic`, `spacing`
#' (letter spacing), `transform` (such as `"uppercase"`), and `align`, plus
#' `line_height`, `margin_top`, `margin_bottom`, `padding_top`, and
#' `padding_bottom`. Any length takes a number, read as pixels, or a CSS string.
#' Any key left out keeps its default. Colors left unset are derived from the
#' table background, so the key reads on light and dark themes. Apply the theme
#' first, since an unthemed table reports a white background and the heading and
#' subtitle come out near-black on whatever ground the theme sets later.
#'
#' @param gt_object A `gt` table object to modify.
#' @param key_info The key. Either a named character vector of `label = color`,
#'   or a data frame with a `color` column and a `label` column. Defaults to
#'   `NULL`, which takes the key [gt_tiers()] recorded on the table.
#' @param heading Optional. A heading shown above the key. Defaults to `NULL`, no
#'   heading.
#' @param subtitle Optional. A subtitle shown under the heading. Defaults to
#'   `NULL`, no subtitle.
#' @param label_placement Character. Either `"outside"` to set each label beside
#'   its swatch, or `"inside"` to print it on the swatch. Defaults to `"outside"`.
#' @param location Character. Where the key goes. `"top"` places it in the header;
#'   `"bottom"` adds it as a source note. When `location` is `"top"` and no
#'   `heading` or `subtitle` is given, any existing title and subtitle are kept
#'   and the key is appended. Defaults to `"top"`.
#' @param shape Character. The swatch shape, one of `"square"`, `"rounded"`, or
#'   `"circle"`. Defaults to `"square"`.
#' @param swatch_size Numeric. The swatch size in pixels. Defaults to `14`.
#' @param border Logical. Should a hairline be drawn around each swatch? This
#'   keeps a pale swatch visible against the page. Defaults to `TRUE`.
#' @param border_color Optional. A hex color for the hairline, applied to every
#'   swatch. When `NULL`, each swatch takes its own edge, a soft darker shade of
#'   its fill, so pale swatches stay defined without a harsh uniform outline.
#'   Defaults to `NULL`.
#' @param border_width Numeric. The hairline width in pixels. Defaults to `1`.
#' @param gap Numeric. The space between keys in pixels. Defaults to `14`.
#' @param direction Character. Whether the keys run `"horizontal"` in a row or
#'   `"vertical"` in a column. Defaults to `"horizontal"`.
#' @param align Character. The alignment of the whole key, one of `"center"`,
#'   `"left"`, or `"right"`. Defaults to `"center"`.
#' @param heading_style A named list styling the heading. See Styling. Defaults to
#'   an empty list.
#' @param subtitle_style A named list styling the subtitle. See Styling. Defaults
#'   to an empty list.
#' @param label_style A named list styling the swatch labels. See Styling. For
#'   `"inside"` labels the color is chosen per swatch for contrast and any `color`
#'   here is ignored. Defaults to an empty list.
#'
#' @details
#' When `label_placement` is `"inside"`, each label takes black or white by
#' whichever contrasts better with its swatch, so a `color` in `label_style` is
#' ignored in that mode.
#'
#' [gt_title_header()] and anything else calling `gt::tab_header()` replaces the
#' whole header. With `location = "top"`, add those first or they will overwrite
#' the key.
#'
#' @returns Returns a modified `gt` table with the key added.
#'
#' @examples
#' \dontrun{
#' library(gt)
#'
#' # a named vector is the quickest way in
#' gt(head(mtcars[c("mpg", "cyl", "hp")], 6)) %>%
#'   gt_legend_discrete(c("Efficient" = "#CCE7F5", "Thirsty" = "#F5CCCC"))
#'
#' # rounded swatches below the table, labels on the swatches
#' gt(head(mtcars[c("mpg", "cyl", "hp")], 6)) %>%
#'   gt_legend_discrete(
#'     c("Yes" = "#FCCF10", "No" = "#EEEEEE"),
#'     heading = "Qualified",
#'     label_placement = "inside",
#'     location = "bottom",
#'     shape = "rounded"
#'   )
#'
#' # style the heading and labels through lists, the same way gt_grid() does
#' gt(head(mtcars[c("mpg", "cyl", "hp")], 6)) %>%
#'   gt_legend_discrete(
#'     c("Home" = "#CCE7F5", "Away" = "#FFFFFF", "Bye" = "#EEEEEE"),
#'     heading = "2025 Schedule",
#'     heading_style = list(font = "Oswald", size = 20, transform = "uppercase"),
#'     label_style = list(size = 13, color = "#444444")
#'   )
#' }
#'
#' @seealso [gt_legend_continuous()] for a continuous ramp.
#' @import gt
#' @importFrom magrittr %>%
#' @export
gt_legend_discrete <- function(gt_object, key_info = NULL, heading = NULL, subtitle = NULL,
                               label_placement = c("outside", "inside"),
                               location = c("top", "bottom"),
                               shape = c("square", "rounded", "circle"),
                               swatch_size = 14, border = TRUE, border_color = NULL,
                               border_width = 1, gap = 14,
                               direction = c("horizontal", "vertical"),
                               align = c("center", "left", "right"),
                               heading_style = list(), subtitle_style = list(),
                               label_style = list()) {

  .check_gt(gt_object)
  label_placement <- match.arg(label_placement)
  location <- match.arg(location)
  shape <- match.arg(shape)
  direction <- match.arg(direction)
  align <- match.arg(align)

  # [gt_tiers()] leaves its level = color mapping on the table, so a key can be
  # asked for with no arguments at all
  if (is.null(key_info)) key_info <- .recorded_key(gt_object)
  if (is.null(key_info)) {
    cli::cli_abort(c(
      "{.arg key_info} is missing, with no key recorded on the table.",
      "i" = "Pass a named vector such as {.code c(\"Home\" = \"#cce7f5\")}, or call \\
             {.fn gt_tiers} first and the key will be picked up."
    ))
  }

  # accept a named vector (label = color) or a data frame of color + label
  if (is.data.frame(key_info)) {
    if (all(c("color", "label") %in% names(key_info))) {
      colors <- as.character(key_info$color)
      labels <- as.character(key_info$label)
    } else if (ncol(key_info) >= 2) {
      colors <- as.character(key_info[[1]])
      labels <- as.character(key_info[[2]])
    } else {
      cli::cli_abort("A data frame {.arg key_info} needs a {.field color} and a {.field label} column.")
    }
  } else if (is.atomic(key_info) && !is.null(names(key_info))) {
    labels <- names(key_info)
    colors <- as.character(unname(key_info))
  } else {
    cli::cli_abort(c(
      "{.arg key_info} must be a named vector or a data frame.",
      "i" = "Try {.code c(\"Home\" = \"#cce7f5\", \"Bye\" = \"#eeeeee\")}."
    ))
  }

  colors <- .hex6(colors)

  # tones off the table background. an unthemed table carries "" here, not NA
  opt <- gt_object[["_options"]]
  bg <- opt$value[opt$parameter == "table_background_color"]
  bg <- if (length(bg)) as.character(bg[[1]]) else NA_character_
  valid <- !is.na(bg) && nzchar(bg) &&
    isTRUE(tryCatch({ grDevices::col2rgb(bg); TRUE }, error = function(e) FALSE))
  if (!valid) bg <- "#FFFFFF"
  ink <- .theme_on_color(bg)

  # per-element defaults, user lists over the top
  s_heading <- .style_merge(list(size = 16, weight = 600, color = ink), heading_style)
  s_subtitle <- .style_merge(list(size = 13, weight = 400, color = .theme_secondary_on(bg, ink)), subtitle_style)
  s_label <- .style_merge(list(size = 12, color = ink), label_style)

  # a darker shade of each swatch, so chips are defined without a hard outline
  swatch_border <- if (is.null(border_color)) {
    vapply(colors, function(cc) .theme_mix("#000000", cc, 0.18), character(1))
  } else {
    rep(border_color, length(colors))
  }
  brd <- if (isTRUE(border)) sprintf("border:%spx solid %s;", border_width, swatch_border) else rep("", length(colors))

  radius <- switch(shape, square = 0, rounded = round(swatch_size / 4), circle = round(swatch_size / 2))

  # one key: a swatch beside its label, or a label printed on the swatch
  items <- if (label_placement == "inside") {
    on <- vapply(colors, .theme_on_color, character(1))
    vapply(seq_along(labels), function(i) sprintf(
      paste0("<span style=\"display:inline-block; padding:2px 9px; background-color:%s;",
             " border-radius:%spx; %s %s\">%s</span>"),
      colors[i], radius, brd[i], .style_css(utils::modifyList(s_label, list(color = on[i]))), labels[i]
    ), character(1))
  } else {
    label_css <- .style_css(s_label)
    vapply(seq_along(labels), function(i) sprintf(
      paste0("<span style=\"display:inline-flex; align-items:center; gap:6px;\">",
             "<span style=\"display:inline-block; width:%spx; height:%spx; background-color:%s;",
             " border-radius:%spx; %s\"></span>",
             "<span style=\"%s\">%s</span></span>"),
      swatch_size, swatch_size, colors[i], radius, brd[i], label_css, labels[i]
    ), character(1))
  }

  justify <- switch(align, left = "flex-start", right = "flex-end", "center")
  key_html <- sprintf(
    paste0("<div style=\"display:flex; flex-wrap:wrap; gap:%spx; flex-direction:%s; %s\">%s</div>"),
    gap,
    if (direction == "vertical") "column" else "row",
    if (direction == "vertical") sprintf("align-items:%s;", justify)
    else sprintf("justify-content:%s; align-items:center;", justify),
    paste(items, collapse = "")
  )

  # optional heading + subtitle stacked above the key
  head_html <- ""
  if (!is.null(heading)) {
    head_html <- paste0(head_html, sprintf("<div style=\"%s\">%s</div>", .style_css(s_heading), heading))
  }
  if (!is.null(subtitle)) {
    head_html <- paste0(head_html, sprintf("<div style=\"%s\">%s</div>", .style_css(s_subtitle), subtitle))
  }

  content <- sprintf(
    "<div style=\"display:flex; flex-direction:column; align-items:%s; gap:6px;\">%s%s</div>",
    justify, head_html, key_html
  )

  # google fonts through gt so the inline font-family refs resolve. needs the
  # target cell to already exist
  fonts <- .style_fonts(s_heading, s_subtitle, s_label)
  apply_fonts <- function(x, loc) {
    for (f in fonts) x <- gt::tab_style(x, style = gt::cell_text(font = gt::google_font(f)), locations = loc)
    x
  }

  if (location == "bottom") {
    out <- gt_object %>% gt::tab_source_note(source_note = gt::html(content))
    return(apply_fonts(out, gt::cells_source_notes()))
  }

  # top with a heading/subtitle: the block takes the title slot
  if (!is.null(heading) || !is.null(subtitle)) {
    out <- gt_object %>% gt::tab_header(title = gt::html(content))
    return(apply_fonts(out, gt::cells_title("title")))
  }

  # top, key only: keep any existing header and ride in the subtitle slot
  hd <- gt_object[["_heading"]]
  has <- function(x) !is.null(x) && length(x) && nzchar(as.character(x))
  old_title <- if (has(hd$title)) as.character(hd$title) else NULL
  old_subtitle <- if (has(hd$subtitle)) as.character(hd$subtitle) else NULL

  if (is.null(old_title)) {
    out <- gt_object %>% gt::tab_header(title = gt::html(content))
    return(apply_fonts(out, gt::cells_title("title")))
  }

  spacer <- if (is.null(old_subtitle)) "" else "<div style=\"height:4px;\"></div>"
  out <- gt_object %>%
    gt::tab_header(
      title = gt::html(old_title),
      subtitle = gt::html(paste0(old_subtitle, spacer, content))
    )
  apply_fonts(out, gt::cells_title("subtitle"))
}
