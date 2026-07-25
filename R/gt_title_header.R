#' Build a styled header block for a `gt` table
#'
#' Wraps `gt::tab_header()` to produce a fuller header: an optional kicker above
#' the title, the title, an optional subtitle, and an optional date. Each of the
#' four is styled independently through its own list, so the typography can be set
#' without writing the `tab_style()` calls yourself.
#'
#' @param gt_object A `gt` table object to modify.
#' @param title Character. The title text.
#' @param subtitle Optional. The subtitle text. Defaults to `NULL`.
#' @param kicker Optional. A short line above the title. Defaults to `NULL`.
#' @param date Optional. A date shown beneath the subtitle. A `Date` is formatted
#'   as, for example, "July 21, 2026"; anything else is shown as given. Defaults
#'   to `NULL`.
#' @param kicker_style A named list styling the kicker. See Styling. Defaults to
#'   an empty list.
#' @param title_style A named list styling the title. Defaults to an empty list.
#' @param subtitle_style A named list styling the subtitle. Defaults to an empty
#'   list.
#' @param date_style A named list styling the date. Defaults to an empty list.
#'
#' @section Styling:
#'
#' Each `*_style` argument takes a named list. Recognized keys are `font` (a
#' Google font name), `size`, `color`, `weight`, `italic`, `spacing` (letter
#' spacing), `transform` (such as `"uppercase"`), and `align`, plus `line_height`,
#' `margin_top`, `margin_bottom`, `padding_top`, and `padding_bottom` for nudging
#' the blocks closer together or further apart. Any key you leave out keeps its
#' default. Lengths take a number, read as pixels, or a CSS string such as
#' `"2rem"`. This is the same convention used by [gt_grid()], [gt_legend_continuous()],
#' and [gt_legend_discrete()].
#'
#' @returns Returns a modified `gt` table with the header block applied.
#'
#' @details
#' Google fonts named in the style lists are loaded through `gt`'s own machinery,
#' so they survive `gtsave()` as well as the HTML output.
#'
#' `gt_title_header()` calls `gt::tab_header()`, which replaces the whole header.
#' Paired with [gt_legend_continuous()] at `location = "top"`, call this one
#' first or it will overwrite the legend.
#'
#' @examples
#' \dontrun{
#' library(gt)
#'
#' gt(head(mtcars[c("mpg", "hp", "wt")], 6)) %>%
#'   gt_title_header(
#'     title = "Fuel economy and power",
#'     subtitle = "A sample of the 1974 Motor Trend road tests",
#'     kicker = "Motor Trend",
#'     date = as.Date("2026-07-21"),
#'     kicker_style = list(color = "#0054AD", size = "0.8em",
#'                         transform = "uppercase", spacing = "0.1em"),
#'     title_style = list(font = "Libre Franklin", weight = 800, size = "26px"),
#'     subtitle_style = list(color = "#666666", italic = TRUE),
#'     date_style = list(color = "#B8232F", weight = 600)
#'   )
#' }
#'
#' @seealso [gt_538_caption()] for the footer equivalent.
#' @import gt
#' @importFrom magrittr %>%
#' @export
gt_title_header <- function(gt_object, title, subtitle = NULL, kicker = NULL, date = NULL,
                            kicker_style = list(), title_style = list(),
                            subtitle_style = list(), date_style = list()) {

  .check_gt(gt_object)

  # per-element defaults, user lists override
  defaults <- list(
    kicker = list(size = "0.75em", weight = 700, color = "#C84630",
                  transform = "uppercase", spacing = "0.08em", italic = FALSE,
                  font = NULL, align = NULL),
    title = list(size = NULL, weight = NULL, color = NULL, transform = NULL,
                 spacing = NULL, italic = FALSE, font = NULL, align = NULL),
    subtitle = list(size = NULL, weight = NULL, color = NULL, transform = NULL,
                    spacing = NULL, italic = FALSE, font = NULL, align = NULL),
    date = list(size = "0.85em", weight = 400, color = "#8A8A8A", transform = NULL,
                spacing = NULL, italic = FALSE, font = NULL, align = NULL)
  )
  s_kicker <- utils::modifyList(defaults$kicker, kicker_style)
  s_title <- utils::modifyList(defaults$title, title_style)
  s_subtitle <- utils::modifyList(defaults$subtitle, subtitle_style)
  s_date <- utils::modifyList(defaults$date, date_style)

  # no font_fallback, so an unstyled element inherits the theme's font
  build_css <- function(s) .style_css(s)

  # double-quoted attrs, since build_css() emits single-quoted font names
  kicker_html <- if (!is.null(kicker)) {
    sprintf('<div style="%smargin-bottom:0.15em;">%s</div>', build_css(s_kicker), kicker)
  } else ""
  title_full <- paste0(kicker_html, sprintf('<div style="%s">%s</div>', build_css(s_title), title))

  subtitle_html <- if (!is.null(subtitle)) {
    sprintf('<div style="%s">%s</div>', build_css(s_subtitle), subtitle)
  } else ""
  date_html <- if (!is.null(date)) {
    date_str <- if (inherits(date, "Date")) format(date, "%B %d, %Y") else as.character(date)
    sprintf('<div style="%smargin-top:0.15em;">%s</div>', build_css(s_date), date_str)
  } else ""
  subtitle_full <- paste0(subtitle_html, date_html)
  subtitle_arg <- if (nzchar(subtitle_full)) gt::html(subtitle_full) else NULL

  # google fonts through gt so the inline font-family refs resolve. the title
  # cell is just somewhere to hang the import
  fonts <- unique(c(s_kicker$font, s_title$font, s_subtitle$font, s_date$font))
  fonts <- fonts[!vapply(fonts, is.null, logical(1))]
  for (f in unlist(fonts)) {
    gt_object <- gt_object %>%
      gt::tab_style(
        style = gt::cell_text(font = gt::google_font(f)),
        locations = gt::cells_title("title")
      )
  }

  gt_object %>%
    gt::tab_header(title = gt::html(title_full), subtitle = subtitle_arg)
}
