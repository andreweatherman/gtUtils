#' Add social handles to a `gt` table
#'
#' Renders a set of accounts, each an icon plus a handle, into the table's source
#' note, styled like the bottom line of [gt_538_caption()]. Icons come from Font
#' Awesome through the `fontawesome` package.
#'
#' If a `caption` is supplied it becomes the top line, a bordered footnote in the
#' same manner as [gt_538_caption()], with the handles beneath it. With no
#' caption, only the handle line is shown.
#'
#' @param gt_object A `gt` table object to modify.
#' @param accounts A named character vector mapping platform to handle, such as
#'   `c(x = "@you", bluesky = "you.bsky.social")`. Names are matched to Font
#'   Awesome brand icons, and friendly aliases are accepted, including `x` and
#'   `twitter`, `ig` and `instagram`, `bsky` and `bluesky`, `gh` and `github`,
#'   `yt` and `youtube`, `web` and `website`, and `email`. An unrecognized name is
#'   used as the icon name directly.
#' @param caption Optional. A caption rendered as the top footnote line. Defaults
#'   to `NULL`, which shows only the handle line.
#' @param stack Logical. Should the accounts be stacked vertically, one per line,
#'   instead of sitting in a row? Defaults to `FALSE`.
#' @param separator Character. The string placed between accounts when not
#'   stacked. Defaults to `" | "`.
#' @param align Character. The alignment of the handle line. One of `"right"`,
#'   `"center"`, or `"left"`. Defaults to `"right"`.
#' @param icon_color Optional. A hex color for the icons. If `NULL`, they inherit
#'   the surrounding text color. Defaults to `NULL`.
#' @param icon_height Character. The height of the icons, as a CSS size. Given in
#'   `em` it scales with `text_size`. Defaults to `"0.9em"`.
#' @param text_size Optional. The font size for the handles, as a CSS size. If
#'   `NULL`, it inherits the source-note size. Defaults to `NULL`.
#' @param text_weight Optional. The font weight for the handles. If `NULL`, it
#'   inherits the source-note weight. Defaults to `NULL`.
#' @param ... Additional arguments passed to [gt_538_caption()], used only when a
#'   `caption` is supplied.
#'
#' @returns Returns a modified `gt` table with the handles, and any caption, added.
#'
#' @details
#' If an icon name is not present in your installed version of `fontawesome`, the
#' function stops with a message naming the icon and the version, instead of
#' rendering a broken glyph.
#'
#' @examples
#' \dontrun{
#' library(gt)
#'
#' gt(head(mtcars)) %>%
#'   gt_social_tag(c(x = "@yourhandle", gh = "yourname"))
#'
#' # with a caption line above, stacked and left-aligned
#' gt(head(mtcars)) %>%
#'   gt_social_tag(
#'     c(x = "@yourhandle", web = "example.com"),
#'     caption = "Data: R built-in datasets",
#'     stack = TRUE, align = "left"
#'   )
#' }
#'
#' @seealso [gt_538_caption()].
#' @import gt
#' @importFrom magrittr %>%
#' @export
gt_social_tag <- function(gt_object, accounts, caption = NULL, stack = FALSE,
                          separator = " | ", align = "right",
                          icon_color = NULL, icon_height = "0.9em",
                          text_size = NULL, text_weight = NULL, ...) {

  .check_gt(gt_object)
  if (!length(accounts) || is.null(names(accounts)) || any(!nzchar(names(accounts)))) {
    cli::cli_abort(c(
      "{.arg accounts} must be a named vector of {.code platform = handle}.",
      "i" = "The names pick the icon, the values are printed beside it.",
      "i" = 'Try {.code c(x = "@andreweatherman", gh = "andreweatherman")}.'
    ))
  }

  fill <- if (is.null(icon_color)) "currentColor" else icon_color

  # icon + handle, centered with inline flexbox
  items <- .social_items(accounts, fill = fill, icon_height = icon_height)

  joined <- if (stack) paste(items, collapse = "<br>") else paste(items, collapse = separator)

  container_style <- paste0(
    "text-align:", align, ";",
    if (!is.null(text_size)) paste0(" font-size:", text_size, ";") else "",
    if (!is.null(text_weight)) paste0(" font-weight:", text_weight, ";") else ""
  )
  social_html <- sprintf("<div style='%s'>%s</div>", container_style, joined)

  if (!is.null(caption)) {
    gt_538_caption(gt_object, top_caption = caption, bottom_caption = social_html, ...)
  } else {
    gt_object %>%
      gt::tab_source_note(source_note = gt::html(social_html))
  }
}
