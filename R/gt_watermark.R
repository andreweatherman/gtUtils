#' Put a watermark behind a `gt` table
#'
#' Sets a wordmark or a logo behind the table body, faint enough to read through.
#'
#' @param gt_object A `gt` table object to modify.
#' @param text Optional. Text to render as the watermark. Defaults to `NULL`.
#' @param image Optional. A path to a PNG, JPEG, or SVG file to use instead of
#'   `text`. Defaults to `NULL`.
#' @param opacity Numeric. How faint the watermark is, from 0 to 1. Defaults to
#'   `0.06`.
#' @param size Character. The width of the watermark relative to the table, as a
#'   CSS background size such as `"60%"`. Defaults to `"60%"`.
#' @param position Character. Where it sits, as a CSS background position such as
#'   `"center"` or `"right bottom"`. Defaults to `"center"`.
#' @param color Character. The text color, used only with `text`. Defaults to
#'   `"#000000"`.
#' @param angle Numeric. Rotation in degrees, used only with `text`. Defaults to
#'   `0`.
#' @param font Character. The font family for `text`. Defaults to a system
#'   sans-serif.
#'
#' @details
#' A text watermark is drawn as an inline SVG rather than as HTML, because a
#' background image is the only way to sit behind the table body without taking
#' up a row. An SVG background cannot load the page's webfonts, so `font` has to
#' name a face installed on the machine rendering the table.
#'
#' The watermark sits behind the body cells, so it will be hidden wherever a cell
#' has its own fill, from row striping or from `gt::data_color()`.
#'
#' @returns Returns a modified `gt` table with a watermark behind the body.
#'
#' @examples
#' \dontrun{
#' library(gt)
#'
#' gt(head(mtcars)) %>% gt_watermark(text = "DRAFT", angle = -30, opacity = 0.08)
#'
#' gt(head(mtcars)) %>%
#'   gt_watermark(image = "logo.png", size = "40%", position = "right bottom")
#' }
#'
#' @seealso [gt_social_tag()] for visible attribution in the source note.
#' @import gt
#' @importFrom magrittr %>%
#' @export
gt_watermark <- function(gt_object, text = NULL, image = NULL, opacity = 0.06,
                         size = "60%", position = "center", color = "#000000",
                         angle = 0, font = "Helvetica, Arial, sans-serif") {

  .check_gt(gt_object)

  if (is.null(text) && is.null(image)) {
    cli::cli_abort("Supply either {.arg text} or {.arg image}.")
  }
  if (!is.null(text) && !is.null(image)) {
    cli::cli_abort("Supply only one of {.arg text} or {.arg image}.")
  }

  res <- .table_id(gt_object)
  gt_object <- res$object
  table_id <- res$id

  tall <- FALSE
  uri <- if (!is.null(text)) {
    wm <- .watermark_svg(text, color = color, opacity = opacity, angle = angle, font = font)
    tall <- wm$tall
    wm$uri
  } else {
    if (!file.exists(image)) cli::cli_abort("Can't find {.file {image}}.")
    mime <- switch(tolower(tools::file_ext(image)),
                   png = "image/png", jpg = , jpeg = "image/jpeg",
                   svg = "image/svg+xml", gif = "image/gif",
                   cli::cli_abort("{.arg image} must be a PNG, JPEG, SVG, or GIF."))
    paste0("data:", mime, ";base64,",
           base64enc::base64encode(image))
  }

  # a text mark bakes fill-opacity into the svg; an image needs it on the wrapper
  extra <- if (!is.null(image)) paste0(" opacity: ", opacity, ";") else ""

  gt_object %>%
    gt::opt_css(paste0(
      "#", table_id, " .gt_table_body {",
      " background-image: url('", uri, "');",
      " background-repeat: no-repeat;",
      " background-position: ", position, ";",
      # size on whichever dimension binds. a steeply rotated wordmark is taller
      # than it is wide, and sizing by width runs it off the bottom
      " background-size: ", if (tall) paste("auto", size) else paste(size, "auto"), ";",
      extra,
      " }"
    ))
}

# inline svg wordmark, url-encoded rather than base64 to keep it readable
.watermark_svg <- function(text, color, opacity, angle, font) {
  esc <- function(x) {
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    gsub(">", "&gt;", x, fixed = TRUE)
  }
  label <- esc(as.character(text))

  size <- 100
  text_w <- max(nchar(label) * size * 0.62, size)
  text_h <- size * 1.3

  # rotating clips the corners, so size the canvas to the rotated bounding box
  rad <- abs(angle) * pi / 180
  w <- ceiling(text_w * cos(rad) + text_h * sin(rad)) + 4
  h <- ceiling(text_w * sin(rad) + text_h * cos(rad)) + 4

  rot <- if (angle != 0) sprintf(" transform=\"rotate(%s %s %s)\"", angle, w / 2, h / 2) else ""

  svg <- sprintf(
    paste0("<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 %s %s\" width=\"%s\" height=\"%s\">",
           "<text x=\"50%%\" y=\"50%%\" text-anchor=\"middle\" dominant-baseline=\"central\" ",
           "font-family=\"%s\" font-size=\"%s\" font-weight=\"700\" fill=\"%s\" fill-opacity=\"%s\"%s>%s</text>",
           "</svg>"),
    w, h, w, h, font, size, color, opacity, rot, label
  )

  list(
    uri = paste0("data:image/svg+xml,", utils::URLencode(svg, reserved = TRUE)),
    tall = h > w
  )
}
