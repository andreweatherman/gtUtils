#' Save a `gt` table onto a fixed-ratio canvas
#'
#' A variant of [gt_save_crop()] that centers the trimmed table on a canvas of a
#' given aspect ratio, for posting to social platforms. The table itself is never
#' cropped; the short side of the canvas is expanded until the ratio is met.
#'
#' @param data A `gt` table object to save.
#' @param file Optional. A path to write the image to. Defaults to `NULL`.
#' @param aspect_ratio Character or numeric. The target ratio, given as `"1:1"`,
#'   `"16:9"`, `"4:5"`, or a number. Defaults to `"1:1"`.
#' @param bg Character. The background color of the canvas. Defaults to `"white"`.
#' @param whitespace Numeric. The amount of whitespace to leave around the table
#'   before padding out to the ratio. Defaults to `60`.
#' @param gravity Character. Where the table sits on the canvas, passed to
#'   `magick::image_extent`. Defaults to `"center"`.
#' @param zoom Numeric. The rendering zoom factor. Defaults to `2`.
#' @param expand Numeric. The pixel expansion passed to the underlying save.
#'   Defaults to `5`.
#' @param width Optional. A final output width in pixels. The finished canvas is
#'   scaled to it, keeping the aspect ratio, so a series of posts can share one
#'   width. Defaults to `NULL`.
#'
#' @returns Returns the cropped and padded image file.
#'
#' @examples
#' \dontrun{
#' library(gt)
#'
#' gt(head(mtcars)) %>%
#'   gt_theme_broadsheet(density = "social") %>%
#'   gt_social_crop("mtcars.png", aspect_ratio = "4:5")
#'
#' # match the canvas to a dark theme
#' gt(head(mtcars)) %>%
#'   gt_theme_midnight() %>%
#'   gt_social_crop("dark.png", bg = "#0C0D10")
#' }
#'
#' @seealso [gt_save_crop()] for a plain trimmed save.
#' @import magick
#' @importFrom magrittr %>%
#' @export
gt_social_crop <- function(data, file = NULL, aspect_ratio = "1:1", bg = "white",
                           whitespace = 60, gravity = "center", zoom = 2, expand = 5,
                           width = NULL) {

  .check_gt(data, arg = "data")

  # parse aspect ratio
  ar <- suppressWarnings(
    if (is.character(aspect_ratio) && grepl("[:x]", aspect_ratio)) {
      parts <- as.numeric(strsplit(aspect_ratio, "[:x]")[[1]])
      parts[1] / parts[2]
    } else {
      as.numeric(aspect_ratio)
    }
  )

  if (length(ar) != 1 || is.na(ar) || ar <= 0) {
    cli::cli_abort(c(
      "{.arg aspect_ratio} could not be read as a ratio.",
      "x" = "Got {.val {aspect_ratio}}.",
      "i" = 'Use {.val 1:1}, {.val 16:9}, {.val 4x5}, or a number like {.val 1.91}.'
    ))
  }

  tmp <- tempfile(fileext = ".png")
  gtExtras::gtsave_extra(data, tmp, zoom = zoom, expand = expand)

  img <- magick::image_read(tmp) %>%
    magick::image_trim() %>%
    magick::image_border(bg, glue::glue("{whitespace}x{whitespace}"))

  info <- magick::image_info(img)
  w <- info$width[[1]]
  h <- info$height[[1]]

  # expand the short side so the table never gets cropped
  if (w / h > ar) {
    target_w <- w
    target_h <- round(w / ar)
  } else {
    target_h <- h
    target_w <- round(h * ar)
  }

  img <- magick::image_extent(
    img,
    geometry = glue::glue("{target_w}x{target_h}"),
    gravity = gravity,
    color = bg
  )

  # scale the finished canvas so a series shares one width, ratio held
  if (!is.null(width)) {
    img <- magick::image_resize(img, glue::glue("{width}x"))
  }

  out <- magick::image_write(img, file)
  unlink(tmp)
  invisible(out)
}
