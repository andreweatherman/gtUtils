#' Save a `gt` table to an image and trim its whitespace
#'
#' Renders a table to a PNG, JPG, or JPEG, trims the whitespace `gtExtras` leaves
#' around it, and pads a uniform border back on. Saving runs through
#' `gtExtras::gtsave_extra()` and the trimming and padding through `magick`.
#'
#' @param data A `gt` table object to save.
#' @param file Character. A path ending in `.png`, `.jpg`, or `.jpeg` to write
#'   the image to. Defaults to `NULL`, which returns the image without writing it.
#' @param bg Character. The background color of the padded border. Defaults to
#'   `"white"`.
#' @param whitespace Numeric. The border, in pixels, left around the trimmed
#'   table. Defaults to `50`.
#' @param zoom Numeric. The rendering zoom factor passed to the underlying save.
#'   A higher value gives a sharper image. Defaults to `2`.
#' @param expand Numeric. The pixel expansion passed to the underlying save.
#'   Defaults to `5`.
#' @param width Optional. A final output width in pixels. The image is scaled to
#'   it, height following, so a series of tables can share one width. Defaults to
#'   `NULL`, which leaves the rendered width alone.
#'
#' @details
#' The table is first written to a temporary file at `zoom` and `expand`, then
#' read back with `magick`, trimmed to its content, and given a `whitespace`
#' border in `bg`. When `width` is set the finished image is resized to that
#' width with the height following, so several tables saved at the same `width`
#' line up when posted together. The temporary file is removed afterward.
#'
#' @returns Writes the cropped image to `file`.
#'
#' @examples
#' \dontrun{
#' library(gt)
#'
#' gt(head(mtcars)) %>%
#'   gt_theme_broadsheet() %>%
#'   gt_save_crop("mtcars.png")
#'
#' # pin a shared width across a series
#' gt(head(iris)) %>%
#'   gt_save_crop("iris.png", width = 900, bg = "#FBFAF7")
#' }
#'
#' @seealso [gt_social_crop()] for padding onto a fixed-ratio canvas.
#' @import magick
#' @importFrom gtExtras gtsave_extra
#' @importFrom glue glue
#' @importFrom magrittr %>%
#'
#' @export
gt_save_crop <- function(data, file = NULL, bg = "white", whitespace = 50, zoom = 2, expand = 5,
                         width = NULL) {

  .check_gt(data, arg = "data")

  tmp <- tempfile(fileext = ".png")
  gtExtras::gtsave_extra(data, tmp, zoom = zoom, expand = expand)

  img <- magick::image_read(tmp) %>%
    magick::image_trim() %>%
    magick::image_border(bg, glue::glue('{whitespace}x{whitespace}'))

  # pin the width so a posted series lines up, height scales with it
  if (!is.null(width)) {
    img <- magick::image_resize(img, glue::glue("{width}x"))
  }

  magick::image_write(img, file)
  unlink(tmp)
}
