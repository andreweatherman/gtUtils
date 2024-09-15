#' Crop `gt` tables
#'
#' Programatically save and crop `gt` tables using `magick`. Uses `gtsave_extra`
#' to save the image and `magick` to crop it.
#'
#' @returns Returns a cropped image file
#' @param file A valid path to a .png, .jpg, or .jpeg to crop
#' @param whitespace A numeric representing the amount of whitespace to leave
#'   around the image
#' @param bg The background color of the added trimming (defaults to white)
#' @import magick
#' @importFrom gtExtras gtsave_extra
#' @importFrom glue glue
#' @importFrom magrittr %>%
#'
#' @export
gt_save_crop <- function(data, file = NULL, bg = "white", whitespace = 50, zoom = 2, expand = 5) {

  tmp <- tempfile(fileext = ".png")
  gtExtras::gtsave_extra(data, tmp, zoom = zoom, expand = expand)

  magick::image_read(tmp) %>%
    magick::image_trim() %>%
    magick::image_border(bg, glue::glue('{whitespace}x{whitespace}')) %>%
    magick::image_write(file)

  unlink(tmp)
}
