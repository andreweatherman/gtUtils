#' Background colors used by the `gtUtils` themes
#'
#' A lookup of the background color each `gt_theme_*` function sets, used to match
#' a saved image's canvas to the table sitting on it. [gt_save_crop()] and
#' [gt_social_crop()] pad the image using their own `bg` argument, and a mismatch
#' shows up as a border around the table.
#'
#' @format A tibble with three columns:
#' \describe{
#'   \item{theme}{The theme function name.}
#'   \item{has_style}{Whether the theme sets additional styling beyond the
#'     background.}
#'   \item{bg}{The background color the theme applies, as a hex code or a color
#'     name.}
#' }
#'
#' @examples
#' \dontrun{
#' # look up the background a theme uses, then match the canvas to it
#' bg <- theme_bg$bg[theme_bg$theme == "gt_theme_gtutils"]
#'
#' gt::gt(head(mtcars)) %>%
#'   gt_theme_gtutils() %>%
#'   gt_save_crop("table.png", bg = bg)
#' }
#'
#' @seealso [gt_save_crop()], [gt_social_crop()].
"theme_bg"
