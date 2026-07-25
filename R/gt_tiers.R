#' Color and bold the tier column of a `gt` table
#'
#' Fills the cells of a `tier` column by tier level and bolds them, pairing each
#' level with a color. The remaining columns are rendered as images, since the
#' intended layout is a tier list with logos or headshots ranked into colored
#' bands.
#'
#' @param gt_object A `gt` table object to modify.
#' @param levels Character. The tier levels, matching the values in the table's
#'   tier column. May instead be a single named vector of `level = color`, in
#'   which case `colors` is left unset. That is the same shape
#'   [gt_legend_discrete()] takes, so one object can drive both.
#' @param colors Character. Hex color codes paired with `levels`, in the same
#'   order. Must be the same length as `levels`. Defaults to `NULL`, which
#'   requires `levels` to carry the colors as names.
#' @param style Character. The color scheme, passed to [gt_theme_tier()]. Either
#'   `"dark"` for a near-black ground or `"light"` for a white one. Defaults to
#'   `"dark"`.
#' @param img_height Character. The height of the images rendered in the image
#'   columns, as a CSS size. Defaults to `"55px"`.
#' @param tier_column Character. The name of the column holding the tier levels.
#'   Defaults to `"tier"`.
#' @param image_columns Optional. The columns to render as images. When `NULL`,
#'   every column other than `tier_column` is rendered as images. Defaults to
#'   `NULL`.
#'
#' @details
#' The theme is applied once with [gt_theme_tier()], the image columns are passed
#' through `gt::fmt_image()` at `img_height`, and all column labels are cleared,
#' so the input for those columns must be image paths or URLs. The function then
#' reduces over `levels`, and for each level fills the matching `tier_column`
#' cells with the paired color and sets their text to black or white, whichever
#' measures higher contrast against that fill, so each band keeps a legible
#' label. Naming `image_columns` leaves the rest untouched, so a text column can
#' sit alongside the images.
#'
#' @returns Returns a modified `gt` table with the tier column colored and bolded.
#'
#' @examples
#' \dontrun{
#' library(gt)
#'
#' standings <- data.frame(
#'   tier = c("A", "A", "B"),
#'   logo = c("https://example.com/1.png",
#'            "https://example.com/2.png",
#'            "https://example.com/3.png")
#' )
#'
#' gt(standings) %>%
#'   gt_tiers(levels = c("A", "B"), colors = c("#1B7837", "#B2182B"))
#' }
#'
#' @import gt
#' @export
gt_tiers <- function(gt_object, levels, colors = NULL, style = "dark",
                     img_height = "55px", tier_column = "tier",
                     image_columns = NULL) {

  .check_gt(gt_object)

  # a named vector of tier = color says the same thing in one object, and is the
  # shape gt_legend_discrete() already takes, so one mapping can feed both
  if (is.null(colors) && !is.null(names(levels))) {
    colors <- unname(levels)
    levels <- names(levels)
  } else if (is.null(colors)) {
    cli::cli_abort(c(
      "{.arg colors} is missing, with no default.",
      "i" = "Pass {.arg levels} and {.arg colors} as two vectors, or a single \\
             named vector such as {.code c(A = \"#C84630\", B = \"#5DA271\")}."
    ))
  }

  if (length(levels) != length(colors)) {
    cli::cli_abort(c(
      "{.arg levels} and {.arg colors} must be the same length.",
      "x" = "Got {length(levels)} level{?s} and {length(colors)} color{?s}.",
      "i" = "Each tier takes one fill, in the order the tiers should read."
    ))
  }

  data <- gt_object[["_data"]]

  if (!tier_column %in% names(data)) {
    cli::cli_abort(c(
      "{.arg tier_column} {.val {tier_column}} is not a column in the table.",
      "i" = "Available column{?s}: {.val {names(data)}}."
    ))
  }

  missing <- setdiff(levels, as.character(data[[tier_column]]))
  if (length(missing)) {
    cli::cli_warn(c(
      "{cli::qty(length(missing))}Tier{?s} {.val {missing}} {?is/are} not in \\
       {.arg tier_column}, so {?it gets/they get} no rows.",
      "i" = "{.arg tier_column} holds {.val {unique(as.character(data[[tier_column]]))}}."
    ))
  }

  fill_colors <- rlang::set_names(colors, levels)

  # image columns: every column but the tier one, unless a set is named
  image_quo <- rlang::enquo(image_columns)
  img_cols <- if (rlang::quo_is_null(image_quo)) {
    setdiff(names(data), tier_column)
  } else {
    setdiff(names(dplyr::select(data, !!image_quo)), tier_column)
  }

  # theme, images and cleared labels are applied once, not once per level
  gt_object <- gt_object %>%
    gt_theme_tier(style = style) %>%
    fmt_image(columns = tidyselect::all_of(img_cols), height = img_height) %>%
    sub_missing(missing_text = "") %>%
    cols_label(everything() ~ "")

  # each level fills its own tier cells and takes its own contrast text color
  out <- Reduce(function(gt_object, level) {
    rows_match <- which(data[[tier_column]] == level)
    if (!length(rows_match)) return(gt_object)
    bg_color <- fill_colors[[level]]
    text_color <- .theme_on_color(bg_color)

    gt_object %>%
      tab_style(
        style = list(
          cell_fill(color = bg_color),
          cell_text(weight = "bold", color = text_color)
        ),
        locations = cells_body(rows = rows_match, columns = tidyselect::all_of(tier_column))
      )
  }, levels, init = gt_object)

  .record_key(out, stats::setNames(colors, levels))
}
