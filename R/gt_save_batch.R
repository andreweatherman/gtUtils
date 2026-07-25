#' Save a matched set of images, one per group
#'
#' Splits data by a column, builds a table for each group with a function you
#' supply, and writes one image per group. The images are padded to a common
#' width so a posted series does not come out at ragged sizes.
#'
#' @param data A data frame to split.
#' @param group The column to split on.
#' @param fn A function building one table. It is called as `fn(df, group)`,
#'   where `df` is that group's rows and `group` is its value, and it must return
#'   a `gt_tbl`.
#' @param file Character. A file name pattern containing `{group}`, which is
#'   replaced by the group value, as `"net-{group}.png"`.
#' @param dir Character. The directory to write into. Created if it does not
#'   exist. Defaults to `"."`.
#' @param match_width Logical. Should every image be padded to the width of the
#'   widest one, so a posted series shares one width? Defaults to `TRUE`.
#' @param bg Character. The background color of the padding. Defaults to
#'   `"white"`.
#' @param whitespace Numeric. Padding left around each table, in pixels. Defaults
#'   to `50`.
#' @param zoom Numeric. The rendering zoom factor. Defaults to `2`.
#' @param quiet Logical. Suppress the per-group progress message. Defaults to
#'   `FALSE`.
#'
#' @details
#' A group whose table fails to build does not abandon the run. The error is
#' collected, the remaining groups are written, and a warning at the end names
#' every group that failed.
#'
#' Group values are used in file names with anything awkward replaced by a dash,
#' so a group called `"North / East"` writes to `north-east`.
#'
#' Without `match_width`, each image is trimmed to its own content, so a set
#' posted together can come out at different widths. With it, every image is
#' padded to the width of the widest before its border is added.
#'
#' @returns Invisibly, a character vector of the files written.
#'
#' @examples
#' \dontrun{
#' library(gt)
#'
#' build <- function(df, group) {
#'   gt(df[c("mpg", "hp", "wt")]) %>%
#'     gt_theme_broadsheet() %>%
#'     tab_header(title = paste(group, "cylinders"))
#' }
#'
#' gt_save_batch(mtcars, cyl, build, "cars-{group}.png", dir = "out")
#' }
#'
#' @seealso [gt_grid()] for the same split composed into one image instead.
#' @import gt
#' @importFrom magrittr %>%
#' @export
gt_save_batch <- function(data, group, fn, file, dir = ".", match_width = TRUE,
                          bg = "white", whitespace = 50, zoom = 2, quiet = FALSE) {

  if (!is.data.frame(data)) cli::cli_abort("{.arg data} must be a data frame.")
  if (!is.function(fn)) cli::cli_abort("{.arg fn} must be a function.")
  if (!grepl("{group}", file, fixed = TRUE)) {
    cli::cli_abort("{.arg file} must contain {.code {{group}}}, as in {.val net-{{group}}.png}.")
  }

  gcol <- names(dplyr::select(data, {{ group }}))
  if (length(gcol) != 1) cli::cli_abort("{.arg group} must select exactly one column.")

  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)

  keys <- unique(data[[gcol]])
  keys <- keys[!is.na(keys)]
  if (!length(keys)) cli::cli_abort("{.arg group} has no non-missing values.")

  slug <- function(x) {
    x <- gsub("[^A-Za-z0-9._-]+", "-", as.character(x))
    tolower(gsub("^-+|-+$", "", x))
  }

  tmp <- character(0)
  built <- character(0)
  failed <- character(0)

  for (k in keys) {
    if (!quiet) cli::cli_alert_info("Building {.val {k}}")
    out <- tryCatch({
      tbl <- fn(data[data[[gcol]] == k, , drop = FALSE], k)
      if (!inherits(tbl, "gt_tbl")) {
        cli::cli_abort("{.arg fn} returned {.obj_type_friendly {tbl}}, not a {.cls gt_tbl}.")
      }
      png <- tempfile(fileext = ".png")
      gtExtras::gtsave_extra(tbl, png, zoom = zoom)
      png
    }, error = function(e) {
      failed[[length(failed) + 1L]] <<- paste0(k, ": ", conditionMessage(e))
      NULL
    })
    if (!is.null(out)) {
      tmp <- c(tmp, out)
      built <- c(built, as.character(k))
    }
  }

  if (!length(tmp)) {
    cli::cli_abort("No group built successfully.")
  }

  imgs <- lapply(tmp, function(p) magick::image_trim(magick::image_read(p)))

  # one width across the set, so a posted series is not ragged
  target <- if (isTRUE(match_width)) {
    max(vapply(imgs, function(i) magick::image_info(i)$width, numeric(1)))
  } else {
    NULL
  }

  paths <- character(0)
  for (i in seq_along(imgs)) {
    img <- imgs[[i]]
    if (!is.null(target)) {
      h <- magick::image_info(img)$height
      img <- magick::image_extent(img, magick::geometry_size_pixels(target, h),
                                  gravity = "center", color = bg)
    }
    dest <- file.path(dir, gsub("{group}", slug(built[[i]]), file, fixed = TRUE))
    magick::image_write(
      magick::image_border(img, bg, glue::glue("{whitespace}x{whitespace}")),
      dest
    )
    paths <- c(paths, dest)
  }

  unlink(tmp)

  if (length(failed)) {
    cli::cli_warn(c("{length(failed)} group{?s} failed and {?was/were} skipped:",
                    stats::setNames(failed, rep("x", length(failed)))))
  }
  if (!quiet) cli::cli_alert_success("Wrote {length(paths)} file{?s} to {.path {dir}}")

  invisible(paths)
}
