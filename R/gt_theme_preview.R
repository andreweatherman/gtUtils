#' Preview data in every theme
#'
#' Renders the same few rows through each `gt_theme_*` function in the package
#' and lays the results out in a grid, one panel per theme, labeled with its name.
#'
#' @param data A data frame. A `gt` table is also accepted, in which case its
#'   underlying data is used.
#' @param themes Character. The theme functions to show, by name. If `NULL`,
#'   every `gt_theme_*` in the package is used. Defaults to `NULL`.
#' @param n Integer. How many rows of `data` to show in each panel. Defaults to
#'   `5`.
#' @param ncol Integer. The number of panels across. Defaults to `3`.
#' @param density Character. A `density` passed to every theme, so the panels are
#'   comparable. If `NULL`, each theme uses its own default. Defaults to
#'   `"compact"`.
#' @param file Optional. A path to write a PNG to. If `NULL`, the grid is
#'   returned for the viewer. Defaults to `NULL`.
#' @param ... Further arguments passed to [gt_grid()], such as `gap` or `bg`.
#'
#' @details
#' Each panel is captioned with the theme's name in a neutral style, set outside
#' the table. Using each theme's own heading instead would let a wide display
#' title stretch its panel out of shape.
#'
#' Themes that take extra arguments, such as `style` on [gt_theme_sofa()], are
#' shown at their defaults.
#'
#' @returns Displays the grid in the viewer, or writes it to `file`.
#'
#' @examples
#' \dontrun{
#' gt_theme_preview(mtcars[c("mpg", "cyl", "hp")])
#'
#' # a subset, sized for a wide screenshot
#' gt_theme_preview(
#'   iris,
#'   themes = c("gt_theme_broadsheet", "gt_theme_swiss", "gt_theme_midnight"),
#'   ncol = 3, file = "themes.png"
#' )
#' }
#'
#' @seealso [gt_grid()], which does the layout.
#' @import gt
#' @importFrom magrittr %>%
#' @export
gt_theme_preview <- function(data, themes = NULL, n = 5, ncol = 3,
                             density = "compact", file = NULL, ...) {

  if (inherits(data, "gt_tbl")) data <- data[["_data"]]
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame or a {.cls gt_tbl}.")
  }
  if (!nrow(data)) cli::cli_abort("{.arg data} has no rows.")

  if (is.null(themes)) {
    cand <- sort(grep("^gt_theme_", getNamespaceExports("gtUtils"), value = TRUE))
    # this function matches its own pattern, and calling it as a theme recurses
    cand <- setdiff(cand, "gt_theme_preview")
    # keep only things shaped like a theme: first argument is the table
    themes <- Filter(function(nm) {
      f <- formals(get(nm, envir = asNamespace("gtUtils")))
      length(f) && names(f)[[1]] == "gt_object"
    }, cand)
  }
  missing <- themes[!vapply(themes, exists, logical(1), where = asNamespace("gtUtils"))]
  if (length(missing)) {
    cli::cli_abort("No such theme{?s}: {.val {missing}}.")
  }

  d <- utils::head(data, n)

  panels <- lapply(themes, function(nm) {
    fn <- get(nm, envir = asNamespace("gtUtils"))
    # only pass density to the themes that take it
    args <- list(gt::gt(d))
    if (!is.null(density) && "density" %in% names(formals(fn))) {
      args$density <- density
    }
    do.call(fn, args)
  })

  # label outside the table, so a wide display title does not stretch its panel
  gt_grid(panels, ncol = ncol, labels = sub("^gt_theme_", "", themes),
          file = file, ...)
}
