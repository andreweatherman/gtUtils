#' Band alternate row groups of a `gt` table
#'
#' Shades every other row group, so each group reads as a block. `gt::opt_row_striping()`
#' bands every other row, which fights the grouping on a table that is already
#' divided into sections.
#'
#' @param gt_object A `gt` table object to modify. It must have row groups.
#' @param color Character. A hex color for the banded groups. Defaults to
#'   `"#F5F5F5"`.
#' @param start Integer. Which group to begin banding on, in the order the groups
#'   are rendered. `2` leaves the first group unshaded, `1` shades it. Defaults to
#'   `2`.
#' @param include_stub Logical. Should the stub column be banded along with the
#'   body? Defaults to `TRUE`.
#'
#' @details
#' Groups are banded in the order they render, so this follows
#' `gt::row_group_order()` rather than the order the groups happen to appear in
#' the data.
#'
#' The fill is applied to body cells rather than through CSS, because a group
#' heading occupies a row of its own and shifts every `nth-child` count below it.
#' Group heading rows are left alone; `gt::tab_options(row_group.background.color)`
#' sets those, and it sets all of them at once.
#'
#' @returns Returns a modified `gt` table with alternate groups banded.
#'
#' @examples
#' \dontrun{
#' library(gt)
#'
#' cars <- mtcars[c("mpg", "hp", "wt")]
#' cars$cyl <- paste(mtcars$cyl, "cylinders")
#'
#' gt(head(cars, 15), groupname_col = "cyl") %>%
#'   gt_theme_broadsheet() %>%
#'   gt_group_stripes()
#'
#' # shade from the first group instead, in a warmer tone
#' gt(head(cars, 15), groupname_col = "cyl") %>%
#'   gt_group_stripes(color = "#FBF3E4", start = 1)
#' }
#'
#' @import gt
#' @importFrom magrittr %>%
#' @export
gt_group_stripes <- function(gt_object, color = "#F5F5F5", start = 2,
                             include_stub = TRUE) {

  .check_gt(gt_object)

  groups <- gt_object[["_row_groups"]]
  stub <- gt_object[["_stub_df"]]

  if (!length(groups) || is.null(stub) || !nrow(stub)) {
    cli::cli_warn(c(
      "{.fn gt_group_stripes} needs a table with row groups.",
      "i" = "Set one with {.code gt(groupname_col = )} or {.fn gt::tab_row_group}."
    ))
    return(gt_object)
  }

  start <- as.integer(start)
  if (!start %in% c(1L, 2L)) {
    cli::cli_abort("{.arg start} must be {.val {1}} or {.val {2}}.")
  }

  # every other group, counted in the order they render
  banded <- groups[seq(start, length(groups), by = 2)]
  if (!length(banded)) return(gt_object)

  rows <- stub[["rownum_i"]][stub[["group_id"]] %in% banded]
  rows <- rows[!is.na(rows)]
  if (!length(rows)) return(gt_object)

  gt_object <- gt_object %>%
    gt::tab_style(
      style = gt::cell_fill(color = color),
      locations = gt::cells_body(rows = rows)
    )

  has_stub <- any(gt_object[["_boxhead"]][["type"]] == "stub")
  if (isTRUE(include_stub) && has_stub) {
    gt_object <- gt_object %>%
      gt::tab_style(
        style = gt::cell_fill(color = color),
        locations = gt::cells_stub(rows = rows)
      )
  }

  gt_object
}
