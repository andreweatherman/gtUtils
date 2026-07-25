# Deprecated function names, kept so existing code keeps working. Each warns
# once per call and forwards to its renamed replacement.

#' Deprecated legend functions
#'
#' These are the former names of the two legend helpers, kept for backward
#' compatibility. Use the new names instead.
#'
#' - `gt_color_legend()` is now [gt_legend_continuous()].
#' - `gt_centered_legend()` is now [gt_legend_discrete()].
#'
#' @param ... Passed on to the replacement function.
#' @param gt_table The `gt` table, forwarded as `gt_object`.
#' @returns Returns a modified `gt` table, from the replacement function.
#' @name gtUtils-deprecated
NULL

#' @rdname gtUtils-deprecated
#' @export
gt_color_legend <- function(...) {
  .Deprecated("gt_legend_continuous", package = "gtUtils")
  gt_legend_continuous(...)
}

#' @rdname gtUtils-deprecated
#' @export
gt_centered_legend <- function(gt_table, ...) {
  .Deprecated("gt_legend_discrete", package = "gtUtils")
  gt_legend_discrete(gt_table, ...)
}
