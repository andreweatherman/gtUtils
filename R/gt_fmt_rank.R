#' Format numbers as ordinals in a `gt` table
#'
#' Turns plain integers into ordinals: 1 becomes 1st, 2 becomes 2nd, 23 becomes
#' 23rd. The suffix can be rendered as superscript, which is the usual treatment
#' in a ranked column.
#'
#' Teens are handled correctly, so 11, 12 and 13 take "th" and not "st", "nd" and
#' "rd". Values that are not numeric are left alone.
#'
#' @param gt_object A `gt` table object to modify.
#' @param columns The column or columns to format.
#' @param superscript Logical. Should the suffix be rendered as superscript?
#'   Defaults to `TRUE`.
#' @param suffix_size Character. The size of the suffix, as a CSS size. Defaults
#'   to `"0.7em"`.
#'
#' @returns Returns a modified `gt` table with ordinal formatting applied.
#'
#' @examples
#' \dontrun{
#' library(gt)
#'
#' ranked <- data.frame(
#'   model = rownames(head(mtcars, 5)),
#'   place = 1:5
#' )
#'
#' gt(ranked) %>% gt_fmt_rank(place)
#'
#' # flat, without the superscript
#' gt(ranked) %>% gt_fmt_rank(place, superscript = FALSE)
#' }
#'
#' @import gt
#' @importFrom magrittr %>%
#' @export
gt_fmt_rank <- function(gt_object, columns, superscript = TRUE, suffix_size = "0.7em") {

  .check_gt(gt_object)

  ordinal_suffix <- function(n) {
    n <- abs(n)
    s <- rep("th", length(n))
    s[n %% 10 == 1 & n %% 100 != 11] <- "st"
    s[n %% 10 == 2 & n %% 100 != 12] <- "nd"
    s[n %% 10 == 3 & n %% 100 != 13] <- "rd"
    s
  }

  gt_object %>%
    gt::text_transform(
      locations = gt::cells_body(columns = {{ columns }}),
      fn = function(x) {
        num <- suppressWarnings(as.numeric(x))
        suffix <- ordinal_suffix(num)
        n_disp <- format(num, trim = TRUE, scientific = FALSE)
        body <- if (superscript) {
          paste0(n_disp, "<sup style='font-size:", suffix_size, ";'>", suffix, "</sup>")
        } else {
          paste0(n_disp, suffix)
        }
        ifelse(is.na(num), x, body)
      }
    )
}
