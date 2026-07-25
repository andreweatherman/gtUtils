#' Draw a labeled cut line between rows of a `gt` table
#'
#' Draws a rule across the table after a given row, with an optional label on it.
#' This is the cut line convention used in ranked and sorted tables to mark a
#' cutoff or qualifying threshold.
#'
#' The line is positional, not data-driven. `gt::tab_row_group()` splits a table
#' on a value in the data and turns the result into labeled sections with heading
#' rows. A cut line leaves the table flowing and marks a fixed row number.
#'
#' @param gt_object A `gt` table object to modify.
#' @param after Numeric. The row number or numbers to draw a line after. `after = 4`
#'   draws between rows 4 and 5. Vectorized, so `c(4, 25)` draws two lines.
#'   `after = 0` draws a line above the first row, which is how you label the top
#'   section of a table sorted into a high group and a low group.
#' @param label Optional. The label or labels for the lines, recycled against
#'   `after`. Use `NA` for an unlabeled line among labeled ones. Rendered in
#'   uppercase. Defaults to `NULL`.
#' @param color Character. A hex color for the rule. Defaults to `"#A6081A"`.
#' @param weight Numeric. The thickness of the rule in pixels. Defaults to `2`.
#' @param style Character. The line style. One of `"dashed"`, `"solid"`, or
#'   `"dotted"`. Defaults to `"dashed"`.
#' @param label_color Optional. A hex color for the label. Defaults to `NULL`,
#'   which uses `color`.
#' @param label_size Numeric. The label size in pixels. Defaults to `9`.
#' @param label_position Character. Either `"below"` to put the label under the
#'   line or `"above"` to put it over. Defaults to `"below"`.
#' @param gap Numeric. Extra space in pixels added around the line to set the two
#'   sections further apart. A single number is applied above and below the line;
#'   a length-2 vector `c(above, below)` sets the two sides separately, so
#'   `c(0, 12)` opens space only under the line. Applies to labeled and unlabeled
#'   lines alike. Defaults to `0`.
#'
#' @details
#' The label is drawn as an inline SVG background image on the row, because
#' `gt`'s CSS inliner strips the `::before` and `::after` pseudo-elements this would
#' normally be built with. That costs two things. An SVG background cannot use
#' the page's webfonts, so the label renders in a system sans-serif rather than
#' the theme's font. And a background image cannot straddle a border, so the
#' labeled row is given extra padding for the label to sit in.
#'
#' The labeled row's cell backgrounds are cleared, since row striping and themed
#' body fills would otherwise cover the label. The row's own color is then
#' reapplied to the row itself, so a striped theme such as [gt_theme_almanac()]
#' keeps an unbroken stripe pattern.
#'
#' Labeled lines assume the table has no row groups, since group heading rows
#' shift the row positions the label CSS targets. The rule itself is unaffected.
#'
#' Apply the theme before this function. The stripe color is read from the
#' table's options, so a line labeled first loses the stripe under its own row.
#'
#' @returns Returns a modified `gt` table with the cut line or lines added.
#'
#' @examples
#' \dontrun{
#' library(gt)
#'
#' efficient <- head(mtcars[order(-mtcars$mpg), c("mpg", "hp", "wt")], 12)
#' efficient$car <- rownames(efficient)
#'
#' # mark where the top six ends
#' gt(efficient) %>%
#'   gt_theme_broadsheet() %>%
#'   gt_cutline(after = 6, label = "Top six")
#'
#' # two lines, one of them unlabeled
#' gt(efficient) %>%
#'   gt_cutline(after = c(3, 6), label = c("Shortlist", NA), color = "#0054AD")
#' }
#'
#' @seealso [gt_spotlight()] for drawing attention to a row.
#' @import gt
#' @importFrom magrittr %>%
#' @export
gt_cutline <- function(gt_object, after, label = NULL,
                       color = "#A6081A", weight = 2, style = "dashed",
                       label_color = NULL, label_size = 9,
                       label_position = c("below", "above"), gap = 0) {

  .check_gt(gt_object)
  label_position <- match.arg(label_position)
  if (!length(after)) return(gt_object)
  if (!is.numeric(after)) cli::cli_abort("{.arg after} must be numeric row number{?s}.")
  if (!is.numeric(gap) || length(gap) < 1 || length(gap) > 2 || any(gap < 0)) {
    cli::cli_abort("{.arg gap} must be one or two non-negative numbers.")
  }
  gaps <- if (length(gap) == 1) rep(gap, 2) else gap[1:2]
  above_gap <- gaps[[1]]
  below_gap <- gaps[[2]]

  n_rows <- nrow(gt_object[["_data"]])
  bad <- after < 0 | after >= n_rows
  if (any(bad)) {
    cli::cli_warn(c(
      "Dropped {sum(bad)} cut line{?s} at {.val {after[bad]}}.",
      "i" = "{.arg after} must be between 0 and {n_rows - 1}; a line after the
             last row is just the table border."
    ))
    label <- if (is.null(label)) NULL else rep_len(label, length(after))[!bad]
    after <- after[!bad]
    if (!length(after)) return(gt_object)
  }
  if (!is.null(label)) label <- rep_len(label, length(after))
  if (is.null(label_color)) label_color <- color

  res <- .table_id(gt_object)
  gt_object <- res$object
  table_id <- res$id

  # the rule is a top border on the row below the cut
  for (a in after) {
    gt_object <- gt_object %>%
      gt::tab_style(
        style = gt::cell_borders(sides = "top", weight = gt::px(weight),
                                 color = color, style = style),
        locations = gt::cells_body(rows = a + 1)
      )
  }

  # room around each rule, split onto the flanking rows. the labeled side folds
  # its share into the label padding below
  gap_css <- character(0)
  for (i in seq_along(after)) {
    a <- after[[i]]
    lab <- if (is.null(label)) NA_character_ else label[[i]]
    labeled <- !is.na(lab) && nzchar(lab)
    label_row <- if (a == 0) 1L else if (label_position == "below") a + 1L else a
    # the row above the rule gets padding-bottom; the row below gets padding-top
    rows <- c(a, a + 1L)
    sides <- c("bottom", "top")
    vals <- c(above_gap, below_gap)
    for (k in seq_along(rows)) {
      r <- rows[[k]]
      if (vals[[k]] <= 0 || r < 1 || r > n_rows) next
      if (labeled && r == label_row) next   # folded into the label padding
      gap_css <- c(gap_css, sprintf(
        "#%s tbody tr:nth-child(%d) td { padding-%s: %dpx !important; }",
        table_id, r, sides[[k]], vals[[k]]))
    }
  }

  apply_css <- function(obj, rules) {
    if (length(rules)) obj %>% gt::opt_css(rules) else obj
  }

  if (is.null(label)) return(apply_css(gt_object, gap_css))

  # label goes on the tr, so clear the cell fills and repaint the stripe there
  opt <- gt_object[["_options"]]
  optval <- function(p) {
    v <- opt$value[opt$parameter == p]
    if (!length(v)) NA_character_ else as.character(v[[1]])
  }
  striping_on <- isTRUE(as.logical(optval("row_striping_include_table_body")))
  stripe_col <- optval("row_striping_background_color")

  css <- character(0)
  for (i in seq_along(after)) {
    lab <- label[[i]]
    if (is.na(lab) || !nzchar(lab)) next

    # row that carries the label
    row_css <- if (label_position == "below") after[[i]] + 1 else after[[i]]
    side <- if (label_position == "below") "top" else "bottom"
    pos <- if (label_position == "below") "left 5px" else "left bottom 5px"

    # nothing above row 1 to sit under, so the label drops into the top of it
    if (row_css < 1) {
      row_css <- 1
      side <- "top"
      pos <- "left 5px"
    }

    # the label's side folds its gap into the padding. "top" is below the line
    label_gap <- if (side == "top") below_gap else above_gap
    pad <- label_size + 13 + label_gap

    # gt stripes even body rows
    row_bg <- if (striping_on && row_css %% 2 == 0 &&
                  !is.na(stripe_col) && nzchar(stripe_col)) stripe_col else NA_character_

    css <- c(
      css,
      # make room, and clear the fills that would cover the label
      sprintf(paste0("#%s tbody tr:nth-child(%d) td { padding-%s: %dpx !important;",
                     " background-color: transparent !important; }"),
              table_id, row_css, side, pad),
      # label spans the row, not one cell
      sprintf("#%s tbody tr:nth-child(%d) { %sbackground-image: url(\"%s\"); %s }",
              table_id, row_css,
              if (is.na(row_bg)) "" else sprintf("background-color: %s; ", row_bg),
              .cutline_svg(lab, label_color, label_size),
              sprintf("background-repeat: no-repeat; background-position: %s;", pos))
    )
  }

  css <- c(gap_css, css)
  if (!length(css)) return(gt_object)
  gt_object %>% gt::opt_css(css)
}

# inline svg holding the label. url-encoded rather than base64 to avoid a dep
.cutline_svg <- function(text, color, size) {
  text <- toupper(as.character(text))
  esc <- function(x) {
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    gsub(">", "&gt;", x, fixed = TRUE)
  }
  tracking <- 1.1
  width <- nchar(text) * (size * 0.80 + tracking) + 4
  height <- size + 4

  svg <- sprintf(
    paste0('<svg xmlns="http://www.w3.org/2000/svg" width="%.0f" height="%.0f">',
           '<text x="0" y="%.1f" font-family="Helvetica,Arial,sans-serif" ',
           'font-size="%s" font-weight="700" letter-spacing="%s" fill="%s">%s</text></svg>'),
    width, height, size + 0.5, size, tracking, color, esc(text)
  )
  paste0("data:image/svg+xml;charset=utf-8,",
         utils::URLencode(svg, reserved = TRUE))
}
