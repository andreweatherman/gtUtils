#' Lay a long table out in side-by-side blocks
#'
#' Breaks a long table into two or more blocks set beside each other, each with
#' its own column labels. A sixty-row ranking becomes two columns of thirty,
#' which is the usual treatment in print and the difference between an image that
#' is unreadably tall and one that fits the frame.
#'
#' @param gt_object A `gt` table object to modify. Body-cell styling applied
#'   before this call is carried through the reshape; formatting and content
#'   transforms are not, so they still come after (see Details).
#' @param n_cols Integer. The number of blocks to lay out. Defaults to `2`.
#' @param rows_per_col Integer. Rows in each block. Given this, `n_cols` is
#'   worked out from the data instead. Defaults to `NULL`.
#' @param gap Numeric. The space between blocks in pixels, added as an empty
#'   spacer column so it does not disturb the alignment of the columns either
#'   side of it. Its borders, fill, and rules are scrubbed so it stays clean
#'   whitespace under any theme (see Details). `0` for no gap. Defaults to `20`.
#' @param fill Character. What to show in the trailing cells when the rows do not
#'   divide evenly. Defaults to `""`, an empty cell.
#' @param clean_gaps Logical. Should the spacer columns be scrubbed of borders,
#'   fill, and rules so the gap reads as clean whitespace (see Details)? Set
#'   `FALSE` when you style the gap yourself, e.g. with a coloured
#'   `gtExtras::gt_add_divider()`. Defaults to `TRUE`.
#'
#' @section Column names after snaking:
#'
#' Snaking is a reshape. The same columns appear once per block, so they cannot
#' all keep the same name: each is suffixed with its block number. A four-column
#' table split into two blocks becomes eight columns.
#'
#' ```
#' before                     after gt_snake(n_cols = 2)
#' ----------------------     ---------------------------------------
#' mpg cyl hp model           mpg_1 cyl_1 hp_1 model_1    <- rows 1-10
#'                            mpg_2 cyl_2 hp_2 model_2    <- rows 11-20
#' ```
#'
#' The *labels* are untouched, so the table still reads `mpg cyl hp model`
#' twice over. Only the names you address in code change.
#'
#' That gives three ways to target columns afterwards:
#'
#' ```
#' # every block of one column, via a tidyselect helper
#' fmt_number(starts_with("mpg"), decimals = 1)
#'
#' # one block only, by its full name
#' tab_style(cell_text(weight = "bold"), cells_body(columns = mpg_2))
#'
#' # the whole table, exactly as usual
#' cols_align(columns = everything(), align = "center")
#' ```
#'
#' Reach for `starts_with()` rather than listing `mpg_1, mpg_2` by hand: it keeps
#' working if you change `n_cols` later. Watch out for prefixes that overlap,
#' though. With columns named `pts` and `pts_pg`, `starts_with("pts")` catches
#' both blocks of both; `matches("^pts_[0-9]+$")` catches only `pts`.
#'
#' The `gap` between blocks is itself an empty column, named `.gap1`, `.gap2`,
#' and so on. It holds nothing and carries no label. To target the spacers in a
#' later call, use `starts_with(".gap")` or `matches("^\\.gap")`; `everything()`
#' selects them too. Set `gap = 0` if you would rather it did not exist.
#'
#' Because the gap is a real column, a theme or `gt_border_grid()` would
#' otherwise draw its column borders and row rules straight through it, leaving
#' the "whitespace" boxed in. When `clean_gaps = TRUE` (the default), the body of
#' each spacer and the inner edges of the columns either side are cleared with
#' scoped `!important` CSS, so it wins even against borders applied after the
#' snake; the spacer's own column label is cleared through `gt` directly, which
#' holds up even when `tab_spanner()` merges header cells. The table's `id` is
#' set (or carried over from the input) so the CSS can be scoped to it. Set
#' `clean_gaps = FALSE` to style the gap yourself instead.
#'
#' @details
#' The table is rebuilt from its data. Body-cell **styles** set beforehand, with
#' `gt_highlight_cells()` or `tab_style()` on `cells_body()`, are carried through:
#' each is moved to its block's suffixed column and its row within that block, so
#' a fill on the long table lands on the same cell once snaked. This means you can
#' color the natural, un-snaked grid and snake afterwards.
#'
#' **Formatting and content transforms are not styles** and are not remapped, so
#' `fmt_*()`, `text_transform()`, and `gtExtras::gt_img_rows()` still have to be
#' applied after the snake, against the suffixed names above. Themes go afterwards
#' too. Any heading and source notes already set are carried over.
#'
#' To reshape a parallel frame (a precomputed mask, per-cell colors) to match the
#' same blocks, see [gt_snake_align()].
#'
#' `gt::gt_split()` also divides a table, but it returns a group of separate
#' tables to be rendered one after another, and it errors on any table carrying
#' body-cell styles, which every theme in this package sets.
#'
#' @returns Returns a `gt` table laid out in blocks.
#'
#' @examples
#' \dontrun{
#' library(gt)
#'
#' cars <- head(mtcars[c("mpg", "cyl", "hp")], 20)
#' cars$model <- rownames(cars)
#'
#' # two blocks of ten
#' gt(cars) %>%
#'   gt_snake(n_cols = 2) %>%
#'   gt_theme_broadsheet()
#'
#' # or set the block length and let the count follow
#' gt(cars) %>%
#'   gt_snake(rows_per_col = 7, gap = 30) %>%
#'   gt_theme_swiss()
#'
#' # formatting after the reshape: note the suffixed names
#' gt(cars) %>%
#'   gt_snake(n_cols = 2) %>%
#'   gt_theme_swiss() %>%
#'   fmt_number(starts_with("mpg"), decimals = 1) %>%
#'   cols_align(columns = everything(), align = "center")
#'
#' # and picking out a single block
#' gt(cars) %>%
#'   gt_snake(n_cols = 2) %>%
#'   gt_theme_swiss() %>%
#'   tab_style(gt::cell_text(weight = "bold"),
#'             gt::cells_body(columns = model_1))
#' }
#'
#' @seealso [gt_stack_tables()] for stacking separate tables vertically.
#' @import gt
#' @importFrom magrittr %>%
#' @export
gt_snake <- function(gt_object, n_cols = 2, rows_per_col = NULL, gap = 20,
                     fill = "", clean_gaps = TRUE) {

  .check_gt(gt_object)

  data <- gt_object[["_data"]]
  boxhead <- gt_object[["_boxhead"]]
  n <- nrow(data)

  if (!is.null(rows_per_col)) {
    per <- as.integer(rows_per_col)
    if (per < 1) cli::cli_abort("{.arg rows_per_col} must be at least 1.")
    n_cols <- ceiling(n / per)
  } else {
    n_cols <- as.integer(n_cols)
    if (n_cols < 1) cli::cli_abort("{.arg n_cols} must be at least 1.")
    per <- ceiling(n / n_cols)
  }

  if (n_cols < 2 || n == 0) return(gt_object)

  styles <- gt_object[["_styles"]]

  # rendered columns, in render order
  vars <- boxhead[["var"]][boxhead[["type"]] %in% c("default", "stub")]
  vars <- vars[order(match(vars, boxhead[["var"]]))]
  labels <- vapply(vars, function(v) {
    as.character(boxhead[["column_label"]][[which(boxhead[["var"]] == v)]][[1]])
  }, character(1))

  # equal-length blocks, the last one padded out with empty rows
  blocks <- lapply(seq_len(n_cols), function(i) {
    idx <- seq.int((i - 1) * per + 1, i * per)
    idx[idx > n] <- NA_integer_
    b <- data[idx, vars, drop = FALSE]
    names(b) <- paste0(vars, "_", i)
    b
  })

  # a real empty column between blocks. padding the seam moves the body cells but
  # not the label, so the two stop lining up
  spacers <- character(0)
  if (gap > 0 && n_cols > 1) {
    parts <- list()
    for (i in seq_len(n_cols)) {
      parts[[length(parts) + 1L]] <- blocks[[i]]
      if (i < n_cols) {
        nm <- paste0(".gap", i)
        sp <- data.frame(rep("", per), stringsAsFactors = FALSE)
        names(sp) <- nm
        parts[[length(parts) + 1L]] <- sp
        spacers <- c(spacers, nm)
      }
    }
    out <- do.call(cbind, parts)
  } else {
    out <- do.call(cbind, blocks)
  }
  rownames(out) <- NULL

  real_names <- unlist(lapply(seq_len(n_cols), function(i) paste0(vars, "_", i)))
  lab_map <- stats::setNames(as.list(rep(labels, times = n_cols)), real_names)
  for (sp in spacers) lab_map[[sp]] <- ""

  res <- gt::gt(out)
  res <- do.call(gt::cols_label, c(list(res), lab_map))

  # carry over what belongs to the table rather than to its cells
  if (!is.null(gt_object[["_heading"]])) res[["_heading"]] <- gt_object[["_heading"]]
  if (!is.null(gt_object[["_source_notes"]])) {
    res[["_source_notes"]] <- gt_object[["_source_notes"]]
  }

  # blank the padding in the final block, without touching real NAs elsewhere
  padded <- n_cols * per - n
  if (padded > 0 && !is.null(fill)) {
    last_cols <- paste0(vars, "_", n_cols)
    res <- gt::sub_missing(res, columns = tidyselect::all_of(last_cols),
                           missing_text = fill)
  }

  if (length(spacers)) {
    fs <- lapply(spacers, function(nm) {
      rlang::new_formula(rlang::sym(nm), rlang::expr(gt::px(!!gap)))
    })
    res <- gt::cols_width(res, .list = fs)
  }

  # carry body-cell styling through the reshape: each style moves to its block's
  # suffixed column and row. formats and text transforms are not styles, so they
  # still belong after gt_snake()
  if (!is.null(styles) && nrow(styles)) {
    is_body <- styles[["locname"]] == "data" & !is.na(styles[["rownum"]])
    body <- styles[is_body, , drop = FALSE]
    if (nrow(body)) {
      blk <- ((body[["rownum"]] - 1) %/% per) + 1
      body[["colname"]] <- paste0(body[["colname"]], "_", blk)
      body[["rownum"]] <- ((body[["rownum"]] - 1) %% per) + 1
    }
    rest <- styles[!is_body, , drop = FALSE]
    named <- rest[!is.na(rest[["colname"]]), , drop = FALSE]
    plain <- rest[is.na(rest[["colname"]]), , drop = FALSE]
    if (nrow(named)) {
      named <- dplyr::bind_rows(lapply(seq_len(n_cols), function(i) {
        z <- named
        z[["colname"]] <- paste0(named[["colname"]], "_", i)
        z
      }))
    }
    res[["_styles"]] <- dplyr::bind_rows(body, named, plain)
  }

  # keep the caller's table id, which the rebuild drops. scoped gap css needs one
  do_clean <- isTRUE(clean_gaps) && length(spacers) > 0
  orig_id <- subset(gt_object[["_options"]], parameter == "table_id")[["value"]][[1]]
  if (!is.na(orig_id) || do_clean) {
    id <- if (is.na(orig_id)) gt::random_id() else orig_id
    pos <- which(res[["_options"]][["parameter"]] == "table_id")[[1]]
    res[["_options"]][["value"]][[pos]] <- id
  }

  # scrub borders, background and rules off the spacer, whatever a later theme
  # paints on. body cells have no colspan so target by position; labels go through
  # gt, since tab_spanner() can merge header cells and break nth-child
  if (do_clean) {
    cols <- names(out)
    sel <- function(k) paste0("#", id, " td:nth-child(", k, ")")
    # a transparent border wins border-collapse and paints nothing. `border: 0`
    # loses, and the neighbor's rule draws through the gap
    css <- unlist(lapply(which(cols %in% spacers), function(k) c(
      paste0(sel(k), " {border: 1px solid transparent !important; background: transparent !important; box-shadow: none !important;}"),
      paste0(sel(k - 1), " {border-right: 1px solid transparent !important;}"),
      paste0(sel(k + 1), " {border-left: 1px solid transparent !important;}")
    )))
    res <- gt::opt_css(res, paste(css, collapse = "\n"), add = TRUE)
    res <- gt::tab_style(
      res,
      style = gt::cell_borders(sides = "all", color = "transparent", weight = gt::px(1)),
      locations = gt::cells_column_labels(columns = tidyselect::all_of(spacers))
    )
  }

  res
}
