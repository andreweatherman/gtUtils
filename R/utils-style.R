# Internal helpers for the *_style list convention shared by gt_grid,
# gt_title_header, gt_legend_continuous, and gt_legend_discrete. Not exported.

# accept 12 or "12px" or "0.8em" and return a css length
.css_len <- function(x, unit = "px") {
  if (is.null(x)) return(NULL)
  if (is.numeric(x)) paste0(x, unit) else as.character(x)
}

# every recognized style key, all off, so a user list only sets what it names
.style_blank <- function() {
  list(font = NULL, size = NULL, color = NULL, weight = NULL, italic = FALSE,
       spacing = NULL, transform = NULL, align = NULL, line_height = NULL,
       margin_top = NULL, margin_bottom = NULL, padding_top = NULL,
       padding_bottom = NULL)
}

# per-element defaults layered over the blank set, then the user's list over that
.style_merge <- function(default = list(), user = list()) {
  if (is.null(user)) user <- list()
  utils::modifyList(utils::modifyList(.style_blank(), default), user)
}

# a style list -> inline css. `font_fallback = NULL` emits no font-family, so text
# in a gt table inherits the theme's; a stack always sets one, for composed html
.style_css <- function(s, font_fallback = NULL) {
  p <- character(0)
  if (!is.null(s$font)) {
    stack <- if (is.null(font_fallback)) "sans-serif" else font_fallback
    p <- c(p, sprintf("font-family:'%s', %s;", s$font, stack))
  } else if (!is.null(font_fallback)) {
    p <- c(p, sprintf("font-family:%s;", font_fallback))
  }
  if (!is.null(s$size)) p <- c(p, sprintf("font-size:%s;", .css_len(s$size)))
  if (!is.null(s$color)) p <- c(p, sprintf("color:%s;", s$color))
  if (!is.null(s$weight)) p <- c(p, sprintf("font-weight:%s;", s$weight))
  if (isTRUE(s$italic)) p <- c(p, "font-style:italic;")
  if (!is.null(s$spacing)) p <- c(p, sprintf("letter-spacing:%s;", .css_len(s$spacing)))
  if (!is.null(s$transform)) p <- c(p, sprintf("text-transform:%s;", s$transform))
  if (!is.null(s$align)) p <- c(p, sprintf("text-align:%s;", s$align))
  if (!is.null(s$line_height)) p <- c(p, sprintf("line-height:%s;", s$line_height))
  if (!is.null(s$margin_top)) p <- c(p, sprintf("margin-top:%s;", .css_len(s$margin_top)))
  if (!is.null(s$margin_bottom)) p <- c(p, sprintf("margin-bottom:%s;", .css_len(s$margin_bottom)))
  if (!is.null(s$padding_top)) p <- c(p, sprintf("padding-top:%s;", .css_len(s$padding_top)))
  if (!is.null(s$padding_bottom)) p <- c(p, sprintf("padding-bottom:%s;", .css_len(s$padding_bottom)))
  paste(p, collapse = "")
}

# the unique google-font names across a set of style lists
.style_fonts <- function(...) {
  fonts <- unlist(lapply(list(...), function(s) s$font))
  unique(fonts[!vapply(fonts, is.null, logical(1))])
}
