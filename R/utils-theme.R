# Internal helpers for the themes, plus the table id and color math anything
# emitting scoped css needs. Not exported.

# the guard every exported function opens with
.check_gt <- function(gt_object, arg = "gt_object", call = rlang::caller_env()) {
  if (inherits(gt_object, "gt_tbl")) return(invisible(gt_object))
  hint <- if (is.data.frame(gt_object)) {
    "It looks like raw data. Pipe it through {.fn gt::gt} first."
  } else {
    "Build a table with {.fn gt::gt} and pass that in."
  }
  cli::cli_abort(
    c("{.arg {arg}} must be a {.cls gt_tbl}, not {.obj_type_friendly {gt_object}}.",
      "i" = hint),
    call = call
  )
}

# find or assign a random table id for css stuff
.table_id <- function(gt_object) {
  table_id <- subset(gt_object[["_options"]], parameter == "table_id")$value[[1]]
  if (is.na(table_id)) {
    table_id <- gt::random_id()
    opt_position <- which("table_id" %in% gt_object[["_options"]][["parameter"]])[[1]]
    gt_object[["_options"]][["value"]][[opt_position]] <- table_id
  }
  list(object = gt_object, id = table_id)
}

# type size and row padding in one scale. "social" is sized for saved images
.theme_density <- function(density = c("comfortable", "compact", "social")) {
  # themes list their own default first, so take the head rather than the formals
  density <- match.arg(density[1], c("comfortable", "compact", "social"))
  switch(
    density,
    comfortable = list(body = 14, pad = 6, title = 26, subtitle = 15,
                       label = 10, group = 11, source = 11),
    compact = list(body = 12, pad = 3, title = 22, subtitle = 13,
                   label = 9, group = 10, source = 10),
    social = list(body = 17, pad = 9, title = 34, subtitle = 19,
                  label = 12, group = 13, source = 13)
  )
}

# density as multipliers against "comfortable"
.theme_density_mult <- function(density) {
  base <- .theme_density("comfortable")
  d <- .theme_density(density)
  mapply(function(a, b) a / b, d, base, SIMPLIFY = FALSE)
}

# scale a css length. relative units (%, em, rem) already track what they inherit
.theme_scale_len <- function(value, k) {
  if (is.null(value) || !length(value)) return(value)
  v <- as.character(value)[[1]]
  if (!grepl("^-?[0-9.]+px$", v)) return(value)
  n <- as.numeric(sub("px$", "", v))
  paste0(round(n * k, 1), "px")
}

# retrofit density onto a finished theme. the older themes hard-code their sizes,
# so walk the built object and scale what it holds
.theme_scale_output <- function(gt_object, density) {
  k <- .theme_density_mult(density)
  if (all(vapply(k, function(x) isTRUE(all.equal(x, 1)), logical(1)))) return(gt_object)

  # sizes set through tab_style()
  role_of <- c(
    title = "title", subtitle = "subtitle",
    columns_columns = "label", columns_groups = "label",
    row_groups = "group", stubhead = "label",
    data = "body", stub = "body",
    source_notes = "source", footnotes = "source"
  )
  styles <- gt_object[["_styles"]]
  if (!is.null(styles) && nrow(styles)) {
    for (i in seq_len(nrow(styles))) {
      role <- role_of[[styles$locname[[i]]]]
      if (is.null(role)) next
      sz <- styles$styles[[i]]$cell_text$size
      if (is.null(sz)) next
      gt_object[["_styles"]]$styles[[i]]$cell_text$size <- .theme_scale_len(sz, k[[role]])
    }
  }

  # sizes and padding set through tab_options()
  opt_role <- c(
    table_font_size = "body", stub_font_size = "body",
    heading_title_font_size = "title", heading_subtitle_font_size = "subtitle",
    column_labels_font_size = "label", row_group_font_size = "group",
    source_notes_font_size = "source", footnotes_font_size = "source",
    data_row_padding = "pad", row_group_padding = "pad",
    column_labels_padding = "pad", heading_padding = "pad",
    source_notes_padding = "pad", footnotes_padding = "pad",
    summary_row_padding = "pad", grand_summary_row_padding = "pad"
  )
  opts <- gt_object[["_options"]]
  for (p in names(opt_role)) {
    pos <- which(opts$parameter == p)
    if (!length(pos)) next
    gt_object[["_options"]][["value"]][[pos[[1]]]] <-
      .theme_scale_len(opts$value[[pos[[1]]]], k[[opt_role[[p]]]])
  }

  gt_object
}

# a hex vector, or a paletteer "package::palette" string to expand. paletteer
# keeps discrete and continuous palettes in separate registries, so try the one
# pal_type names and fall back to the other rather than making the caller know
.resolve_palette <- function(palette, pal_type = "discrete", arg = "palette",
                             call = rlang::caller_env()) {
  if (!length(palette) || !grepl("::", palette[1])) return(palette)

  from_c <- function() as.character(paletteer::paletteer_c(palette[1], n = 256))
  from_d <- function() as.character(paletteer::paletteer_d(palette[1]))
  want_c <- identical(pal_type, "continuous")

  out <- tryCatch(if (want_c) from_c() else from_d(), error = function(e) NULL)
  if (!is.null(out)) return(out)

  out <- tryCatch(if (want_c) from_d() else from_c(), error = function(e) NULL)
  if (!is.null(out)) return(out)

  cli::cli_abort(c(
    "Palette {.val {palette[1]}} was not found in {.pkg paletteer}.",
    "x" = "It is in neither the discrete nor the continuous registry.",
    "i" = "Check the spelling of {.code package::palette}, or pass a vector of \\
           hex colors to {.arg {arg}} instead."
  ), call = call)
}

# the coloring functions leave the scale they used on the table so a legend can
# read it back instead of making the caller restate it. an R attribute rather
# than a slot in the gt object, so gt never sees it. survives the whole pipeline
# except gt_snake(), which rebuilds the table from its data
.record_scale <- function(gt_object, columns, palette, domain, reverse, pal_type) {
  attr(gt_object, "gtutils_scale") <- list(
    columns = columns, palette = palette, domain = domain,
    reverse = reverse, pal_type = pal_type
  )
  gt_object
}

.recorded_scale <- function(gt_object) attr(gt_object, "gtutils_scale")

# the same, for a discrete key: label = color
.record_key <- function(gt_object, key) {
  attr(gt_object, "gtutils_key") <- key
  gt_object
}

.recorded_key <- function(gt_object) attr(gt_object, "gtutils_key")

# col2rgb() rejects 3-digit hex, so expand #abc to #aabbcc. anything else
# (named colors, 6- and 8-digit hex) passes through
.hex6 <- function(x) {
  ifelse(grepl("^#[0-9a-fA-F]{3}$", x),
         paste0("#", gsub("([0-9a-fA-F])", "\\1\\1", substring(x, 2))),
         x)
}

# wcag relative luminance and contrast ratio
.theme_luminance <- function(hex) {
  v <- grDevices::col2rgb(.hex6(hex))[, 1] / 255
  v <- ifelse(v <= 0.03928, v / 12.92, ((v + 0.055) / 1.055)^2.4)
  0.2126 * v[1] + 0.7152 * v[2] + 0.0722 * v[3]
}

.theme_contrast <- function(a, b) {
  la <- .theme_luminance(a)
  lb <- .theme_luminance(b)
  (max(la, lb) + 0.05) / (min(la, lb) + 0.05)
}

# blend fg into bg by weight w (1 = pure fg)
.theme_mix <- function(fg, bg, w) {
  a <- grDevices::col2rgb(.hex6(fg))[, 1]
  b <- grDevices::col2rgb(.hex6(bg))[, 1]
  grDevices::rgb(t(round(a * w + b * (1 - w))), maxColorValue = 255)
}

# black or white by measured contrast. vectorized over bg
.theme_on_color <- function(bg) {
  vapply(bg, function(b) {
    if (.theme_contrast("#000000", b) >= .theme_contrast("#FFFFFF", b)) "#000000" else "#FFFFFF"
  }, character(1), USE.NAMES = FALSE)
}

# muted but legible. gray washes out on a saturated ground, so blend the ground's
# own text color until it clears target
.theme_secondary_on <- function(bg, fg, target = 4.5) {
  for (w in seq(0.45, 1, by = 0.05)) {
    cand <- .theme_mix(fg, bg, w)
    if (.theme_contrast(cand, bg) >= target) return(cand)
  }
  fg
}

# gt's hline under the last row doubles up with the table's closing rule
.theme_last_row_border <- function(table_id, bg) {
  paste0("#", table_id, " tbody tr:last-child {border-bottom: 2px solid ", bg, ";}")
}

# tabular figures, so digits don't shimmy between rows
.theme_tabular_nums <- function(table_id) {
  # not font-feature-settings: 'tnum', which also spaces out commas in some faces
  paste0("#", table_id, " td { font-variant-numeric: tabular-nums; }")
}
