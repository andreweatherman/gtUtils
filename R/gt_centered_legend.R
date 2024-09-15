#' Add a centered title, subtitle, and legend to a `gt` table with customizable
#' label placement
#'
#' This function adds a centered title, subtitle, and a key or legend at the top
#' of a `gt` table. The key/legend is passed through as a tibble containing
#' colors and labels. Labels can be placed either inside or outside the color
#' boxes.
#'
#' @param gt_table A `gt` table object to modify.
#' @param key_info A tibble containing two columns: `color` (hex color codes)
#'   and `label` (the corresponding text labels).
#' @param heading Character. The main title text for the table. Defaults to
#'   `"Centered Title"`.
#' @param subtitle Character. The subtitle text for the table. Defaults to
#'   `"Centered Subtitle"`.
#' @param label_placement Character. Either `"inside"` or `"outside"`,
#'   specifying whether the labels should be inside or outside the color boxes.
#'   Defaults to `"outside"`.
#'
#' @details The function creates a centered title, subtitle, and a legend based
#' on the provided key information (colors and labels). If `label_placement` is
#' `"inside"`, the labels will be placed inside the colored boxes; if
#' `"outside"`, they will be placed next to the colored boxes.
#'
#' @returns Returns a modified `gt` table with a centered title, subtitle, and a
#'   key.
#'
#' @importFrom gt tab_header
#' @importFrom glue glue
#' @importFrom dplyr mutate pull
#' @export
gt_centered_legend <- function(gt_table, key_info, heading = "Centered Title", subtitle = "Centered Subtitle", label_placement = "outside") {

  if (label_placement == "inside") {

    key_html <- key_info %>%
      mutate(
        key_item = glue::glue(
          "<div style='border: 1.5px solid black; padding: 2px 5px; text-align: center; background-color: {color}; font-size: 10px; margin-right: 10px;'>{label}</div>"
        )
      ) %>%
      pull(key_item) %>%
      paste(collapse = ' ')

    full_header_html <- glue::glue(
      "<div style='text-align: center;'>
        <div style='font-size: 16px; margin-bottom: 2px;'>{heading}</div>
        <div style='font-size: 13px; margin-bottom: 5px;font-weight:normal'>{subtitle}</div>
        <div style='display: flex; justify-content: center; align-items: center; margin-top: 5px; margin-bottom: 5px;'>{key_html}</div>
      </div>"
    )
  } else {

    key_html <- key_info %>%
      mutate(
        key_item = glue::glue(
          "<div style='display: flex; align-items: center; margin-right: 15px;'>
            <span style='display: inline-block; margin-right: 5px; width: 15px; height: 15px; background-color: {color};'></span>
            <span style='font-size:12px; vertical-align:20%'>{label}</span>
          </div>"
        )
      ) %>%
      pull(key_item) %>%
      paste(collapse = ' ')

    full_header_html <- glue::glue(
      "<div style='text-align: center;'>
        <div style='font-size: 16px; margin-bottom: 2px;'>{heading}</div>
        <div style='font-size: 13px; margin-bottom: 5px;font-weight:normal'>{subtitle}</div>
        <div style='margin-bottom: 5px; display: flex; justify-content: center; align-items: center;'>{key_html}</div>
      </div>"
    )
  }

  gt_table %>%
    tab_header(
      htmltools::HTML(full_header_html)
    )
}
