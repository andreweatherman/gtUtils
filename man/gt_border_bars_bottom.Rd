% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gt_border_bars.R
\name{gt_border_bars_bottom}
\alias{gt_border_bars_bottom}
\title{Add horizontal bars with optional image, text, and Google Font to the bottom of a \code{gt} table}
\usage{
gt_border_bars_bottom(
  gt_object,
  colors,
  bar_height = 10,
  bar_width = "100\%",
  bar_align = "center",
  img = NULL,
  img_width = 30,
  img_height = 30,
  img_padding = 10,
  img_align = "right",
  text = NULL,
  text_weight = "bold",
  text_color = "#FFFFFF",
  text_size = 18,
  text_align = "left",
  text_padding = 10
)
}
\arguments{
\item{gt_object}{A \code{gt} table object.}

\item{colors}{A character vector of hex color codes for the bars.}

\item{bar_height}{The height of the bars in pixels. Default is 10px.}

\item{bar_width}{The width of the bars in percentage. Default is 100\%.}

\item{bar_align}{Alignment of the bars when \code{bar_width} is not 100\%. Options are 'left', 'center', or 'right'. Default is 'center'.}

\item{img}{A URL for an image to render in the bar (optional).}

\item{img_width}{The width of the image in pixels. Default is 30px.}

\item{img_height}{The height of the image in pixels. Default is 30px.}

\item{img_padding}{Padding around the image to ensure it's not touching the edges. Default is 10px.}

\item{img_align}{Alignment of the image within the bar ('left', 'center', 'right'). Default is 'right'.}

\item{text}{Optional text to display in the bar.}

\item{text_weight}{Font weight of the text. Default is 'bold' for emphasis.}

\item{text_color}{Color of the text. Default is '#FFFFFF' (white).}

\item{text_size}{Font size for the text. Default is 18px.}

\item{text_align}{Alignment of the text within the bar ('left', 'center', 'right'). Default is 'left'.}

\item{text_padding}{Padding around the text to ensure it's not touching the edges. Default is 10px.}
}
\value{
A \code{gt} table with a row of horizontal bars at the bottom.
}
\description{
This function adds a row of horizontal bars at the bottom of a \code{gt} table,
using a character vector of hex color codes. The bars are added as a source note with
customizable height, width, alignment, image size, text formatting, and image/text alignment.
If image or text is provided, only one color will be used, and that bar will
contain the image and/or text. The function automatically inherits the font from the \code{gt}
object and uses the corresponding Google Font for any text rendered in the bar.
}
