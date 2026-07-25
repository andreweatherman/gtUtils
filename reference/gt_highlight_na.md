# Style and relabel missing values in a `gt` table

Highlights missing cells and can relabel them in the same call.
[`gt::sub_missing()`](https://gt.rstudio.com/reference/sub_missing.html)
substitutes text only, and only for real `NA` values.

## Usage

``` r
gt_highlight_na(
  gt_object,
  columns = gt::everything(),
  fill = "#F0F0F0",
  text_color = NULL,
  bold = FALSE,
  italic = FALSE,
  missing_text = NULL,
  na_strings = "NA",
  ignore_case = FALSE,
  ...
)
```

## Arguments

- gt_object:

  A `gt` table object to modify.

- columns:

  The column or columns to check. Defaults to all of them.

- fill:

  Character. A hex color for the cell fill behind missing values.
  Defaults to `"#F0F0F0"`.

- text_color:

  Optional. A hex color for the text of missing values. Defaults to
  `NULL`.

- bold:

  Logical. Should missing values be bolded? Defaults to `FALSE`.

- italic:

  Logical. Should missing values be italicized? Defaults to `FALSE`.

- missing_text:

  Optional. Replacement text for missing values, such as `"--"` or
  `"Not reported"`. Defaults to `NULL`, which leaves the text alone.

- na_strings:

  A character vector of strings to treat as missing alongside real `NA`.
  Defaults to `"NA"`.

- ignore_case:

  Logical. Should `na_strings` be matched case-insensitively? Defaults
  to `FALSE`.

- ...:

  Additional arguments passed to
  [`gt::cell_text`](https://gt.rstudio.com/reference/cell_text.html).

## Value

Returns a modified `gt` table with missing values styled.

## Details

Alongside real `NA`, this also catches values that are literally the
string `"NA"`, a common artifact of reading a CSV and a frequent reason
[`sub_missing()`](https://gt.rstudio.com/reference/sub_missing.html)
appears to do nothing. Widen `na_strings` to catch other placeholders
such as `"-"` or `"N/A"`.

## See also

[`gt_outliers()`](https://andreweatherman.github.io/gtUtils/reference/gt_outliers.md)
for flagging values that are present but suspect.

## Examples

``` r
if (FALSE) { # \dontrun{
library(gt)

gt(head(airquality, 10)) %>% gt_highlight_na(c(Ozone, Solar.R))

# relabel as well as highlight
gt(head(airquality, 10)) %>%
  gt_highlight_na(c(Ozone, Solar.R), missing_text = "not recorded",
                  italic = TRUE, fill = "#FFF8E1")

# also catch placeholder strings left behind by a CSV import
gt(head(airquality, 10)) %>%
  gt_highlight_na(everything(), na_strings = c("NA", "N/A", "-"))
} # }
```
