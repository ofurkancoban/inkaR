# View Indicators in RStudio Viewer

Opens the available indicators in the RStudio data viewer for easy
filtering and searching.

## Usage

``` r
view_indicators(lang = c("de", "en"), theme = NULL)
```

## Arguments

- lang:

  Language code: "de" (German) or "en" (English).

- theme:

  Optional character. Filter to a specific theme/domain before opening
  the viewer. Use
  [`get_themes()`](https://ofurkancoban.github.io/inkaR/reference/get_themes.md)
  to list available themes.

## Value

Invokes [`View()`](https://rdrr.io/r/utils/View.html) on the data frame.
