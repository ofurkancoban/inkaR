# Interactively Select an Indicator

Opens a GUI selection list (e.g., in RStudio) to browse and pick an
indicator. For code-based workflows, use `inkaR("name")` or
[`search_indicators()`](https://ofurkancoban.github.io/inkaR/reference/search_indicators.md)
instead.

## Usage

``` r
select_indicator(pattern = NULL, lang = c("de", "en"))
```

## Arguments

- pattern:

  Optional character. Pre-filter the list by a keyword or regex. If
  `NULL` (default), the full indicator list is shown.

- lang:

  Language for names: `"de"` (default) or `"en"`.

## Value

Character. The selected indicator ID, or NULL if cancelled.
