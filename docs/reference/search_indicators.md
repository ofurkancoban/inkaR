# Search Indicators and Print Results

Search for indicators by keyword. Prints a formatted table and invisibly
returns the matches so you can copy the ID for use in
[`inkaR()`](https://ofurkancoban.github.io/inkaR/reference/inkaR.md).

## Usage

``` r
search_indicators(pattern, lang = c("de", "en"), theme = NULL)
```

## Arguments

- pattern:

  Text to search in names and descriptions.

- lang:

  Language to search in ("de" or "en").

- theme:

  Optional character. Filter to a specific theme/domain before
  searching. Use
  [`get_themes()`](https://ofurkancoban.github.io/inkaR/reference/get_themes.md)
  to list available themes.

## Value

A filtered tibble of indicators (invisibly).
