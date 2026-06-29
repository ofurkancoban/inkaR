# List Available Indicator Themes

Returns the unique theme/domain values present in the local `indicators`
dataset. Pass one of these values to the `theme` argument of
[`search_indicators()`](https://ofurkancoban.github.io/inkaR/reference/search_indicators.md)
or
[`view_indicators()`](https://ofurkancoban.github.io/inkaR/reference/view_indicators.md)
to narrow results.

## Usage

``` r
get_themes()
```

## Value

A sorted character vector of theme names.

## Examples

``` r
get_themes()
#> [1] "API Discovered"
```
