# Download INKAR Data (English Shortcut)

A convenience wrapper around
[`get_inkar_data()`](https://ofurkancoban.github.io/inkaR/reference/get_inkar_data.md)
with English output and `year = "latest"` as defaults. Equivalent to
calling `get_inkar_data(variable, level, year = "latest", lang = "en")`.

## Usage

``` r
inkar(variable, level = "KRE", year = "latest", lang = "en", ...)
```

## Arguments

- variable:

  Character. Indicator ID, short name, or partial name.

- level:

  Character. Spatial level code (default `"KRE"`).

- year:

  Integer/Character vector or `"latest"` (default). Year(s) to download.

- lang:

  Character. Output language (default `"en"`).

- ...:

  Additional arguments passed to
  [`get_inkar_data()`](https://ofurkancoban.github.io/inkaR/reference/get_inkar_data.md).

## Value

A tibble with English column names.

## Examples

``` r
# \donttest{
  try(df <- inkar("011", level = "KRE"))
#> Using M_ID '11' directly.
#> ✔ Downloaded: Gross domestic product in 1000 euros [thousand euros]
#>   Level: Kreise | Regions: 400 | Year(s): 2022 | Rows: 400
  try(df <- inkar("011", level = "KRE", year = 2019:2021))
#> Using M_ID '11' directly.
#> ✔ Downloaded: Gross domestic product in 1000 euros [thousand euros]
#>   Level: Kreise | Regions: 400 | Year(s): 2019–2021 | Rows: 1200
# }
```
