# Filter Downloaded Data to Specific Districts

A specialized wrapper around
[`compare_regions()`](https://ofurkancoban.github.io/inkaR/reference/compare_regions.md)
to filter a data frame returned by
[`get_inkar_data()`](https://ofurkancoban.github.io/inkaR/reference/get_inkar_data.md)
to rows matching specific district names or IDs (Kennziffer).

## Usage

``` r
compare_districts(data, districts, exact = FALSE)

compare_district(data, districts, exact = FALSE)
```

## Arguments

- data:

  A data frame returned by
  [`get_inkar_data()`](https://ofurkancoban.github.io/inkaR/reference/get_inkar_data.md).

- districts:

  Character/Numeric vector. District names or IDs (Kennziffer) to keep.
  Names match literally as parts of names. IDs match as a prefix (`"09"`
  keeps all of Bavaria) or exactly with leading zeros ignored (`1001`
  matches `"01001"`).

- exact:

  Logical. If `TRUE`, require exact string match. Default `FALSE`.

## Value

A filtered tibble.

## Examples

``` r
# \donttest{
  df <- try(get_inkar_data("011", level = "KRE", year = 2021, lang = "en"))
#> Using M_ID '11' directly.
#> ✔ Downloaded: Gross domestic product in 1000 euros [thousand euros]
#>   Level: Kreise | Regions: 400 | Year(s): 2021 | Rows: 400
  if (is.data.frame(df) && nrow(df) > 0) compare_districts(df, c("Berlin", "Hamburg"))
#>     region_id                   region_name level_name M_ID
#> 16      02000 Hamburg, Freie und Hansestadt     Kreise   11
#> 325     11000                 Berlin, Stadt     Kreise   11
#>                           indicator_name
#> 16  Gross domestic product in 1000 euros
#> 325 Gross domestic product in 1000 euros
#>                                                              description
#> 16  Gross Domestic Product (GDP) in absolute terms, in millions of euros
#> 325 Gross Domestic Product (GDP) in absolute terms, in millions of euros
#>               unit year     value
#> 16  thousand euros 2021 134119504
#> 325 thousand euros 2021 165928576
# }
```
