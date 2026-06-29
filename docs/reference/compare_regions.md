# Filter Downloaded Data to Specific Regions

Filters a data frame returned by
[`get_inkar_data()`](https://ofurkancoban.github.io/inkaR/reference/get_inkar_data.md)
to rows matching the supplied region names (partial, case-insensitive
match by default).

## Usage

``` r
compare_regions(data, regions, exact = FALSE)

compare_region(data, regions, exact = FALSE)
```

## Arguments

- data:

  A data frame returned by
  [`get_inkar_data()`](https://ofurkancoban.github.io/inkaR/reference/get_inkar_data.md).

- regions:

  Character vector. Region names or partial patterns to keep.

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
  if (is.data.frame(df)) compare_regions(df, c("Berlin", "Hamburg"))
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
