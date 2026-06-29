# Download Data from INKAR (Interactive Alias)

A full-featured alias for
[`get_inkar_data()`](https://ofurkancoban.github.io/inkaR/reference/get_inkar_data.md)
with bilingual support and an interactive wizard when called without
arguments (in interactive sessions). Call `inkaR("011")` to download
directly, or `inkaR()` to open the wizard.

## Usage

``` r
inkaR(variable = NULL, level = NULL, year = NULL, lang = c("de", "en"), ...)
```

## Arguments

- variable:

  Character. Indicator ID, shortname, or partial name. If `NULL`
  (default), opens an interactive selection menu (interactive sessions
  only).

- level:

  Character. Spatial level code (e.g., `"KRE"` for Kreise). If `NULL`
  and `variable` is also `NULL`, an interactive level menu is shown.

- year:

  Integer/Character vector or `"latest"`. Specific year (e.g. 2021) or
  range.

- lang:

  Character. `"de"` (default) for German column names, `"en"` for
  English.

- ...:

  Additional arguments passed to
  [`get_inkar_data()`](https://ofurkancoban.github.io/inkaR/reference/get_inkar_data.md),
  such as `format` or `csv`.

## Value

A tibble containing the downloaded data, or `NULL` if selection was
cancelled.

## Details

For a simpler English-first shortcut, see
[`inkar()`](https://ofurkancoban.github.io/inkaR/reference/inkar_shortcut.md).

## Examples

``` r
if (interactive()) {
  df <- inkaR()  # opens interactive menu
}

# \donttest{
  try(df <- inkaR("bip", level = "KRE", year = 2021))
#> Using M_ID '11' for Indicator 'bip'
#> ✔ Downloaded: Bruttoinlandsprodukt (BIP) absolut in Millionen Euro [1.000 Euro]
#>   Level: Kreise | Regions: 400 | Year(s): 2021 | Rows: 400
  try(df <- inkaR("Bruttoinlandsprodukt", level = "KRE"))
#> 6 indicators matched 'Bruttoinlandsprodukt'. Use a more specific name or an exact ID.
#>   bip  Bruttoinlandsprodukt in 1000 Euro
#>   q_bip_ew  Bruttoinlandsprodukt je Einwohner
#>   q_bip_et  Bruttoinlandsprodukt je Erwerbstätigen
#>   398  Bruttoinlandsprodukt in 1.000 € je Einwohner
#>   399  Bruttoinlandsprodukt in 1.000 € je Erwerbstätigen
#>   546  Bruttoinlandsprodukt in € je Einwohner
#> 
#> Tip: Use search_indicators("Bruttoinlandsprodukt") to explore.
# }
```
