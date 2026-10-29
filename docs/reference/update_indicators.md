# Refresh Local Indicators Metadata

Checks the INKAR API for indicators that are not in the local
`indicators` dataset. The check is informational only; install a newer
package version to add them to the local metadata. New indicators can
still be downloaded by their numeric M_ID with
[`get_inkar_data()`](https://ofurkancoban.github.io/inkaR/reference/get_inkar_data.md).

## Usage

``` r
update_indicators(lang = c("de", "en"))
```

## Arguments

- lang:

  Character. Language for messages: `"de"` or `"en"`. Only indicators
  for German spatial levels are compared; the API's European indicators
  (NUTS-2) are not supported by this package.

## Value

Invisibly returns the local `indicators` tibble. Indicators the API
offers but the local metadata lacks are attached as attribute
`"new_indicators"` (`M_ID`, `Name`, `Bereich`); active local indicators
the API no longer offers as `"removed_indicators"`.
