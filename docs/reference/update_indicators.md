# Refresh Local Indicators Metadata

Checks the INKAR API for new indicators not present in the local
`indicators` dataset. The check is informational only; reinstall the
package to permanently add new indicators to local metadata.

## Usage

``` r
update_indicators(lang = c("de", "en"))
```

## Arguments

- lang:

  Character. Language for messages: `"de"` or `"en"`.

## Value

Invisibly returns the local `indicators` tibble.
