# Get Administrative Boundaries Matching INKAR

Returns `sf` polygons for a spatial level with a `Kennziffer` column in
the same format as
[`get_inkar_data()`](https://ofurkancoban.github.io/inkaR/reference/get_inkar_data.md),
so the two can be joined directly. Boundaries come from Eurostat GISCO
LAU (Gemeinden), whose territorial status matches INKAR. Higher levels
are built by dissolving the Gemeinden; Gemeindeverbaende use the
Destatis Gemeindeverzeichnis to assign each Gemeinde to its Verband.

## Usage

``` r
get_inkar_geometry(
  level = c("KRE", "BND", "BLD", "GVB", "GEM"),
  refresh = FALSE
)
```

## Arguments

- level:

  Character. One of `"BND"`, `"BLD"`, `"KRE"`, `"GVB"`, `"GEM"`. `"ROR"`
  is not available.

- refresh:

  Logical. If `TRUE`, ignore the cache and download again.

## Value

An `sf` object (CRS ETRS89-LAEA, EPSG:3035) with columns `Kennziffer`
and `geometry` (plus `name` for `"GEM"`).

## Details

The GISCO file (about 43 MB) is downloaded once and the German polygons
are cached. In interactive sessions the cache persists across sessions;
see
[`clear_inkar_cache()`](https://ofurkancoban.github.io/inkaR/reference/clear_inkar_cache.md).

Source: (c) EuroGeographics for the administrative boundaries. Please
keep this attribution in published maps.

## Examples

``` r
# Downloads about 43 MB on first use, so it only runs interactively
if (interactive() && requireNamespace("sf", quietly = TRUE)) {
  kreise <- get_inkar_geometry("KRE")
}
```
