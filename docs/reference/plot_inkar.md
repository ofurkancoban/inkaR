# Plot INKAR Data on German Maps

Automatically projects regional INKAR data onto administrative
boundaries of Germany using the `geodata` and `sf` packages. Supports
Bund (BND), Bundeslaender (BLD), Kreise (KRE), and Gemeinden (GEM)
levels. Alternatively, a custom `sf` geometry can be provided.

## Usage

``` r
plot_inkar(
  data,
  variable = NULL,
  year = NULL,
  mode = c("light", "dark"),
  highlight = NULL,
  breaks = c("equal", "quantile"),
  title = NULL,
  geom = NULL
)
```

## Arguments

- data:

  A data frame returned by
  [`get_inkar_data()`](https://ofurkancoban.github.io/inkaR/reference/get_inkar_data.md).

- variable:

  Character. For wide-format data with multiple indicators, specify
  which indicator column to plot.

- year:

  Integer/Character. If the data contains multiple years, specify which
  year to plot. If NULL and multiple years exist, the most recent year
  is plotted.

- mode:

  Character. "light" (default) or "dark" theme.

- highlight:

  Character vector. Region names (partial match) to highlight; all other
  regions are shown at reduced opacity.

- breaks:

  Character. Color scale break method: `"equal"` (default) or
  `"quantile"` for quantile-based color breaks.

- title:

  Character. Custom plot title. Defaults to the indicator name.

- geom:

  Optional `sf` object (spatial data frame) to use for plotting. If
  supplied, GADM geometries are not downloaded, and the data is merged
  directly with this object.

## Value

A `ggplot2` object displaying the mapped data.
