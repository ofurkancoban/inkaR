# Plot INKAR Data on German Maps

Automatically projects regional INKAR data onto administrative
boundaries of Germany. Supports Bund (BND), Bundeslaender (BLD), Kreise
(KRE), Gemeindeverbaende (GVB) and Gemeinden (GEM). Boundaries come from
Eurostat GISCO with the same territorial status as INKAR (see
[`get_inkar_geometry()`](https://ofurkancoban.github.io/inkaR/reference/get_inkar_geometry.md));
they are downloaded once and cached. Alternatively, a custom `sf`
geometry can be provided, for example for Raumordnungsregionen (ROR).

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
  `"quantile"` for five quantile classes, each with its own color step.

- title:

  Character. Custom plot title. Defaults to the indicator name.

- geom:

  Optional `sf` object (spatial data frame) to use for plotting. If
  supplied, no boundaries are downloaded, and the data is merged
  directly with this object.

## Value

A `ggplot2` object displaying the mapped data.
