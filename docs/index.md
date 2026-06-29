# inkaR

![](https://raw.githubusercontent.com/ofurkancoban/inkaR/main/man/figures/logo.png)

**Professional R Interface for BBSR INKAR Spatial Development Data**

The `inkaR` package provides a professional, fast, and feature-rich R
interface to download and analyze spatial development indicators from
the [BBSR INKAR](https://www.inkar.de/) (Indikatoren und Karten zur
Raum- und Stadtentwicklung) database.

Designed for researchers and data scientists, `inkaR` abstracts away the
complex JSON API of INKAR into clean, analytical data frames. Version
0.6.1 introduces a premium interactive wizard, multi-indicator support
with automatic joining, and high-end visualization themes.

## Deeply Integrated Features 💎

- 🪄 **Interactive Selection Wizard**: Run
  [`inkaR()`](https://ofurkancoban.github.io/inkaR/reference/inkaR.md)
  without arguments for a guided terminal session.
- 🔗 **Multi-Indicator Support**: Download and merge multiple variables
  at once (Vertical or Horizontal joins).
- 🔍 **Bilingual Fuzzy Search**: Intelligent, error-tolerant search for
  both German and English indicator names.
- ⭐ **Usage History & Favorites**: Highlighting frequently used
  indicators for a personalized experience.
- 🎨 **Professional Visualizations**: Dedicated ggplot2 themes
  (`theme_inkaR`) for publication-ready maps.
- 🚀 **Optimized Performance**: Intelligent persistent caching and
  parallel API discovery.

## Installation 📦

You can install the released version of **inkaR** from
[CRAN](https://CRAN.R-project.org/package=inkaR) with:

``` r
install.packages("inkaR")
```

Or get the latest development version from
[GitHub](https://github.com/ofurkancoban/inkaR):

``` r
# install.packages("devtools")
devtools::install_github("ofurkancoban/inkaR")
```

## Quick Start

### 1. Interactive Selection (Wizard Mode)

Simply call
[`inkaR()`](https://ofurkancoban.github.io/inkaR/reference/inkaR.md) in
an interactive R session. A professional selection wizard will guide you
through: - **Indicator Discovery**: Search with keywords (supports fuzzy
matching). - **Spatial Level Selection**: Automatically probes the API
for available levels (Districts, States, etc.). - **Year Selection**:
Choose specific years or download the entire time series.

``` r
library(inkaR)
# Launch the Interactive Wizard
df <- inkaR() 
```

### 2. Analytical Multi-Indicator Download

You can download multiple datasets and join them automatically. Choose
between a “Long” (stacked) format or a “Wide” (analytical) format with
indicators as columns.

``` r
# Horizontal Join: Indicators as side-by-side columns
df_wide <- inkaR(
  variable = c("bip", "xbev"), 
  level    = "KRE", 
  year     = 2021, 
  lang     = "en", 
  format   = "wide"
)

# Ready for direct calculation:
# df_wide$bip_per_capita <- df_wide$bip / df_wide$`Total population`
```

### 3. Professional Mapping

`inkaR` integrates seamlessly with `sf` and `ggplot2` to render premium
maps.

``` r
# Plot with the premium High-End theme (Dark or Light mode)
plot_inkar(df_wide, mode = "dark")
```

## Available Spatial Levels

- `KRE`: Districts (Kreise / Kreisfreie Städte)
- `GEM`: Municipalities (Gemeinden)
- `ROR`: Spatial Planning Regions (Raumordnungsregionen)
- `BLD`: Federal States (Bundesländer)
- `BND`: Federal Territory (Bund)

You can explore the full spatial hierarchy via
[`get_geographies()`](https://ofurkancoban.github.io/inkaR/reference/get_geographies.md).

------------------------------------------------------------------------

## Scientific Attribution 🎓

If you use `inkaR` in your research, please cite it to support the
project and ensure reproducibility:

**APA Style:** \> Coban, O. F. (2026). inkaR: Download and Analyze
Spatial Development Data from BBSR INKAR. R package version 0.6.2.
<https://cran.r-project.org/web/packages/inkaR>

**BibTeX:**

``` bibtex
@Manual{,
  title = {inkaR: Download and Analyze Spatial Development Data from BBSR INKAR},
  author = {Omer Furkan Coban},
  year = {2026},
  note = {R package version 0.6.2},
  url = {https://cran.r-project.org/web/packages/inkaR},
}
```
