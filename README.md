
<!-- README.md is generated from README.Rmd. Please edit that file -->

# inkaR <img src="man/figures/logo.svg" align="right" height="139" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/ofurkancoban/inkaR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ofurkancoban/inkaR/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The `inkaR` package provides a modern, fast, and lightweight R interface
to download spatial development indicators from the [BBSR
INKAR](https://www.inkar.de/) (Indikatoren und Karten zur Raum- und
Stadtentwicklung) database.

Designed for researchers and data scientists, `inkaR` abstracts away the
complex JSON API of INKAR into simple, tidy data frames (`tibbles`). It
features intelligent persistent caching, parallel API queries, and
native geospatial mapping integration to instantly visualize regional
data across Germany.

## Installation

You can install the development version of inkaR from
[GitHub](https://github.com/ofurkancoban/inkaR) with:

``` r
# install.packages("devtools")
devtools::install_github("ofurkancoban/inkaR")
```

## Features

1.  **Modern Architecture**: Uses `httr2` for bulletproof HTTP requests
    and retry logic.
2.  **Offline Caching**: Automatically saves API metadata to disk
    (`tools::R_user_dir()`), speeding up repeated spatial lookups by up
    to 10x.
3.  **Parallel Networking**: Uses asynchronous queries
    (`req_perform_parallel`) to ping 6 spatial levels simultaneously,
    dropping discovery delays from 15 seconds to under 2 seconds.
4.  **Offline Resiliency**: Includes `httptest2` integration so the
    entire test suite works without internet access, perfect for CI/CD
    environments.
5.  **Geospatial Mapping**: Built-in support to render data directly
    onto German administrative maps using `ggplot2` and `geodata`.

## Basic Usage

The workflow consists of finding an indicator ID, ensuring its spatial
level is available, and downloading it.

### 1. View Available Indicators

``` r
library(inkaR)

# Open an interactive viewer to search for indicators like 'BIP' or 'Population'
# Supports bilingual metadata: lang = "de" or lang = "en"
view_indicators(lang = "en")
```

### 2. Download Data

Once you locate your target indicator ID (e.g., “011” for Gross Domestic
Product) and a spatial group (e.g., “KRE” for Districts), download the
data:

``` r
library(inkaR)
# Download Gross Domestic Product (BIP) for all German Districts (2021)
df <- get_inkar_data("011", level = "KRE", year = 2021)

head(df)
#>   Kennziffer            Raumeinheit Aggregat M_ID
#> 1      01001       Flensburg, Stadt   Kreise   11
#> 2      01002 Kiel, Landeshauptstadt   Kreise   11
#> 3      01003     Lübeck, Hansestadt   Kreise   11
#> 4      01004      Neumünster, Stadt   Kreise   11
#> 5      01051           Dithmarschen   Kreise   11
#> 6      01053    Herzogtum Lauenburg   Kreise   11
#>                                              Indikator
#> 1 Bruttoinlandsprodukt (BIP) absolut in Millionen Euro
#> 2 Bruttoinlandsprodukt (BIP) absolut in Millionen Euro
#> 3 Bruttoinlandsprodukt (BIP) absolut in Millionen Euro
#> 4 Bruttoinlandsprodukt (BIP) absolut in Millionen Euro
#> 5 Bruttoinlandsprodukt (BIP) absolut in Millionen Euro
#> 6 Bruttoinlandsprodukt (BIP) absolut in Millionen Euro
#>                                           Beschreibung    Einheit Zeit     Wert
#> 1 Bruttoinlandsprodukt (BIP) absolut in Millionen Euro 1.000 Euro 2021  3992237
#> 2 Bruttoinlandsprodukt (BIP) absolut in Millionen Euro 1.000 Euro 2021 12468565
#> 3 Bruttoinlandsprodukt (BIP) absolut in Millionen Euro 1.000 Euro 2021 11196036
#> 4 Bruttoinlandsprodukt (BIP) absolut in Millionen Euro 1.000 Euro 2021  3918518
#> 5 Bruttoinlandsprodukt (BIP) absolut in Millionen Euro 1.000 Euro 2021  5082480
#> 6 Bruttoinlandsprodukt (BIP) absolut in Millionen Euro 1.000 Euro 2021  5010598
```

### 3. Native Mapping

If you have the optional `sf`, `ggplot2`, and `geodata` packages
installed, `inkaR` can seamlessly map your downloaded data:

``` r
# Renders a high-quality map of the data
plot_inkar(df)
```

*(Note: The first plot call will automatically download GADM boundary
shapes for the matched spatial level.)*

## Available Spatial Levels

-   `KRE`: Districts (Kreise / Kreisfreie Städte)
-   `GEM`: Municipalities (Gemeinden)
-   `ROR`: Spatial Planning Regions (Raumordnungsregionen)
-   `BLD`: Federal States (Bundesländer)
-   `BND`: Federal Territory (Bund)

You can view complete structure relationships programmatically via
`get_geographies()`.
