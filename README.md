# inkaR: INKAR Data Access for R

This package provides a lightweight, modern interface to download data from the [BBSR INKAR](https://www.inkar.de/) database.

## Features
- **Modern Backend**: Uses `httr2` for robust API requests.
- **Bilingual**: Search and retrieve indicator metadata in German (DE) or English (EN).
- **Offline Metadata**: Includes a bundled dataset of indicators for fast searching.

## Installation

```r
# Install from local source
devtools::install(".")
```

## Usage

### 1. Find Indicators
Search for data you are interested in.
```r
library(inkaR)

# List all indicators (German)
all_inds <- get_indicators(lang = "de")

# Search for "Population" (English)
# Note: Requires EN metadata to be populated in the dataset
pop_inds <- search_indicators("Population", lang = "en")
```

### 2. Download Data
Use the `ID` (Shortname) from the search results.

```r
# Download data for indicator "001" (Total Population) for Districts (Kreise)
df <- get_inkar_data(variable = "001", level = "Kreise")
```

## Setup (Development)
If you have a new `Uebersicht der Indikatoren.xlsx` file, update the package data by running:

```r
source("data-raw/process_metadata.R")
```
