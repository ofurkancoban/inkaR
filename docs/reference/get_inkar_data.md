# Download Data from INKAR

Retrieves statistical data for a given variable and spatial level.
Automatically handles time reference lookup.

## Usage

``` r
get_inkar_data(
  variable,
  level = "KRE",
  year = NULL,
  lang = c("de", "en"),
  format = c("long", "wide"),
  csv = FALSE,
  export_dir = NULL
)
```

## Arguments

- variable:

  Character. The indicator ID (Shortname), e.g., "011".

- level:

  Character. Spatial level code (e.g., "KRE" for Kreise).

- year:

  Integer/Character vector. Specific year (e.g., 2021) or range (e.g.,
  2010:2020). If NULL, fetches all available years.

- lang:

  Character. "de" (default) for German column names, "en" for English.

- format:

  Character. "long" (default) for tidy format, "wide" for years as
  columns.

- csv:

  Logical. If TRUE, saves the data to a CSV file in the directory
  specified by `export_dir`.

- export_dir:

  Character. Directory to save the CSV file if `csv = TRUE`. If `NULL`
  (default), it saves to the current working directory (`"."`).

## Value

A tibble containing the data.
