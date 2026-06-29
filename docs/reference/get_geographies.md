# Get Available Geographies or Region List

Retrieves a list of available spatial levels (if `geography` is NULL) or
a list of regions for a specific level (e.g., "KRE").

## Usage

``` r
get_geographies(geography = NULL)
```

## Arguments

- geography:

  Character. Spatial level code (e.g. "KRE"). If NULL, returns all
  levels.

## Value

A data frame with `ID` and `Name`.
