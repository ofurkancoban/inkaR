# Interactively Select a Spatial Level

Provides an interactive console menu to choose an INKAR spatial level.
If a variable ID is provided, it probes the live API to find which
levels actually have data for that indicator.

## Usage

``` r
select_level(variable = NULL)
```

## Arguments

- variable:

  Optional character. The indicator ID to probe available levels.

## Value

Character. The selected level ID, e.g., "KRE".
