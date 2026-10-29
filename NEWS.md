# inkaR 0.7.0

## New Features

* **Maps from Eurostat GISCO with INKAR's territorial status**: `plot_inkar()` now draws
  boundaries from GISCO LAU 2024 instead of GADM 4.1. GADM uses pre-2016 boundaries, so merged
  districts (e.g. Göttingen) and about 1% of Gemeinden were missing. With GISCO every region at
  `BND`, `BLD`, `KRE`, `GVB` and `GEM` is drawn (10,978 Gemeinden, 4,594 Gemeindeverbände,
  400 Kreise). Higher levels are dissolved from the Gemeinden; Gemeindeverbände use the Destatis
  Gemeindeverzeichnis (31.12.2023), stored as internal data. The file is downloaded once
  (about 43 MB) and cached. Maps carry the required "© EuroGeographics" attribution, and
  `plot_inkar()` warns when a region has no boundary.
* **Indicator metadata synced with the live API**: INKAR identifies indicators by the wizard
  key the API expects (`M_ID`) and by an official number from its indicator overview, which
  differ for 554 of 565 indicators. The bundled metadata held both as separate rows, so 400
  indicators appeared twice: once usable but undocumented, once documented (short ID such as
  `q_alo`, source, English texts) but marked inactive because its number returns no data.
  These pairs are now merged: 565 active indicators, 411 with their official short ID, and only
  2 genuinely discontinued ones. New columns `IndID` (official number) and `Short_DE` (official
  short name). `get_inkar_data()` accepts the short ID, the `M_ID` and the official number
  (`"q_alo"`, `"12"` and `"1101"` give the same data), and a name typed in full
  (`"Arbeitslosenquote"`) wins over longer names containing it. `Theme` now holds the 17 topic
  areas of the INKAR wizard, so `get_themes()` and the `theme` filters work. `Kreise`/`Gemeinden`
  show the years available at that level instead of the placeholders "Active"/"Unknown".
  Description, algorithm and source are filled for all active indicators (English description
  for about three quarters); 29 more indicators got a unit taken from their name. The three
  commuter indicators (`a_pend50`, `a_pend150`, `a_pend300`, Gemeinden level) are now active.
  The metadata is rebuilt by `data-raw/sync_indicators_api.R`.
* **`get_inkar_geometry()`**: new exported function returning the boundaries as an `sf` object
  with a `Kennziffer` column that joins directly with `get_inkar_data()`.
* **`digits` parameter**: `get_inkar_data()`, `inkaR()`, and `inkar()` now accept a `digits`
  argument to round numeric output values. The INKAR API uses single-precision floats, which
  can produce artifacts such as `8.44999...` instead of `8.45`. Setting `digits = 2` corrects
  this transparently (e.g. `inkar("12", digits = 2)`).

## Breaking Changes

* **`geodata` is no longer used**: automatic maps no longer depend on `geodata`/GADM.
* **Multi-indicator wide columns are named by short ID**: `get_inkar_data(c("xbev", "bip"), format = "wide")`
  now returns columns `xbev` and `bip` instead of names built from the truncated indicator title
  (e.g. `Bruttoinlandsprodukt_BIP_absolut_in_Mill`). The names are the same in `lang = "de"` and `"en"`.
* **Search and region filters match literally**: `search_indicators()`, the name lookup in
  `get_inkar_data()`, `compare_regions()`, `compare_districts()`, `inkar_trends(regions = )` and
  `plot_inkar(highlight = )` no longer interpret their input as a regular expression.
* **`compare_districts()` ID matching**: IDs now match as a prefix (`"09"` keeps all of Bavaria) or
  exactly with leading zeros ignored (`1001` matches `"01001"`), never in the middle of an ID.

## Bug Fixes

* **`BND` and `BLD` levels returned no data**: the INKAR API expects the codes `BU` (Bund) and
  `BL` (Laender). `BND`/`BLD` stay the public codes and are translated internally, so
  `get_inkar_data()`, `get_geographies()`, `select_level()` and `select_years()` now return data
  for both levels. The misleading "Most INKAR indicators are only available at KRE or GEM level"
  message has been removed.
* **Missing indicator names in downloads**: `Indikator`/`indicator_name` was `NA` for every
  indicator whose API key differs from its official number (554 of 565), because the name was
  joined on the official number. It is now joined on the API key.
* **`update_indicators()` never worked**: it sent an empty indicator list to `Wizard/GetMöglich`,
  which the API answers with HTTP 500, so it always reported "Could not reach INKAR API". It now
  reads the wizard's topic areas (`Wizard/GetBereiche`) and their indicators
  (`Wizard/GetIndikatorenZuBereich`), lists indicators missing from the local metadata and
  attaches them as attribute `"new_indicators"`; active local indicators the API dropped are
  reported as `"removed_indicators"`. The API's European (NUTS-2) indicators are ignored, since
  the package supports German levels only.
* **`plot_inkar(breaks = "quantile")`**: quantiles were only used as legend ticks on a continuous
  scale, so colors stayed linear and the labels overlapped. The map now uses five quantile classes
  with one color step each. Legend values are no longer shown in scientific notation (`3e+06`).
* **`plot_inkar()` level detection**: `"Bundesländer"` data was detected as `Bund` and drawn as a
  single national polygon; level detection now matches whole labels. `GEM` maps matched no
  region, because they were joined against GADM's Gemeindeverband layer.
* **Regex metacharacters**: `search_indicators("Bevölkerung (mit")` and the same text in
  `get_inkar_data()` failed with "invalid regular expression"; `compare_regions(df, "Frankfurt (Oder)")`
  silently returned no rows.
* **Name lookup prefers active indicators**: a name that matches active and inactive indicators
  resolves to the active one. The inactive warning now points to `search_indicators()`.
* **Wizard input**: in the terminal wizard, a multi-word name such as `Total population` was split
  into invalid IDs, and a short input such as `1` silently selected indicator `12`. Names are now
  matched as a whole, several matches narrow the list, and inputs under 3 characters are not
  fuzzy-matched.
* **Skipped indicators in multi-indicator downloads**: an indicator that is ambiguous or has no
  data now triggers a warning naming it. Previously a `NULL` result deleted its slot in the
  result list, so it was dropped silently.
* **Misleading warnings after a failed request**: when the API cannot be reached, `get_inkar_data()`
  no longer adds "API response missing expected columns" and "No data found for this
  variable/level combination" after the connection message.
* **Input validation**: `get_inkar_data()` stops with a clear error for `NA`, `""` or
  `character(0)`; `get_geographies()` rejects unknown level codes.
* **Umlauts in labels**: the `Kürzel` column of `view_indicators("de")` and the level menu of
  `select_level()` showed `<U+00FC>`-style escapes when the package was installed in a non-UTF-8
  locale.
* **Bundled `indicators` table not found after `library(inkaR)`**: with `LazyData: true` the
  dataset lives in the lazy-data environment, so `exists("indicators", envir = asNamespace("inkaR"))`
  was always `FALSE` outside `devtools::load_all()` and testthat. As a result `get_indicators()`
  returned 0 rows, `search_indicators()` errored, `get_themes()` was empty, `select_level()` /
  `select_years()` skipped ID resolution, and `get_inkar_data()` sent short IDs such as
  `"bev_korr"` to the API unresolved. All sites now use an internal `inkar_indicators()` helper.
  A regression test runs in a fresh R process via `callr` (added to Suggests).
* **Unresolvable indicator IDs**: `get_inkar_data()` now stops with a clear error (and
  suggestions when `stringdist` is installed) instead of passing an unknown non-numeric ID to
  the API and returning an empty result. Numeric M_IDs not in local metadata are still passed on.
* **Clearer SSL error**: when certificate verification fails, the message now explains that
  www.inkar.de does not send its intermediate certificate and how to supply a CA bundle via
  `CURL_CA_BUNDLE`.
* **`view_indicators()` DE**: `dplyr::any_of()` was incorrectly used as a rename target inside
  `dplyr::select()`. The `Statistische Grundlagen` column is now normalized with an explicit
  `dplyr::rename()` before the `select()` call, removing the silent column-drop.
* **`utils::adist()` invalid argument**: `method = "osa"` was passed to `utils::adist()`, which
  does not accept a `method` parameter. The argument has been removed from all call sites in
  `search_indicators()` and `select_indicator()`.
* **`dplyr::pick()` outside data-masking context**: Description/unit lookup in
  `get_inkar_data()` called `dplyr::pick()` inside an `if()` condition, which is only valid
  inside `mutate()` / `summarise()`. Replaced with pre-computed `has_desc_en` / `has_unit_en`
  logical flags evaluated before the `mutate()`.
* **Duplicate `M_ID` column after EN name join**: The English indicator name lookup joined a
  table that included `M_ID`, creating a spurious `M_ID.y` column. The column is now dropped
  from the lookup before the join.
* **`get_time_references()` schema guard**: Added an explicit check for required API response
  columns (`Gruppe`, `IndID`, `RaumID`, `ZeitID`) before calling `dplyr::select()`, producing
  an informative warning instead of a cryptic error when the API changes its response shape.
* **`api.R` redundant `requireNamespace("httr2")`**: Removed the runtime check; `httr2` is
  a hard `Imports` dependency and is always available.
* **Content-type check**: Changed `!= "application/json"` to
  `!startsWith(..., "application/json")` so that responses with a `charset=utf-8` suffix no
  longer trigger a spurious warning.
* **`globalVariables` false entry**: `select_level` is an exported function, not a free
  variable; removed from the `globalVariables()` declaration.
* **`data.R` Roxygen syntax**: Fixed a stray blank line before the closing `}` of the
  `\describe{}` block in the `indicators` dataset documentation.

## Enhancements

* **`get_geographies()` caching**: Region lists fetched from the API are now cached for
  7 days, eliminating a redundant network round-trip on every `get_inkar_data()` call.
* **`plot_inkar()` year type coercion**: The `year` parameter is now coerced with
  `as.integer()` before filtering, so `year = 2021L`, `year = 2021`, and `year = "2021"` all
  produce identical results.
* **`inkar_trends()` wide-format guard**: Passing wide-format data (year columns) to
  `inkar_trends()` now raises an informative error suggesting `format = "long"` instead of a
  generic column-not-found error.
* **Numeric format heuristic**: The value parser in `parse_inkar_json()` now distinguishes
  German locale formatting (`1.234,56` → `1234.56`) from English decimal notation (`1.5`)
  by checking for the co-occurrence of both `.` and `,` in the vector, preventing inadvertent
  corruption of English-formatted values.
* **`DESCRIPTION`**: Removed unused `lifecycle` entry from `Suggests`.

## Testing

* Added 21 new unit tests covering: `compare_districts()` with German-language data,
  `inkar_trends()` with German-language data, wide-format error message, `plot_inkar()` with
  long-format English data and custom `sf` geometry, `plot_inkar()` year parameter type
  coercion, `clear_inkar_cache()` file-deletion verification, and the new `digits` rounding
  parameter.
* Replaced non-standard `skip_on_ci()` calls with `skip_if(nzchar(Sys.getenv("CI")))`.
* Wrapped `search_indicators()` and `view_indicators()` calls in `capture.output()` to suppress
  `cat()` noise during test runs.
* Total: 68 test blocks with 147 expectations; live API and download tests are skipped on
  CRAN, on CI and when the server is unreachable.

# inkaR 0.6.6

## New Functions

* **`compare_districts()`** / **`compare_district()`**: Filter downloaded datasets by district IDs (Kennziffer) or names.
* **`compare_region()`**: Singular alias for `compare_regions()`.

## Enhancements

* **`plot_inkar()` upgrades**:
  * Added support for `BND` (Germany level 0) and `GEM` (Municipalities level 3) boundaries.
  * Added `geom` parameter to plot data on custom `sf` boundaries (e.g. `ROR` spatial planning regions).
  * Added a viridis map output image to the introduction vignette.
* **Base R Fuzzy Fallback**: Added a fallback using built-in `utils::adist()` when the `stringdist` package is not available.
* **CRAN Policy compliance**: Relocated usage history tracking from `~/.inkaR_history` to `tools::R_user_dir("inkaR", which = "config")` in interactive sessions, and disabled it entirely in non-interactive sessions.
* **API stability**: Added a null-response check to parallel request loops to handle timeouts gracefully.
* **Documentation**: Built a complete `pkgdown` package documentation website.

# inkaR 0.6.5


## New Functions

* **`inkar()`**: English-first shortcut alias for `get_inkar_data()` with `lang = "en"` and `year = "latest"` as defaults.
* **`compare_regions()`**: Filter a downloaded data frame to specific regions by partial name match.
* **`inkar_trends()`**: Plot time series as a `ggplot2` line chart for selected regions.
* **`get_themes()`**: List unique indicator themes from the local metadata for use with `theme` filtering.

## Enhancements

* **`year = "latest"`**: `get_inkar_data()` now accepts `year = "latest"` to automatically download only the most recent available year.
* **`plot_inkar()` improvements**: New `highlight`, `breaks` (equal/quantile), and `title` parameters for richer map customization.
* **`search_indicators()` / `view_indicators()`**: New `theme` parameter to pre-filter by indicator domain (see `get_themes()`).
* **pkgdown support**: Added `_pkgdown.yml` for documentation website generation.

# inkaR 0.6.4

## New Features

* **Smart ID resolver**: `get_inkar_data()` now resolves indicator IDs through five fallback tiers (exact, numeric, normalized, partial name, fuzzy).
* **Schema guarantee**: All 9 expected output columns are always present, even when the API returns partial data.
* **Readable wide-format column names**: Multi-indicator wide-format output now uses indicator names (e.g., `Gross_domestic_product`) instead of opaque IDs (`ind_11`).
* **Retry & timeout**: API requests now include a 30-second timeout and automatic retry.
* **Progress bar**: Multi-indicator downloads show a `cli` progress bar.
* **Improved fuzzy search**: `search_indicators()` now compares against `Name_EN` only and uses a tighter Jaro-Winkler threshold.
* **`update_indicators()` stability**: Gracefully handles API 500 errors instead of crashing.
* **BLD-level warning**: Informative message when BLD-level data is unavailable for a given indicator.

# inkaR 0.6.2

## Bug Fixes

* **CRAN Archive Fix**: `get_cache_dir()` no longer writes to `~/.cache/R/inkaR/`
  during non-interactive sessions (e.g. `R CMD check`). In non-interactive
  environments the cache now uses `tempdir()` (cleaned up at session end),
  while interactive sessions continue to use the persistent
  `tools::R_user_dir("inkaR", which = "cache")` directory. This resolves the
  NOTE that caused the package to be archived on CRAN
  (`checking for new files in some other directories`).

# inkaR 0.6.0

## New Features

* **Interactive Selection Wizard**: Calling `inkaR()` without arguments now opens a professional CLI wizard to guide users through selecting an indicator, then an available spatial level, and finally a year or year range.
* **Analytical Multi-Indicator Downloads**: Users can now pass a vector of IDs (e.g., `c("bip", "xbev")`) or enter multiple IDs in the wizard (separated by spaces).
* **Multi-Indicator Joins**: Automatically handles the merging of multiple indicators.
  * `format = "long"` (default) stacks indicators in a tidy format.
  * `format = "wide"` pivots indicators into individual columns for direct comparison and mathematical operations.
* **Fuzzy Search Engine**: Indicator selection now supports error-tolerant search using the `stringdist` package (Jaro-Winkler method).

## CLI Enhancements

* **Professional Table Display**: Rewrote indicator selection and search results using the `cli` package for high-end, paginated, and responsive terminal tables.
* **Usage History & Favorites**: The package now tracks frequently and recently used indicators, highlighting them in green and pinning them to the top of selection lists.
* **Intelligent Year Selection**: Implemented a professional 4-column layout for year selection with validated user input.

## Professional Visualizations

* **Premium Themes**: Introduced `theme_inkaR()`, a high-end ggplot2 theme with dedicated light and dark modes.
* **Upgraded Mapping**: `plot_inkar()` now supports bilingual labels, automatic unit extraction, and professional color palettes (Viridis, Magma).

# inkaR 0.4.4

* FIXED: Added graceful handling for SSL certificate verification issues on specific CRAN builders (e.g., Fedora).
* IMPROVED: Wrapped network-dependent examples in `try()` to ensure CRAN checks pass even with environment-specific network limitations.

# inkaR 0.4.3

* FIXED: Addressed CRAN feedback regarding single quotes in `DESCRIPTION`.
* FIXED: Added missing `\value` tags to exported `.Rd` files (e.g., `clear_inkar_cache()`).
* FIXED: Replaced `\dontrun{}` with `\donttest{}` or `if(interactive())` to ensure proper example testing.
* FIXED: Ensured CSV exporting examples do not write to the user home directory (`tempdir()`).

# inkaR 0.4.2

## Enhancements

* Added persistent disk-caching (`tools::R_user_dir`) to heavily improve `GetGebieteZumRaumbezug` lookup times across sessions.
* Implemented parallel API dispatching (`httr2::req_perform_parallel`) for exploring spatial levels, eliminating the sequential wait time.
* Introduced `plot_inkar()` for native geospatial mapping with `ggplot2` and `sf` by directly downloading and matching GADM polygon data.

## Bug Fixes

* Fixed missing mock payloads in `testthat` by completely regenerating `httptest2` records.
* Avoided decimal conversion errors (`NA`) by implementing robust locale-agnostic numeric parsing.
* Addressed spell checking NOTEs caught by win-builder.
