# inkaR News

## inkaR 0.6.6

### New Functions

* **`compare_districts()`** / **`compare_district()`**: Filter downloaded datasets by district IDs (Kennziffer) or names.
* **`compare_region()`**: Singular alias for `compare_regions()`.

### Enhancements

* **`plot_inkar()` upgrades**:
  * Added support for `BND` (Germany level 0) and `GEM` (Municipalities level 3) boundaries.
  * Added `geom` parameter to plot data on custom `sf` boundaries (e.g. `ROR` spatial planning regions).
  * Added a viridis map output image to the introduction vignette.
* **Base R Fuzzy Fallback**: Added a fallback using built-in `utils::adist()` when the `stringdist` package is not available.
* **CRAN Policy compliance**: Relocated usage history tracking from `~/.inkaR_history` to `tools::R_user_dir("inkaR", which = "config")` in interactive sessions, and disabled it entirely in non-interactive sessions.
* **API stability**: Added a null-response check to parallel request loops to handle timeouts gracefully.
* **Documentation**: Built a complete `pkgdown` package documentation website.

## inkaR 0.6.5


### New Functions

* **`inkar()`**: English-first shortcut alias for `get_inkar_data()` with `lang = "en"` and `year = "latest"` as defaults.
* **`compare_regions()`**: Filter a downloaded data frame to specific regions by partial name match.
* **`inkar_trends()`**: Plot time series as a `ggplot2` line chart for selected regions.
* **`get_themes()`**: List unique indicator themes from the local metadata for use with `theme` filtering.

### Enhancements

* **`year = "latest"`**: `get_inkar_data()` now accepts `year = "latest"` to automatically download only the most recent available year.
* **`plot_inkar()` improvements**: New `highlight`, `breaks` (equal/quantile), and `title` parameters for richer map customization.
* **`search_indicators()` / `view_indicators()`**: New `theme` parameter to pre-filter by indicator domain (see `get_themes()`).
* **pkgdown support**: Added `_pkgdown.yml` for documentation website generation.

## inkaR 0.6.4

### New Features

* **Smart ID resolver**: `get_inkar_data()` now resolves indicator IDs through five fallback tiers (exact, numeric, normalized, partial name, fuzzy).
* **Schema guarantee**: All 9 expected output columns are always present, even when the API returns partial data.
* **Readable wide-format column names**: Multi-indicator wide-format output now uses indicator names (e.g., `Gross_domestic_product`) instead of opaque IDs (`ind_11`).
* **Retry & timeout**: API requests now include a 30-second timeout and automatic retry.
* **Progress bar**: Multi-indicator downloads show a `cli` progress bar.
* **Improved fuzzy search**: `search_indicators()` now compares against `Name_EN` only and uses a tighter Jaro-Winkler threshold.
* **`update_indicators()` stability**: Gracefully handles API 500 errors instead of crashing.
* **BLD-level warning**: Informative message when BLD-level data is unavailable for a given indicator.

## inkaR 0.6.2

### Bug Fixes

* **CRAN Archive Fix**: `get_cache_dir()` no longer writes to `~/.cache/R/inkaR/`
  during non-interactive sessions (e.g. `R CMD check`). In non-interactive
  environments the cache now uses `tempdir()` (cleaned up at session end),
  while interactive sessions continue to use the persistent
  `tools::R_user_dir("inkaR", which = "cache")` directory. This resolves the
  NOTE that caused the package to be archived on CRAN
  (`checking for new files in some other directories`).

## inkaR 0.6.0

### New Features

* **Interactive Selection Wizard**: Calling `inkaR()` without arguments now opens a professional CLI wizard to guide users through selecting an indicator, then an available spatial level, and finally a year or year range.
* **Analytical Multi-Indicator Downloads**: Users can now pass a vector of IDs (e.g., `c("bip", "xbev")`) or enter multiple IDs in the wizard (separated by spaces).
* **Multi-Indicator Joins**: Automatically handles the merging of multiple indicators.
  * `format = "long"` (default) stacks indicators in a tidy format.
  * `format = "wide"` pivots indicators into individual columns for direct comparison and mathematical operations.
* **Fuzzy Search Engine**: Indicator selection now supports error-tolerant search using the `stringdist` package (Jaro-Winkler method).

### CLI Enhancements

* **Professional Table Display**: Rewrote indicator selection and search results using the `cli` package for high-end, paginated, and responsive terminal tables.
* **Usage History & Favorites**: The package now tracks frequently and recently used indicators, highlighting them in green and pinning them to the top of selection lists.
* **Intelligent Year Selection**: Implemented a professional 4-column layout for year selection with validated user input.

### Professional Visualizations

* **Premium Themes**: Introduced `theme_inkaR()`, a high-end ggplot2 theme with dedicated light and dark modes.
* **Upgraded Mapping**: `plot_inkar()` now supports bilingual labels, automatic unit extraction, and professional color palettes (Viridis, Magma).

## inkaR 0.4.4

* FIXED: Added graceful handling for SSL certificate verification issues on specific CRAN builders (e.g., Fedora).
* IMPROVED: Wrapped network-dependent examples in `try()` to ensure CRAN checks pass even with environment-specific network limitations.

## inkaR 0.4.3

* FIXED: Addressed CRAN feedback regarding single quotes in `DESCRIPTION`.
* FIXED: Added missing `\value` tags to exported `.Rd` files (e.g., `clear_inkar_cache()`).
* FIXED: Replaced `\dontrun{}` with `\donttest{}` or `if(interactive())` to ensure proper example testing.
* FIXED: Ensured CSV exporting examples do not write to the user home directory (`tempdir()`).

## inkaR 0.4.2

### Enhancements

* Added persistent disk-caching (`tools::R_user_dir`) to heavily improve `GetGebieteZumRaumbezug` lookup times across sessions.
* Implemented parallel API dispatching (`httr2::req_perform_parallel`) for exploring spatial levels, eliminating the sequential wait time.
* Introduced `plot_inkar()` for native geospatial mapping with `ggplot2` and `sf` by directly downloading and matching GADM polygon data.

### Bug Fixes

* Fixed missing mock payloads in `testthat` by completely regenerating `httptest2` records.
* Avoided decimal conversion errors (`NA`) by implementing robust locale-agnostic numeric parsing.
* Addressed spell checking NOTEs caught by win-builder.
