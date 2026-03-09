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
