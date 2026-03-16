# CRAN Comments

## New Feature Release: v0.6.0

This major update introduces a professional, bilingual interface and analytical multi-indicator support:

*   **Interactive Selection Wizard**: A guided CLI flow for selecting indicators, spatial levels, and years (bilingual German/English).
*   **Multi-Indicator Support**: Ability to download multiple indicators simultaneously.
*   **Analytical Joins**: Support for both vertical (long/tidy) and horizontal (wide/analytical) joins for multi-indicator research.
*   **Fuzzy Search**: Improved indicator discovery with string distance matching.
*   **Premium Visualizations**: New `theme_inkaR()` and plot enhancements for modern, publication-ready maps with dark mode support.
*   **Maintenance**: Cleaned up the package workspace by removing redundant metadata sources and optimizing internal data storage.

## Test environments

* local OS X install, R 4.5.3
* macOS-latest, R-release (via GitHub Actions)
* ubuntu-latest, R-release (via GitHub Actions)
* windows-latest, R-release (via GitHub Actions)

## R CMD check results
0 errors | 0 warnings | 1 note

* The single NOTE is regarding "Possibly misspelled words in DESCRIPTION". These are false positives caused by the German terms and technical acronyms (INKAR, BBSR, BND, BLD, ROR, KRE, GVB, GEM) which are not in the English dictionary.

## Downstream dependencies
There are currently no downstream dependencies.
