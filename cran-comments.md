# CRAN Comments

## Resubmission for v0.4.3

This is a resubmission addressing feedback from CRAN maintainer Konstanze Lauseker:

* **Fixed DESCRIPTION**: Removed single quotes around non-package terms ("INKAR", "BBSR", etc.).
* **Fixed Examples**: Replaced `\dontrun{}` tags with `\donttest{}` for data-downloading functions, and `if(interactive())` for interactive menu functions.
* **Fixed .Rd output files**: Added missing `\value` tags describing the outputs of all exported functions, including functions with side-effects like `clear_inkar_cache()`.
* **Fixed file paths**: Adjusted `get_inkar_data(csv=TRUE)` examples to properly write outputs to `tempdir()` to comply with CRAN policies regarding user home filespaces.

## Test environments

* local OS X install, R 4.5
* macOS-latest, R-release (via GitHub Actions)
* ubuntu-latest, R-release (via GitHub Actions)
* windows-latest, R-release (via GitHub Actions)

## R CMD check results
0 errors | 0 warnings | 1 notes

* This is a new release.
* The single NOTE is regarding "Possibly misspelled words in DESCRIPTION". These are false positives caused by the German terms ("und", "zur", "Raum", "Karten", "Indikatoren", "Stadtentwicklung") and the package acronyms (INKAR, BBSR) which are not inside the English dictionary. 

## Downstream dependencies
There are currently no downstream dependencies for this package.
