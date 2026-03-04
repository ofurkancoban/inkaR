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
