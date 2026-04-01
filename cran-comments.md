# CRAN Comments

## Resubmission for v0.6.2 (Archive Fix)

The package was archived on 2026-03-30 because the previous version left files
in `~/.cache/R/inkaR/` during `R CMD check`, which violates the CRAN policy of
not writing to the user's home filespace in non-interactive sessions.

**Root cause:** `get_cache_dir()` always called `tools::R_user_dir()`, even
during automated checks and tests, causing the NOTE:

```text
* checking for new files in some other directories ... NOTE
Found the following files/directories:
  '~/.cache/R/inkaR' '~/.cache/R/inkaR/times_11_KRE.qs'
```

**Fix applied in v0.6.2:** `get_cache_dir()` now detects non-interactive
sessions via `interactive()` and falls back to `file.path(tempdir(), "inkaR")`
in those cases. Persistent caching via `tools::R_user_dir()` is only used when
the package is loaded interactively by an end-user. The temp directory is
cleaned up automatically at the end of each R session.

## Previous Resubmission Notes (v0.6.1)

* **Fixed DESCRIPTION**: Single-quoted software names in Title and Description ('INKAR', 'BBSR', 'ggplot2') to comply with CRAN policies.
* **Fixed Misspellings**: Corrected/Quoted 'ggplot2' usage to resolve spelling notes.

## Test environments

* local OS X install, R 4.5.3
* macOS-latest, R-release (via GitHub Actions)
* ubuntu-latest, R-release (via GitHub Actions)
* windows-latest, R-release (via GitHub Actions)
* win-builder (R-devel)

## R CMD check results

0 errors | 0 warnings | 1 note

* The single NOTE is regarding "Possibly misspelled words in DESCRIPTION". These are
  false positives caused by German terms and technical acronyms (INKAR, BBSR, BND,
  BLD, ROR, KRE, GVB, GEM) which are not in the English dictionary.

## Downstream dependencies

There are currently no downstream dependencies.
