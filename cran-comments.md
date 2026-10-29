# CRAN Comments

## Update to v0.7.0

This is an update of the CRAN version 0.6.6. Main changes (see NEWS.md):

* The bundled indicator metadata was synchronised with the live INKAR API, so
  every active indicator now resolves to the key the API expects.
* Maps now use administrative boundaries from Eurostat GISCO instead of
  'geodata' (removed from Suggests). The new exported function
  `get_inkar_geometry()` returns these boundaries as an `sf` object.
* Several bug fixes in downloads, indicator search and plotting.

## Network access

* All functions that contact the INKAR API or Eurostat GISCO fail with an
  informative error when the service is unreachable.
* Examples that need the network are wrapped in `\donttest{}` or
  `if (interactive())`. The vignette is not evaluated during the build.
* Tests that need the network are skipped on CRAN and on CI; all other tests
  use mocked responses.
* The download cache is written to `tempdir()` in non-interactive sessions, so
  `R CMD check` leaves no files in the user's home filespace.
* Examples that download data only use the result when the download returned
  rows, so they pass with `--run-donttest` even when INKAR is unreachable.

## Test environments

* local macOS (Darwin 25.6.0), R 4.6.0

## R CMD check results

0 errors | 0 warnings | 1 note

* The NOTE ("checking HTML version of manual ... Skipping checking HTML
  validation: 'tidy' doesn't look like recent enough HTML Tidy") comes from
  the outdated HTML Tidy on the local machine, not from the package.

The check was run with `--as-cran --run-donttest`.

On machines that do not have the intermediate certificate of www.inkar.de, the
URL check may also report:

```text
Found the following (possibly) invalid URLs:
  URL: https://www.inkar.de/
    Message: libcurl error code 60:
      SSL certificate problem: unable to get local issuer certificate
      (Status without verification: OK)
```

The URL is correct and is the official data source. The INKAR server does not
send its intermediate certificate (GoDaddy TLS Intermediate CA DV - R1v1), so
curl cannot verify the chain although browsers can. The same URL was part of
the current CRAN version 0.6.6.

## Downstream dependencies

There are currently no downstream dependencies.
