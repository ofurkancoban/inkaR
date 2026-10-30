# Weekly health check of the live services inkaR depends on.
#
# Run from the package root after installing the package. Writes a Markdown
# report and, on GitHub Actions, the step output `status` ("ok" or "problem").
# Every check is isolated: an unexpected error in one check is reported as a
# failure of that check and the others still run.
#
# Status levels:
#   ok      works as expected
#   notice  worth knowing, no action needed (does not open an issue)
#   warn    action needed, e.g. new indicators or changed territorial status
#   fail    something users will hit: no data, broken format, unreachable
#
# Environment:
#   HEALTH_REPORT                   report path (default: health-report.md)
#   INKAR_HEALTH_SIMULATE_FAILURE   "true" adds a failing check, to test alerts
#   INKAR_CA_BUNDLE_ERROR           set by the workflow if the CA bundle failed

report_file <- Sys.getenv("HEALTH_REPORT", "health-report.md")
results <- list()

record <- function(check, status, detail) {
  results[[length(results) + 1]] <<- list(check = check, status = status, detail = detail)
}

run_check <- function(check, fn) {
  tryCatch(fn(), error = function(e) {
    record(check, "fail", paste("Error:", conditionMessage(e)))
  })
}

bullet_list <- function(x, max = 20) {
  shown <- utils::head(x, max)
  more <- if (length(x) > max) paste0("\n  - ... and ", length(x) - max, " more") else ""
  paste0(paste0("\n  - ", shown, collapse = ""), more)
}

# 1. TLS chain of www.inkar.de without the workaround bundle -----------------
run_check("INKAR certificate chain", function() {
  bundle <- Sys.getenv("CURL_CA_BUNDLE")
  Sys.unsetenv("CURL_CA_BUNDLE")
  on.exit(if (nzchar(bundle)) Sys.setenv(CURL_CA_BUNDLE = bundle), add = TRUE)
  ok <- tryCatch({
    curl::curl_fetch_memory("https://www.inkar.de/", handle = curl::new_handle(nobody = TRUE))
    TRUE
  }, error = function(e) conditionMessage(e))
  if (isTRUE(ok)) {
    record("INKAR certificate chain", "notice",
           "www.inkar.de now sends its full certificate chain. The CURL_CA_BUNDLE workaround in the docs and the SSL hint in R/api.R may no longer be needed.")
  } else if (grepl("certificate|SSL", ok, ignore.case = TRUE)) {
    record("INKAR certificate chain", "ok",
           "Intermediate certificate still missing (known); the pinned CA bundle works around it.")
  } else {
    record("INKAR certificate chain", "fail", paste("Connection failed:", ok))
  }
})

# The workflow sets INKAR_CA_BUNDLE_ERROR when the server certificate is no
# longer signed by the intermediate pinned in .github/certs/
ca_error <- Sys.getenv("INKAR_CA_BUNDLE_ERROR")
if (nzchar(ca_error)) {
  record("Pinned INKAR certificate", "fail", paste(
    "The CA bundle could not be built:", ca_error,
    "Review the new issuer of www.inkar.de and update .github/certs/."))
}

# 2. API reachable ------------------------------------------------------------
api_ok <- FALSE
run_check("INKAR API reachable", function() {
  # perform_request() reports failures as messages; keep them for the report
  msgs <- character()
  resp <- withCallingHandlers(
    inkaR:::perform_request(inkaR:::inkar_request("Wizard/GetGebieteZumRaumbezug/KRE")),
    message = function(m) {
      msgs <<- c(msgs, trimws(conditionMessage(m)))
      invokeRestart("muffleMessage")
    }
  )
  if (is.null(resp) || length(resp) == 0) {
    stop("no response. ", paste(utils::head(msgs[nzchar(msgs)], 2), collapse = " "))
  }
  api_ok <<- TRUE
  record("INKAR API reachable", "ok", "Wizard endpoint answers.")
})

# 3. Data downloads in the documented format ----------------------------------
expected_rows <- c(BLD = 16, KRE = 400, GEM = 10978)
samples <- list(
  list(id = "q_alo", level = "KRE", lang = "de"),
  list(id = "bip", level = "KRE", lang = "de"),
  list(id = "xbev", level = "BLD", lang = "de"),
  list(id = "a_pend50", level = "GEM", lang = "de"),
  list(id = "q_alo", level = "KRE", lang = "en")
)
if (api_ok) {
  for (s in samples) {
    check <- sprintf("Download %s (%s, %s)", s$id, s$level, s$lang)
    run_check(check, function() {
      df <- suppressMessages(suppressWarnings(
        inkaR::get_inkar_data(s$id, level = s$level, year = "latest", lang = s$lang)
      ))
      cols <- if (s$lang == "en") c("region_id", "region_name", "year", "value") else c("Kennziffer", "Raumeinheit", "Zeit", "Wert")
      problems <- character()
      if (!is.data.frame(df) || nrow(df) == 0) {
        problems <- "no rows returned"
      } else {
        missing_cols <- setdiff(cols, names(df))
        if (length(missing_cols) > 0) problems <- c(problems, paste("missing columns:", paste(missing_cols, collapse = ", ")))
        # Some indicators lack values for a few Gemeinden, so allow up to 5%
        # fewer rows; more rows than regions means the territorial status changed
        n_expected <- expected_rows[[s$level]]
        if (nrow(df) > n_expected || nrow(df) < 0.95 * n_expected) {
          problems <- c(problems, sprintf("%d rows, expected %d", nrow(df), n_expected))
        }
        values <- df[[cols[4]]]
        if (!is.numeric(values)) {
          problems <- c(problems, "values are not numeric")
        } else if (mean(is.na(values)) > 0.05) {
          problems <- c(problems, sprintf("%.0f%% of values are NA", 100 * mean(is.na(values))))
        }
      }
      if (length(problems) > 0) {
        record(check, "fail", paste(problems, collapse = "; "))
      } else {
        record(check, "ok", sprintf("%d rows, latest year %s.", nrow(df), max(df[[cols[3]]])))
      }
    })
  }
} else {
  record("Data downloads", "fail", "Skipped because the API is not reachable.")
}

# 4. Indicator catalogue vs bundled metadata ----------------------------------
if (api_ok) {
  run_check("Indicator catalogue", function() {
    api <- inkaR:::fetch_api_indicators()
    if (is.null(api) || nrow(api) == 0) stop("the wizard returned no indicators")
    local <- inkaR::indicators
    active <- local[local$Active, ]
    new_ind <- api[!api$M_ID %in% active$M_ID, ]
    removed <- active[!active$M_ID %in% api$M_ID, ]
    if (nrow(new_ind) == 0 && nrow(removed) == 0) {
      record("Indicator catalogue", "ok", sprintf("%d indicators, same as the bundled metadata.", nrow(api)))
    } else {
      detail <- character()
      if (nrow(new_ind) > 0) {
        detail <- c(detail, paste0(nrow(new_ind), " new in INKAR:",
                                   bullet_list(paste0("`", new_ind$M_ID, "` ", new_ind$Name))))
      }
      if (nrow(removed) > 0) {
        detail <- c(detail, paste0(nrow(removed), " no longer offered:",
                                   bullet_list(paste0("`", removed$ID, "` (M_ID ", removed$M_ID, ") ", removed$Name_DE))))
      }
      detail <- c(detail, "The monthly metadata sync opens a pull request for these changes.")
      record("Indicator catalogue", "warn", paste(detail, collapse = "\n\n"))
    }
  })
}

# 5. INKAR region codes vs boundaries used for maps ---------------------------
gisco_ok <- tryCatch({
  httr2::request("https://gisco-services.ec.europa.eu/distribution/v2/lau/datasets.json") |>
    httr2::req_timeout(60) |>
    httr2::req_perform()
}, error = function(e) {
  record("Eurostat GISCO reachable", "fail", conditionMessage(e))
  NULL
})

if (!is.null(gisco_ok)) {
  record("Eurostat GISCO reachable", "ok", "Dataset list answers.")

  run_check("GISCO releases", function() {
    years <- as.integer(sub("^lau-", "", names(httr2::resp_body_json(gisco_ok))))
    used <- inkaR:::inkar_gisco_year
    if (max(years, na.rm = TRUE) > used) {
      record("GISCO releases", "notice", sprintf(
        "GISCO LAU %d is available; the package uses %d. Only switch when INKAR moves to a newer territorial status (see the boundary check below).",
        max(years, na.rm = TRUE), used))
    } else {
      record("GISCO releases", "ok", sprintf("The package uses the latest release (LAU %d).", used))
    }
  })

  if (api_ok && requireNamespace("sf", quietly = TRUE)) {
    for (level in c("BLD", "KRE", "GVB", "GEM")) {
      check <- sprintf("Boundaries cover INKAR regions (%s)", level)
      run_check(check, function() {
        inkar_codes <- suppressMessages(inkaR::get_geographies(level))$region_id
        geom_codes <- suppressMessages(inkaR::get_inkar_geometry(level))$Kennziffer
        if (length(inkar_codes) == 0) stop("INKAR returned no regions")
        missing <- setdiff(inkar_codes, geom_codes)
        if (length(missing) == 0) {
          record(check, "ok", sprintf("All %d regions have a boundary.", length(inkar_codes)))
        } else {
          record(check, "warn", paste0(
            length(missing), " of ", length(inkar_codes), " INKAR regions have no boundary, so maps miss them. ",
            "INKAR probably moved to a newer territorial status: update `inkar_gisco_year` in R/geometry.R and ",
            "`gv_date` in data-raw/gemeinde_gvb.R.", bullet_list(missing, 10)))
        }
      })
    }
  }
}

# 6. Optional simulated failure to test the alert path ------------------------
if (identical(tolower(Sys.getenv("INKAR_HEALTH_SIMULATE_FAILURE")), "true")) {
  record("Simulated failure", "fail", "Manual test run with simulate_failure = true. Not a real problem.")
}

# Report ----------------------------------------------------------------------
statuses <- vapply(results, `[[`, character(1), "status")
icons <- c(ok = "OK", notice = "NOTICE", warn = "WARN", fail = "FAIL")
problem <- any(statuses %in% c("warn", "fail"))

lines <- c(
  "## inkaR health check",
  "",
  sprintf("Run: %s UTC | inkaR %s | R %s",
          format(Sys.time(), "%Y-%m-%d %H:%M", tz = "UTC"),
          as.character(utils::packageVersion("inkaR")),
          paste(R.version$major, R.version$minor, sep = ".")),
  "",
  sprintf("**Result: %s** (%d ok, %d notice, %d warn, %d fail)",
          if (problem) "action needed" else "all good",
          sum(statuses == "ok"), sum(statuses == "notice"), sum(statuses == "warn"), sum(statuses == "fail")),
  ""
)
order_idx <- order(match(statuses, c("fail", "warn", "notice", "ok")))
for (r in results[order_idx]) {
  # Indent continuation lines so multi-line details stay inside the list item
  detail <- gsub("\n", "\n  ", r$detail)
  lines <- c(lines, sprintf("- **%s** %s: %s", icons[[r$status]], r$check, detail))
}
run_url <- Sys.getenv("GITHUB_RUN_URL")
if (nzchar(run_url)) lines <- c(lines, "", sprintf("[Workflow run](%s)", run_url))

writeLines(lines, report_file, useBytes = TRUE)
cat(lines, sep = "\n")

output <- Sys.getenv("GITHUB_OUTPUT")
if (nzchar(output)) {
  cat(sprintf("status=%s\n", if (problem) "problem" else "ok"), file = output, append = TRUE)
}
