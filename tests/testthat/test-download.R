library(testthat)
library(inkaR)

test_that("normalize_id strips punctuation and lowercases", {
  expect_equal(inkaR:::normalize_id("q_alo"), "qalo")
  expect_equal(inkaR:::normalize_id("Q_ALO"), "qalo")
  expect_equal(inkaR:::normalize_id("bip"), "bip")
  expect_equal(inkaR:::normalize_id("BIP-2"), "bip2")
})

test_that("get_inkar_data errors for an unresolvable variable", {
  # Fails during local ID resolution, before any API request is made
  expect_error(
    suppressMessages(get_inkar_data("__nonexistent__xyz__", level = "KRE")),
    "Could not resolve indicator"
  )
})

test_that("the indicator table is found in a normal library() session", {
  # devtools::load_all() and testthat both make the lazy data visible in the
  # namespace, which hid the exists(envir = asNamespace()) bug. Check in a
  # fresh R process against the installed package instead.
  skip_on_cran()
  skip_if_not_installed("callr")
  n <- callr::r(function() {
    suppressWarnings(nrow(inkaR::get_indicators("de")))
  })
  expect_gt(n, 0)
  hits <- callr::r(function() {
    utils::capture.output(res <- inkaR::search_indicators("Einwohner"))
    nrow(res)
  })
  expect_gt(hits, 0)
  themes <- callr::r(function() length(inkaR::get_themes()))
  expect_gt(themes, 0)
})

test_that("search_indicators returns data frame for known term", {
  capture.output(
    result <- suppressMessages(search_indicators("population", lang = "en")),
    type = "output"
  )
  expect_true(is.data.frame(result))
})

test_that("search_indicators fuzzy match does not error on typo", {
  capture.output(
    expect_no_error(suppressMessages(search_indicators("employmnt", lang = "en"))),
    type = "output"
  )
})

test_that("search_indicators returns invisible tibble for no match without stringdist", {
  result <- suppressMessages(search_indicators("zzznomatchzzz", lang = "en"))
  expect_true(is.data.frame(result))
})

test_that("update_indicators is exported and callable", {
  expect_true(is.function(update_indicators))
})

test_that("BND and BLD levels return data", {
  # The API expects BU/BL; BND/BLD used to return 0 rows for every indicator
  skip_on_cran()
  skip_if(nzchar(Sys.getenv("CI")), "Skipping network test on CI")
  skip_if_inkar_unreachable()
  bld <- suppressMessages(get_inkar_data("xbev", level = "BLD", year = 2020))
  bnd <- suppressMessages(get_inkar_data("xbev", level = "BND", year = 2020))
  expect_equal(nrow(bld), 16)
  expect_equal(nrow(bnd), 1)
  expect_equal(sum(bld$Wert), bnd$Wert, tolerance = 1e-4)
  expect_equal(nrow(suppressMessages(get_geographies("BLD"))), 16)
})

test_that("to_api_level maps public level codes to API codes", {
  expect_equal(
    inkaR:::to_api_level(c("BND", "BLD", "ROR", "KRE", "GVB", "GEM")),
    c("BU", "BL", "ROR", "KRE", "GVB", "GEM")
  )
})

test_that("get_inkar_data rejects empty or missing variable", {
  expect_error(get_inkar_data(NA), "non-empty")
  expect_error(get_inkar_data(""), "non-empty")
  expect_error(get_inkar_data(character(0)), "non-empty")
  expect_error(get_inkar_data(c("xbev", NA)), "non-empty")
})

test_that("get_geographies rejects an unknown level", {
  expect_error(get_geographies("XYZ"), "Invalid 'geography'")
})

test_that("name with regex metacharacters resolves without error", {
  # "(" used to be passed to grepl() as a regex and fail
  local_mocked_bindings(get_time_references = function(variable, level) stop("resolved:", variable))
  err <- tryCatch(suppressMessages(get_inkar_data("Bev\u00f6lkerung (mit", "KRE")),
                  error = function(e) conditionMessage(e))
  expect_match(err, "resolved:5")
})

test_that("name resolution prefers active indicators", {
  fake <- tibble::tibble(
    ID = c("old_dichte", "new_dichte"), M_ID = c(900, 901),
    Name_DE = c("Testdichte alt", "Testdichte neu"),
    Name_EN = c("Test density old", "Test density new"),
    Description_DE = c("", ""), Active = c(FALSE, TRUE)
  )
  local_mocked_bindings(
    inkar_indicators = function() fake,
    get_time_references = function(variable, level) stop("resolved:", variable)
  )
  err <- tryCatch(suppressMessages(get_inkar_data("Testdichte", "KRE")),
                  error = function(e) conditionMessage(e))
  expect_match(err, "resolved:901")
})

test_that("multi-indicator download warns about skipped indicators", {
  local_mocked_bindings(get_time_references = function(variable, level) {
    if (variable == "2") {
      tibble::tibble(Gruppe = "2", IndID = "2", RaumID = "KRE", ZeitID = "2020",
                     Zeit = "2020", Indikator = "x", Raum = "Kreise")
    } else {
      tibble::tibble()
    }
  })
  local_mocked_bindings(perform_request = function(req) {
    list(stats::setNames(
      list("01001", "2020", "1", "KRE", "2"),
      c("Schl\u00fcssel", "ZeitID", "Wert", "Raumbezug", "Indikator")
    ))
  })
  local_mocked_bindings(get_geographies = function(geography = NULL) tibble::tibble())
  warns <- character()
  res <- withCallingHandlers(
    suppressMessages(get_inkar_data(c("xbev", "bip"), "KRE", 2020)),
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_true(any(grepl("No data for: bip", warns)))
  expect_equal(nrow(res), 1)
})

test_that("multi-indicator download warns when a name is ambiguous", {
  fake <- tibble::tibble(
    ID = c("a", "dup1", "dup2"), M_ID = c(2, 901, 902),
    Name_DE = c("Alpha", "Dup eins", "Dup zwei"), Name_EN = c("Alpha", "Dup one", "Dup two"),
    Description_DE = "", Description_EN = "", Unit_DE = NA_character_, Unit_EN = NA_character_,
    Active = TRUE
  )
  local_mocked_bindings(
    inkar_indicators = function() fake,
    get_time_references = function(variable, level) {
      tibble::tibble(Gruppe = "2", IndID = "2", RaumID = "KRE", ZeitID = "2020",
                     Zeit = "2020", Indikator = "x", Raum = "Kreise")
    },
    perform_request = function(req) {
      list(stats::setNames(
        list("01001", "2020", "1", "KRE", "2"),
        c("Schl\u00fcssel", "ZeitID", "Wert", "Raumbezug", "Indikator")
      ))
    },
    get_geographies = function(geography = NULL) tibble::tibble()
  )
  # "Dup" matches two indicators, so it returns NULL; that NULL must not
  # silently vanish from the result list
  expect_warning(
    res <- suppressMessages(get_inkar_data(c("a", "Dup"), "KRE", 2020)),
    "No data for: Dup"
  )
  expect_equal(nrow(res), 1)
})

test_that("failed request does not add a misleading 'No data found' warning", {
  local_mocked_bindings(perform_request = function(req) NULL)
  expect_no_warning(res <- suppressMessages(get_inkar_data("xbev", "KRE", 2020)))
  expect_equal(nrow(res), 0)
})

test_that("compare_districts matches IDs without leading zeros and by prefix", {
  df <- data.frame(
    Kennziffer = c("01001", "01002", "09162", "11000"),
    Raumeinheit = c("Flensburg, Stadt", "Kiel", "M\u00fcnchen", "Berlin"),
    stringsAsFactors = FALSE
  )
  expect_equal(compare_districts(df, 1001, exact = TRUE)$Kennziffer, "01001")
  expect_equal(compare_districts(df, "09")$Kennziffer, "09162")
  # "100" must not match "01001" in the middle of the ID
  expect_equal(nrow(suppressMessages(compare_districts(df, "100"))), 0)
})

test_that("compare_regions matches names with parentheses literally", {
  df <- data.frame(
    Kennziffer = c("12053", "06412"),
    Raumeinheit = c("Frankfurt (Oder), Stadt", "Frankfurt am Main, Stadt"),
    stringsAsFactors = FALSE
  )
  expect_equal(compare_regions(df, "Frankfurt (Oder)")$Kennziffer, "12053")
})

test_that("search_indicators handles regex metacharacters", {
  capture.output(
    res <- suppressMessages(search_indicators("Bev\u00f6lkerung (mit")),
    type = "output"
  )
  expect_true(nrow(res) >= 1)
  expect_no_error(capture.output(suppressMessages(search_indicators("(")), type = "output"))
})

test_that("wizard input matching handles names and short input", {
  df <- get_indicators("en")
  expect_equal(inkaR:::match_indicator_input("Total population", df), "xbev")
  expect_null(inkaR:::match_indicator_input("1", df))
  several <- inkaR:::match_indicator_input("population", df)
  expect_true(is.data.frame(several) && nrow(several) > 1)
})

test_that("detect_map_level does not confuse Bundeslaender with Bund", {
  expect_equal(inkaR:::detect_map_level("Bundesl\u00e4nder"), "BLD")
  expect_equal(inkaR:::detect_map_level("Bund"), "BND")
  expect_equal(inkaR:::detect_map_level("Gemeindeverb\u00e4nde"), "GVB")
  expect_equal(inkaR:::detect_map_level("Gemeinden"), "GEM")
  expect_true(is.na(inkaR:::detect_map_level("Raumordnungsregionen")))
})

test_that("inkaR() and inkar() pass arguments to get_inkar_data()", {
  local_mocked_bindings(get_inkar_data = function(...) list(...))
  a <- inkaR("xbev", year = 2020, digits = 1)
  expect_equal(a$level, "KRE")
  expect_equal(a$lang, "de")
  expect_equal(a$digits, 1)
  b <- inkar("xbev", format = "wide")
  expect_equal(b$year, "latest")
  expect_equal(b$lang, "en")
  expect_equal(b$format, "wide")
})

test_that("update_indicators reports new indicators", {
  local_mocked_bindings(fetch_api_indicators = function() {
    data.frame(M_ID = c(2, 999999), Name = c("Bev\u00f6lkerung gesamt", "Neuer Indikator"),
               Bereich = "000", stringsAsFactors = FALSE)
  })
  msgs <- capture_messages(res <- update_indicators())
  expect_match(paste(msgs, collapse = ""), "1 new indicator")
  expect_equal(attr(res, "new_indicators")$M_ID, 999999)
  expect_equal(nrow(res), nrow(inkaR::indicators))
})

test_that("update_indicators reports up-to-date metadata", {
  active <- inkaR::indicators$M_ID[inkaR::indicators$Active]
  local_mocked_bindings(fetch_api_indicators = function() {
    data.frame(M_ID = active, Name = "x", Bereich = "000")
  })
  msgs <- capture_messages(res <- update_indicators())
  expect_match(paste(msgs, collapse = ""), "up to date")
  expect_null(attr(res, "new_indicators"))
  expect_null(attr(res, "removed_indicators"))
})

test_that("update_indicators reports indicators the API no longer offers", {
  active <- inkaR::indicators$M_ID[inkaR::indicators$Active]
  local_mocked_bindings(fetch_api_indicators = function() {
    data.frame(M_ID = active[-1], Name = "x", Bereich = "000")
  })
  msgs <- capture_messages(res <- update_indicators())
  expect_match(paste(msgs, collapse = ""), "no longer offered")
  expect_equal(attr(res, "removed_indicators")$M_ID, active[1])
})

test_that("indicators metadata links short IDs, M_ID and official numbers", {
  inds <- inkaR::indicators
  expect_false(anyDuplicated(inds$ID) > 0)
  expect_false(anyDuplicated(inds$M_ID) > 0)
  expect_true(all(!is.na(inds$IndID)))
  # Arbeitslosenquote: short ID q_alo, API key 12, official number 1101
  q <- inds[inds$ID == "q_alo", ]
  expect_equal(q$M_ID, 12)
  expect_equal(q$IndID, 1101)
  expect_true(q$Active)
  expect_true(all(!is.na(inds$Theme[inds$Active])))
})

test_that("short ID and official number resolve to the API key", {
  local_mocked_bindings(get_time_references = function(variable, level) stop("resolved:", variable))
  for (v in c("q_alo", "12", "1101")) {
    err <- tryCatch(suppressMessages(get_inkar_data(v, "KRE")), error = function(e) conditionMessage(e))
    expect_match(err, "resolved:12", info = v)
  }
})

test_that("official short names are searchable and resolve exactly", {
  capture.output(hits <- suppressMessages(search_indicators("Arbeitslosenquote")), type = "output")
  expect_true("q_alo" %in% hits$ID)
  local_mocked_bindings(get_time_references = function(variable, level) stop("resolved:", variable))
  # The full name wins over "Arbeitslosenquote Frauen" and similar names
  err <- tryCatch(suppressMessages(get_inkar_data("arbeitslosenquote", "KRE")),
                  error = function(e) conditionMessage(e))
  expect_match(err, "resolved:12")
  df <- get_indicators("de")
  expect_equal(inkaR:::match_indicator_input("Arbeitslosenquote", df), "q_alo")
})

test_that("downloaded data carries the indicator name when Gruppe and IndID differ", {
  skip_on_cran()
  skip_if(nzchar(Sys.getenv("CI")), "Skipping network test on CI")
  skip_if_inkar_unreachable()
  df <- suppressMessages(get_inkar_data("q_alo", "KRE", 2020))
  expect_equal(nrow(df), 400)
  expect_false(anyNA(df$Indikator))
  expect_false(anyNA(df$Beschreibung))
  expect_equal(unique(df$M_ID), "12")
})

test_that("update_indicators handles an unreachable API", {
  local_mocked_bindings(perform_request = function(req) NULL)
  res <- suppressMessages(update_indicators())
  expect_equal(nrow(res), nrow(inkaR::indicators))
})

test_that("fetch_api_indicators lists indicators from the live API", {
  skip_on_cran()
  skip_if(nzchar(Sys.getenv("CI")), "Skipping network test on CI")
  skip_if_inkar_unreachable()
  api <- inkaR:::fetch_api_indicators()
  expect_gt(nrow(api), 500)
  expect_true(all(c(2, 5, 11) %in% api$M_ID))
})
