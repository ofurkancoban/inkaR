library(testthat)
library(inkaR)

# Small fake boundaries: two squares with INKAR-style Kennziffer
fake_geometry <- function(codes) {
  polys <- lapply(seq_along(codes), function(i) {
    sf::st_polygon(list(rbind(c(i, 0), c(i + 1, 0), c(i + 1, 1), c(i, 1), c(i, 0))))
  })
  sf::st_sf(Kennziffer = codes, geometry = sf::st_sfc(polys, crs = 3035))
}

test_that("gvb_lookup maps every Gemeinde to a 9-digit Verband code", {
  lookup <- inkaR:::gvb_lookup
  expect_gt(nrow(lookup), 10000)
  expect_false(anyDuplicated(lookup$AGS) > 0)
  expect_true(all(nchar(lookup$AGS) == 8))
  expect_true(all(nchar(lookup$GVB) == 9))
  # Kreisfreie Stadt: Verband code is the Kreis code plus "0000"
  expect_equal(lookup$GVB[lookup$AGS == "01001000"], "010010000")
})

test_that("plot_inkar uses GISCO boundaries for the detected level", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  requested <- NULL
  local_mocked_bindings(get_inkar_geometry = function(level, refresh = FALSE) {
    requested <<- level
    fake_geometry(c("01", "02"))
  })
  df <- data.frame(Kennziffer = c("01", "02"), Raumeinheit = c("A", "B"),
                   Aggregat = "Bundesländer", Zeit = 2020, Wert = c(1, 2))
  p <- suppressMessages(plot_inkar(df))
  expect_equal(requested, "BLD")
  expect_s3_class(p, "gg")
  expect_match(p$labels$caption, "EuroGeographics")
  expect_equal(sort(p$data$Wert), c(1, 2))
})

test_that("plot_inkar restores leading zeros of numeric IDs", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  local_mocked_bindings(get_inkar_geometry = function(level, refresh = FALSE) {
    fake_geometry(c("01001", "09162"))
  })
  df <- data.frame(Kennziffer = c(1001, 9162), Raumeinheit = c("A", "B"),
                   Aggregat = "Kreise", Zeit = 2020, Wert = c(1, 2))
  p <- suppressMessages(plot_inkar(df))
  expect_equal(sum(!is.na(p$data$Wert)), 2)
})

test_that("plot_inkar warns about regions without boundaries", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  local_mocked_bindings(get_inkar_geometry = function(level, refresh = FALSE) {
    fake_geometry(c("01001", "01002"))
  })
  df <- data.frame(Kennziffer = c("01001", "01002", "99999"), Raumeinheit = c("A", "B", "C"),
                   Aggregat = "Kreise", Zeit = 2020, Wert = 1:3)
  expect_warning(suppressMessages(plot_inkar(df)), "1 region\\(s\\) have no boundary.*99999")
})

test_that("custom geom has no GISCO attribution", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  df <- data.frame(Kennziffer = c("0101", "0102"), Aggregat = "ROR", Zeit = 2020, Wert = 1:2)
  p <- suppressMessages(plot_inkar(df, geom = fake_geometry(c("0101", "0102"))))
  expect_null(p$labels$caption)
})

test_that("get_inkar_geometry rejects ROR", {
  skip_if_not_installed("sf")
  expect_error(get_inkar_geometry("ROR"))
})

test_that("GISCO boundaries cover every INKAR region code", {
  skip_on_cran()
  skip_if(nzchar(Sys.getenv("CI")), "Skipping network test on CI")
  skip_if_not_installed("sf")
  reachable <- tryCatch({
    httr2::request("https://gisco-services.ec.europa.eu/distribution/v2/lau/datasets.json") |>
      httr2::req_timeout(20) |>
      httr2::req_perform()
    TRUE
  }, error = function(e) FALSE)
  skip_if(!reachable, "Eurostat GISCO not reachable")

  gem <- suppressMessages(get_inkar_geometry("GEM"))
  expect_equal(nrow(gem), 10978)
  expect_true(all(gem$Kennziffer %in% inkaR:::gvb_lookup$AGS))

  counts <- vapply(c("BND", "BLD", "KRE", "GVB"), function(lv) {
    nrow(suppressMessages(get_inkar_geometry(lv)))
  }, integer(1))
  expect_equal(unname(counts), c(1L, 16L, 400L, 4594L))
  expect_true("02000" %in% get_inkar_geometry("KRE")$Kennziffer)
})

test_that("quantile breaks use a binned color scale with readable labels", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  codes <- sprintf("%05d", 1:10)
  local_mocked_bindings(get_inkar_geometry = function(level, refresh = FALSE) fake_geometry(codes))
  df <- data.frame(Kennziffer = codes, Raumeinheit = codes, Aggregat = "Kreise",
                   Zeit = 2020, Wert = c(1, 2, 3, 4, 5, 10, 100, 1000, 1e5, 3e6))
  p <- suppressMessages(plot_inkar(df, breaks = "quantile"))
  fill <- p$scales$get_scales("fill")
  expect_s3_class(fill, "ScaleBinned")
  # Four quintile edges between the minimum and maximum
  expect_length(fill$breaks, 4)

  p_equal <- suppressMessages(plot_inkar(df))
  labels <- p_equal$scales$get_scales("fill")$get_labels()
  expect_false(any(grepl("e\\+", labels)))
})
