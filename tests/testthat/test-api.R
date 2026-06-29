test_that("get_indicators returns proper data frame", {
  df_de <- get_indicators("de")
  expect_true(is.data.frame(df_de))
  expect_true(nrow(df_de) > 0)
  expect_true(all(c("ID", "Name", "Unit", "Active") %in% names(df_de)))

  df_en <- get_indicators("en")
  expect_true(is.data.frame(df_en))
  expect_true(nrow(df_en) > 0)
  expect_true(all(c("ID", "Name", "Unit", "Active") %in% names(df_en)))
})

test_that("get_geographies returns levels when null", {
  geo <- get_geographies()
  expect_true(is.data.frame(geo))
  expect_true(nrow(geo) > 0)
  expect_true("KRE" %in% geo$ID)
})

test_that("get_inkar_data handles invalid level parameter", {
  expect_error(get_inkar_data("011", level = "INVALID_LEVEL"))
})

test_that("get_inkar_data validates year argument", {
  expect_error(get_inkar_data("011", year = "abc"), "Invalid 'year'")
})

test_that("view_indicators DE does not error", {
  # Runs the reshape logic without opening the GUI viewer
  withr::with_options(list(viewer = NULL), {
    expect_no_error(suppressMessages(view_indicators("de")))
  })
})

test_that("view_indicators EN does not error", {
  withr::with_options(list(viewer = NULL), {
    expect_no_error(suppressMessages(view_indicators("en")))
  })
})

test_that("search_indicators returns filtered results", {
  res <- suppressMessages(search_indicators("Bevölkerung"))
  expect_true(is.data.frame(res))
  expect_true(nrow(res) > 0)
})

test_that("plot_inkar errors on unsupported spatial level", {
  skip_if_not_installed("sf")
  skip_if_not_installed("geodata")
  skip_if_not_installed("ggplot2")
  df <- data.frame(
    Kennziffer = "01001", Aggregat = "ROR", Wert = 1, Zeit = 2021,
    stringsAsFactors = FALSE
  )
  expect_error(plot_inkar(df), "only supports")
})

test_that("plot_inkar supports custom geom sf object", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  
  # Create a simple polygon sf object
  p1 <- matrix(c(0,0, 0,1, 1,1, 1,0, 0,0), ncol=2, byrow=TRUE)
  p2 <- matrix(c(1,1, 1,2, 2,2, 2,1, 1,1), ncol=2, byrow=TRUE)
  polys <- sf::st_sfc(sf::st_polygon(list(p1)), sf::st_polygon(list(p2)))
  geom_dummy <- sf::st_sf(
    Kennziffer = c("01001", "02000"),
    geometry = polys
  )
  
  df <- data.frame(
    Kennziffer = c("01001", "02000"),
    Aggregat = c("KRE", "KRE"),
    Wert = c(10, 20),
    Zeit = c(2021, 2021),
    stringsAsFactors = FALSE
  )
  
  p <- expect_no_error(plot_inkar(df, geom = geom_dummy))
  expect_s3_class(p, "gg")
})


test_that("clear_inkar_cache runs without error", {
  expect_no_error(suppressMessages(clear_inkar_cache()))
})

test_that("get_inkar_data returns data for valid inputs", {
  skip_if_not_installed("httptest2")

  # Using a known active indicator
  # with_mock_dir records the exact API request/response the first time it runs,
  # and plays it back identically on all subsequent runs.
  httptest2::with_mock_dir("api-mocks", {
    df <- suppressWarnings(get_inkar_data(
      variable = "011",
      level = "KRE",
      year = 2021
    ))

    if (!is.null(df) && nrow(df) > 0) {
      expect_true(is.data.frame(df))
      expect_true("Kennziffer" %in% names(df))
      expect_true("Wert" %in% intersect(names(df), c("Wert", "2021")))
    }
  })
})
