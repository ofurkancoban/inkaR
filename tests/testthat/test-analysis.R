test_that("compare_regions filters by partial name (EN)", {
  df <- data.frame(
    region_id   = c("01001", "02000", "11000"),
    region_name = c("Flensburg", "Hamburg", "Berlin"),
    value       = c(100, 200, 300),
    stringsAsFactors = FALSE
  )
  result <- compare_regions(df, c("Berlin", "Hamburg"))
  expect_equal(nrow(result), 2)
  expect_true(all(result$region_name %in% c("Berlin", "Hamburg")))
  
  # Singular alias test
  result_singular <- compare_region(df, "Berlin")
  expect_equal(nrow(result_singular), 1)
  expect_equal(result_singular$region_name, "Berlin")
})


test_that("compare_regions filters by partial name (DE)", {
  df <- data.frame(
    Kennziffer  = c("01001", "02000", "11000"),
    Raumeinheit = c("Flensburg", "Hamburg", "Berlin"),
    Wert        = c(100, 200, 300),
    stringsAsFactors = FALSE
  )
  result <- compare_regions(df, "Berlin")
  expect_equal(nrow(result), 1)
})

test_that("compare_regions exact=TRUE requires exact match", {
  df <- data.frame(
    region_name = c("Berlin", "Berlin Mitte"),
    value       = c(1, 2),
    stringsAsFactors = FALSE
  )
  result <- compare_regions(df, "Berlin", exact = TRUE)
  expect_equal(nrow(result), 1)
  expect_equal(result$region_name, "Berlin")
})

test_that("compare_regions returns empty tibble when no match found", {
  df <- data.frame(
    region_name = c("Hamburg"),
    value       = c(1),
    stringsAsFactors = FALSE
  )
  result <- suppressMessages(compare_regions(df, "München"))
  expect_equal(nrow(result), 0)
})

test_that("get_themes returns character vector", {
  themes <- get_themes()
  expect_true(is.character(themes))
})

test_that("search_indicators theme parameter filters results", {
  skip_if(!exists("indicators", envir = asNamespace("inkaR")))
  themes <- get_themes()
  if (length(themes) == 0) skip("No themes in metadata")
  # Should return fewer rows than unfiltered search
  res_all    <- search_indicators("a", lang = "en")
  res_themed <- search_indicators("a", lang = "en", theme = themes[1])
  expect_true(nrow(res_themed) <= nrow(res_all))
})

test_that("inkar_trends returns NULL when no matching regions", {
  skip_if_not_installed("ggplot2")
  df <- data.frame(
    region_name    = c("Hamburg", "Hamburg"),
    year           = c(2020L, 2021L),
    value          = c(100, 110),
    indicator_name = c("GDP", "GDP"),
    stringsAsFactors = FALSE
  )
  result <- inkar_trends(df, regions = "NoSuchRegion")
  expect_null(result)
})

test_that("inkar_trends returns ggplot for valid data", {
  skip_if_not_installed("ggplot2")
  df <- data.frame(
    region_name    = c("Hamburg", "Hamburg", "Berlin", "Berlin"),
    year           = c(2020L, 2021L, 2020L, 2021L),
    value          = c(100, 110, 90, 95),
    indicator_name = rep("GDP", 4),
    unit           = rep("EUR", 4),
    stringsAsFactors = FALSE
  )
  p <- inkar_trends(df)
  expect_s3_class(p, "gg")
})

test_that("compare_districts filters by name and id (EN)", {
  df <- data.frame(
    region_id   = c("01001", "02000", "11000"),
    region_name = c("Flensburg", "Hamburg", "Berlin"),
    value       = c(100, 200, 300),
    stringsAsFactors = FALSE
  )
  result_name <- compare_districts(df, "Hamburg")
  expect_equal(nrow(result_name), 1)
  expect_equal(result_name$region_name, "Hamburg")

  result_id <- compare_districts(df, "11000")
  expect_equal(nrow(result_id), 1)
  expect_equal(result_id$region_name, "Berlin")
})

test_that("compare_district (singular alias) works identically", {
  df <- data.frame(
    region_id   = c("01001", "02000"),
    region_name = c("Flensburg", "Hamburg"),
    value       = c(100, 200),
    stringsAsFactors = FALSE
  )
  result <- compare_district(df, "02000", exact = TRUE)
  expect_equal(nrow(result), 1)
  expect_equal(result$region_name, "Hamburg")
})

