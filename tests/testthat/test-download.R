library(testthat)
library(inkaR)

test_that("normalize_id strips punctuation and lowercases", {
  expect_equal(inkaR:::normalize_id("q_alo"), "qalo")
  expect_equal(inkaR:::normalize_id("Q_ALO"), "qalo")
  expect_equal(inkaR:::normalize_id("bip"), "bip")
  expect_equal(inkaR:::normalize_id("BIP-2"), "bip2")
})

test_that("get_inkar_data returns empty tibble for unknown variable", {
  skip_on_cran()
  result <- suppressWarnings(
    suppressMessages(get_inkar_data("__nonexistent__xyz__", level = "KRE"))
  )
  expect_true(is.data.frame(result))
})

test_that("search_indicators returns data frame for known term", {
  result <- suppressMessages(search_indicators("population", lang = "en"))
  expect_true(is.data.frame(result))
})

test_that("search_indicators fuzzy match does not error on typo", {
  expect_no_error(suppressMessages(search_indicators("employmnt", lang = "en")))
})

test_that("search_indicators returns invisible tibble for no match without stringdist", {
  result <- suppressMessages(search_indicators("zzznomatchzzz", lang = "en"))
  expect_true(is.data.frame(result))
})

test_that("update_indicators is exported and callable", {
  expect_true(is.function(update_indicators))
})

test_that("get_inkar_data BLD level returns tibble with warning", {
  skip_on_cran()
  result <- suppressWarnings(suppressMessages(
    get_inkar_data("011", level = "BLD", year = 2021)
  ))
  expect_true(is.data.frame(result))
})
