# --- Sync indicators metadata with the live INKAR wizard ---
# Run after process_metadata.R (which builds names and translations).
#
# INKAR identifies an indicator in two ways:
#   * the wizard "Gruppe": the key the data API expects (stored as M_ID), and
#   * the official indicator number from the "Uebersicht der Indikatoren"
#     spreadsheet, returned by the API as IndID (stored as IndID).
# They differ for almost all indicators (e.g. Arbeitslosenquote: Gruppe 12,
# official 1101). The bundled metadata used to hold both as separate rows: an
# "active" row keyed by Gruppe without short ID or documentation, and an
# "inactive" row keyed by the official number with short ID (q_alo), source
# and English texts. Requests with the official number return no data, so the
# short IDs did not work. This script merges each pair into one row keyed by
# Gruppe, adds IndID and sets Theme to the wizard's topic area (Bereich).
#
# German indicators only: areas with an EU spatial reference (NUTS-2) are
# skipped because the package supports German levels only.

load("data/indicators.rda")
old <- indicators

base_url <- "https://www.inkar.de"
wizard <- '{"IndicatorCollection":[],"TimeCollection":[],"SpaceCollection":[],"Title":"","pageorder":1,"currentpage":0,"modified":false}'

# The API answers with JSON encoded as a JSON string
parse_body <- function(resp) {
  jsonlite::fromJSON(jsonlite::fromJSON(httr2::resp_body_string(resp)))
}
post <- function(path, body) {
  httr2::request(base_url) |>
    httr2::req_url_path_append(path) |>
    httr2::req_body_raw(body, type = "application/json; charset=utf-8") |>
    httr2::req_retry(max_tries = 3)
}

# 1. Topic areas and their indicators
areas <- parse_body(httr2::req_perform(post("Wizard/GetBereiche", wizard)))$Bereiche
area_reqs <- lapply(areas$ID, function(id) {
  post("Wizard/GetIndikatorenZuBereich",
       sprintf('{"bereichsID": "%s", "wiz": "%s"}', id, gsub('"', "\u00a7", wizard, fixed = TRUE)))
})
area_resps <- httr2::req_perform_parallel(area_reqs, max_active = 6, progress = FALSE)
api <- do.call(rbind, lapply(area_resps, function(r) {
  parse_body(r)$Indikatoren[, c("Gruppe", "Bereich", "EU", "KurznamePlus")]
}))
api <- api[api$EU == "", ]
api$Theme <- areas$Bereich[match(api$Bereich, areas$ID)]
api$M_ID <- as.numeric(api$Gruppe)
api <- api[!duplicated(api$M_ID), ]

# 2. Official number and documentation per Gruppe
info_reqs <- lapply(api$Gruppe, function(g) {
  httr2::request(paste0(base_url, "/Wizard/GetIndikatorInfo/", g)) |> httr2::req_retry(max_tries = 3)
})
info_resps <- httr2::req_perform_parallel(info_reqs, max_active = 6, progress = FALSE)
info <- do.call(rbind, lapply(info_resps, function(r) {
  x <- parse_body(r)$Info
  data.frame(IndID = as.numeric(x$ID[1]), Kurzname = x$Kurzname[1], Name = x$Name[1],
             Algorithmus = x$Algorithmus[1], Quelle = x$Quelle[1], stringsAsFactors = FALSE)
}))
api <- cbind(api, info)
stopifnot(
  !anyNA(api$IndID),
  # An official number that is also another indicator's Gruppe would merge
  # the wrong rows below
  !any(api$IndID != api$M_ID & api$IndID %in% api$M_ID)
)

# 2b. Years available at Kreise and Gemeinden level, straight from the API
year_range <- function(level) {
  reqs <- lapply(api$Gruppe, function(g) {
    httr2::request(paste0(base_url, "/Wizard/GetM%C3%B6glich")) |>
      httr2::req_body_json(list(IndicatorCollection = list(list(Gruppe = g)), TimeCollection = "",
                                SpaceCollection = list(list(level = level)))) |>
      httr2::req_retry(max_tries = 3)
  })
  resps <- httr2::req_perform_parallel(reqs, max_active = 6, progress = FALSE)
  vapply(resps, function(r) {
    times <- parse_body(r)[["M\u00f6glich"]]
    years <- if (is.data.frame(times)) suppressWarnings(as.integer(times$Zeit)) else integer()
    years <- years[!is.na(years)]
    if (length(years) == 0) NA_character_ else paste0(min(years), "-", max(years))
  }, character(1))
}
api$years_KRE <- year_range("KRE")
api$years_GEM <- year_range("GEM")

# 3. Merge each Gruppe with its documentation row
fill <- function(primary, fallback) ifelse(is.na(primary) | primary == "", fallback, primary)
doc_cols <- c("Description_DE", "Algorithmus", "Anmerkungen", "Statistische Grundlagen",
              "Gemeinden", "Kreise", "Anmerkungen_EN", "Stat_Grund_EN", "Unit_DE", "Unit_EN",
              "Description_EN")

rows <- lapply(seq_len(nrow(api)), function(i) {
  a <- api[i, ]
  by_gruppe <- old[old$M_ID == a$M_ID, ]
  by_official <- if (a$IndID != a$M_ID) old[old$M_ID == a$IndID, ] else old[0, ]

  if (nrow(by_gruppe) == 0 && nrow(by_official) == 0) {
    stop("No local metadata for Gruppe ", a$M_ID, " (", a$KurznamePlus, "); add it to process_metadata.R first.")
  }
  # Names and translations: keep the row users already know (Gruppe row)
  row <- if (nrow(by_gruppe) > 0) by_gruppe[1, ] else by_official[1, ]
  doc <- if (nrow(by_official) > 0) by_official[1, ] else row

  # The official short ID (e.g. "q_alo") replaces a numeric placeholder ID
  if (nrow(by_official) > 0 && grepl("^[0-9]+$", row$ID)) row$ID <- doc$ID
  for (col in doc_cols) row[[col]] <- fill(row[[col]], doc[[col]])

  # Short official name ("Arbeitslosenquote"); Name_DE often holds the long one
  row$Short_DE <- a$KurznamePlus
  # Availability as reported by the API; replaces the "Active"/"Unknown"
  # placeholders and stale ranges from the overview spreadsheet
  row$Kreise <- a$years_KRE
  row$Gemeinden <- a$years_GEM
  row$Description_DE <- fill(row$Description_DE, a$Name)
  row$Algorithmus <- fill(row$Algorithmus, a$Algorithmus)
  row$`Statistische Grundlagen` <- fill(row$`Statistische Grundlagen`, a$Quelle)
  row$M_ID <- a$M_ID
  row$IndID <- a$IndID
  row$Theme <- a$Theme
  row$Active <- TRUE
  row
})
current <- do.call(rbind, rows)

# Rows whose number is neither a current Gruppe nor a merged official number
# are discontinued; keep them so old scripts get a clear "inactive" warning.
retired <- old[!old$M_ID %in% c(api$M_ID, api$IndID), ]
retired$IndID <- retired$M_ID
retired$Short_DE <- retired$Name_DE
retired$Theme <- NA_character_
retired$Active <- FALSE

indicators <- rbind(current, retired)

# Units that the long name states unambiguously ("... in %", "... je km\u00b2").
# Indicators without such an ending (mostly absolute counts) keep NA.
unit_rules <- list(
  list(pattern = " in %$", de = "%", en = "%"),
  list(pattern = " in Minuten$", de = "Minuten", en = "minutes"),
  list(pattern = " je 10\\.000 Einwohner$", de = "je 10.000 Einwohner", en = "per 10,000 inhabitants"),
  list(pattern = " je km\u00b2$", de = "je km\u00b2", en = "per km\u00b2"),
  list(pattern = " in \u20ac$", de = "Euro", en = "euros")
)
for (rule in unit_rules) {
  hit <- is.na(indicators$Unit_DE) & grepl(rule$pattern, indicators$Description_DE)
  indicators$Unit_DE[hit] <- rule$de
  indicators$Unit_EN[hit] <- rule$en
}
indicators <- indicators[order(!indicators$Active, indicators$M_ID), ]
indicators <- tibble::as_tibble(indicators)

# 4. Checks
stopifnot(
  !anyDuplicated(indicators$ID),
  !anyDuplicated(indicators$M_ID),
  sum(indicators$Active) == nrow(api),
  # every active indicator has data at Kreise or Gemeinden level
  all(!is.na(indicators$Kreise[indicators$Active]) | !is.na(indicators$Gemeinden[indicators$Active])),
  all(api$M_ID %in% indicators$M_ID[indicators$Active]),
  # every short ID from the old metadata is still resolvable
  all(old$ID[!grepl("^[0-9]+$", old$ID)] %in% indicators$ID)
)
message(sprintf(
  "Indicators: %d active (%d with official short ID), %d discontinued; %d topic areas.",
  sum(indicators$Active), sum(indicators$Active & !grepl("^[0-9]+$", indicators$ID)),
  sum(!indicators$Active), length(unique(indicators$Theme[indicators$Active]))
))

usethis::use_data(indicators, overwrite = TRUE)
