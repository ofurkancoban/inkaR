library(httr2)

url <- "https://www.inkar.de/Table/GetDataTable"

# Hypothesis 2: use 'xbev' and IndID=2 (from Excel M_ID)
# Note: 'xbev' is population. M_ID is 2.
body <- list(
  IndicatorCollection = list(list(Gruppe = "xbev")),
  TimeCollection = list(list(
    Gruppe = "xbev",
    IndID = 2,
    RaumID = "KRE",
    ZeitID = 2021
  )),
  SpaceCollection = list(list(level = "KRE")),
  pageorder = "1"
)

message("Sending payload...")
req <- request(url) |>
  req_method("POST") |>
  req_body_json(body) |>
  req_error(is_error = function(resp) FALSE)

resp <- req_perform(req)

message("Status: ", resp_status(resp))
safe_body <- tryCatch(resp_body_string(resp), error = function(e) "")

if (safe_body != "") {
  print(substring(safe_body, 1, 500))
} else {
  message("Empty body")
}
