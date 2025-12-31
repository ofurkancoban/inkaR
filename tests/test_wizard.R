library(httr2)

url <- "https://www.inkar.de/Wizard/GetM%C3%B6glich"

body <- list(
    IndicatorCollection = list(list(Gruppe = "011")),
    TimeCollection = "",
    SpaceCollection = list(list(level = "KRE"))
)

message("Sending Wizard payload...")
req <- request(url) |>
    req_method("POST") |>
    req_body_json(body) |>
    req_error(is_error = function(resp) FALSE)

resp <- req_perform(req)

message("Status: ", resp_status(resp))
if (resp_status(resp) == 200) {
    print(head(resp_body_json(resp, simplifyVector = TRUE)))
} else {
    print(resp_body_string(resp))
}
