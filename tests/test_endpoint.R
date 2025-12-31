try_url <- function(url) {
    message("Testing: ", url)
    tryCatch(
        {
            resp <- httr2::request(url) |>
                httr2::req_method("OPTIONS") |> # or HEAD
                httr2::req_timeout(5) |>
                httr2::req_perform()
            message("Status: ", httr2::resp_status(resp))
            return(TRUE)
        },
        error = function(e) {
            message("Error: ", e$message)
            return(FALSE)
        }
    )
}

# Potential endpoints
urls <- c(
    "https://www.inkar.de/API/GetValues",
    "https://www.inkar.de/Json/GetValues",
    "https://www.inkar.de/services/GetValues"
)

for (u in urls) {
    try_url(u)
}
