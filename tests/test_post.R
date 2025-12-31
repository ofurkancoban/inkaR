check_post <- function(url) {
    message("Testing POST: ", url)
    tryCatch(
        {
            # Send empty body which should trigger error 500 or 400 if endpoint exists
            resp <- httr2::request(url) |>
                httr2::req_method("POST") |>
                httr2::req_body_json(list(foo = "bar")) |>
                httr2::req_perform()
            message("Status: ", httr2::resp_status(resp))
        },
        error = function(e) {
            # If it's a 500/400 error, it means endpoint exists!
            if (grepl("500|400|405", e$message)) {
                message("Endpoint EXISTS (got expected error): ", e$message)
            } else {
                message("Error: ", e$message)
            }
        }
    )
}

check_post("https://www.inkar.de/Table/GetDataTable")
check_post("https://www.inkar.de/API/Table/GetDataTable")
