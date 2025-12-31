#' Internal request helper
#'
#' @param path API endpoint path
#' @param query List of query parameters
#' @param base_url Base URL for INKAR API
#' @return httr2 request object
#' @noRd
inkar_request <- function(
    path,
    query = list(),
    base_url = "https://www.inkar.de"
) {
    # Base URL confirmed as inkar.de
    # Endpoint usually /Table/GetDataTable for POST

    if (!requireNamespace("httr2", quietly = TRUE)) {
        stop("Package 'httr2' is required for this function.")
    }

    req <- httr2::request(base_url) |>
        httr2::req_url_path_append(path) |>
        httr2::req_url_query(!!!query) |>
        httr2::req_user_agent(
            "inkaR R Package"
        ) |>
        httr2::req_retry(max_tries = 3) |>
        # Do not throw R error on HTTP error immediately, so we can parse body
        httr2::req_error(is_error = function(resp) FALSE)

    return(req)
}

#' Perform request and parse JSON
#'
#' @param req httr2 request
#' @return Parsed list or data frame
#' @noRd
perform_request <- function(req) {
    resp <- httr2::req_perform(req)

    if (httr2::resp_is_error(resp)) {
        # Try to print body for debugging
        try(
            {
                msg <- httr2::resp_body_string(resp)
                message(
                    "API Error (",
                    httr2::resp_status(resp),
                    "): ",
                    substr(msg, 1, 500)
                )
            },
            silent = TRUE
        )
        stop("API request failed with status ", httr2::resp_status(resp))
    }

    if (httr2::resp_content_type(resp) != "application/json") {
        warning("Response is not JSON.")
    }

    # Parse initial JSON
    content <- httr2::resp_body_json(resp, simplifyVector = TRUE)

    # Handle Double-JSON encoding (INKAR quirk)
    # Sometimes the API returns a JSON string wrapped in quotes "{\"Foo\":...}"
    if (is.character(content) && length(content) == 1) {
        # Try parsing the inner string
        tryCatch(
            {
                inner_content <- jsonlite::fromJSON(
                    content,
                    simplifyVector = TRUE
                )
                return(inner_content)
            },
            error = function(e) {
                # If not valid JSON, return original content
                return(content)
            }
        )
    }

    return(content)
}
