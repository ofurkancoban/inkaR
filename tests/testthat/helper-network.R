# Skip a live API test when www.inkar.de cannot be reached. The server omits
# its intermediate certificate, so this happens on systems without a complete
# CA bundle. The request bypasses the disk cache, which mock tests may fill.
skip_if_inkar_unreachable <- function() {
  resp <- tryCatch(
    suppressMessages(inkaR:::perform_request(
      inkaR:::inkar_request("Wizard/GetGebieteZumRaumbezug/KRE")
    )),
    error = function(e) NULL
  )
  testthat::skip_if(is.null(resp), "INKAR API not reachable")
}
