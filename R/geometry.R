# GISCO LAU release whose territorial status matches INKAR (Gebietsstand
# 31.12.2023: 10,978 Gemeinden). Update together with data-raw/gemeinde_gvb.R.
inkar_gisco_year <- 2024L

gisco_attribution <- "Administrative boundaries: \u00a9 EuroGeographics"

#' Get Administrative Boundaries Matching INKAR
#'
#' Returns `sf` polygons for a spatial level with a `Kennziffer` column in the
#' same format as [get_inkar_data()], so the two can be joined directly.
#' Boundaries come from Eurostat GISCO LAU (Gemeinden), whose territorial status
#' matches INKAR. Higher levels are built by dissolving the Gemeinden;
#' Gemeindeverbaende use the Destatis Gemeindeverzeichnis to assign each
#' Gemeinde to its Verband.
#'
#' The GISCO file (about 43 MB) is downloaded once and the German polygons are
#' cached. In interactive sessions the cache persists across sessions; see
#' [clear_inkar_cache()].
#'
#' Source: (c) EuroGeographics for the administrative boundaries. Please keep
#' this attribution in published maps.
#'
#' @param level Character. One of `"BND"`, `"BLD"`, `"KRE"`, `"GVB"`, `"GEM"`.
#'   `"ROR"` is not available.
#' @param refresh Logical. If `TRUE`, ignore the cache and download again.
#' @return An `sf` object (CRS ETRS89-LAEA, EPSG:3035) with columns
#'   `Kennziffer` and `geometry` (plus `name` for `"GEM"`).
#' @export
#' @examples
#' # Downloads about 43 MB on first use, so it only runs interactively
#' if (interactive() && requireNamespace("sf", quietly = TRUE)) {
#'   kreise <- get_inkar_geometry("KRE")
#' }
get_inkar_geometry <- function(level = c("KRE", "BND", "BLD", "GVB", "GEM"), refresh = FALSE) {
    if (!requireNamespace("sf", quietly = TRUE)) {
        stop("Package 'sf' is required. Install it with: install.packages('sf')", call. = FALSE)
    }
    level <- match.arg(toupper(level), c("KRE", "BND", "BLD", "GVB", "GEM"))

    cache_file <- file.path(get_cache_dir(), paste0("geom_", level, "_", inkar_gisco_year, ".rds"))
    if (!refresh && file.exists(cache_file)) {
        cached <- tryCatch(readRDS(cache_file), error = function(e) NULL)
        if (!is.null(cached)) return(cached)
    }

    gem <- if (level == "GEM") load_gisco_gemeinden(refresh) else get_inkar_geometry("GEM", refresh)
    if (level == "GEM") return(gem)

    key <- switch(level,
        BND = rep("1", nrow(gem)),
        BLD = substr(gem$Kennziffer, 1, 2),
        KRE = substr(gem$Kennziffer, 1, 5),
        GVB = gvb_lookup$GVB[match(gem$Kennziffer, gvb_lookup$AGS)]
    )
    if (anyNA(key)) {
        warning(sum(is.na(key)), " Gemeinden could not be assigned to a ", level, " region.", call. = FALSE)
    }
    out <- stats::aggregate(gem["Kennziffer"], by = list(code = key), FUN = length)
    out <- out[, "code"]
    names(out)[names(out) == "code"] <- "Kennziffer"
    out <- sf::st_set_geometry(out, "geometry")

    tryCatch(saveRDS(out, cache_file), error = function(e) NULL)
    out
}

#' Download GISCO LAU and keep the German Gemeinden
#' @noRd
load_gisco_gemeinden <- function(refresh = FALSE) {
    cache_file <- file.path(get_cache_dir(), paste0("geom_GEM_", inkar_gisco_year, ".rds"))
    if (!refresh && file.exists(cache_file)) {
        cached <- tryCatch(readRDS(cache_file), error = function(e) NULL)
        if (!is.null(cached)) return(cached)
    }

    layer <- sprintf("LAU_RG_01M_%d_3035", inkar_gisco_year)
    url <- sprintf("https://gisco-services.ec.europa.eu/distribution/v2/lau/shp/%s.shp.zip", layer)
    zip_file <- tempfile(fileext = ".zip")
    on.exit(unlink(zip_file), add = TRUE)

    message("Downloading GISCO LAU ", inkar_gisco_year, " boundaries (about 43 MB, once)...")
    resp <- tryCatch(
        httr2::request(url) |>
            httr2::req_user_agent("inkaR R Package") |>
            httr2::req_timeout(600) |>
            httr2::req_retry(max_tries = 3) |>
            httr2::req_perform(path = zip_file),
        error = function(e) {
            stop("Could not download boundaries from Eurostat GISCO: ", conditionMessage(e), call. = FALSE)
        }
    )

    gem <- sf::st_read(
        paste0("/vsizip/", zip_file, "/", layer, ".shp"),
        query = sprintf("SELECT GISCO_ID, LAU_NAME FROM \"%s\" WHERE CNTR_CODE = 'DE'", layer),
        quiet = TRUE
    )
    gem <- sf::st_set_geometry(gem, "geometry")
    gem$Kennziffer <- sub("^DE_", "", gem$GISCO_ID)
    gem$name <- gem$LAU_NAME
    gem <- gem[, c("Kennziffer", "name")]

    if (nrow(gem) == 0) stop("The GISCO file contains no German Gemeinden.", call. = FALSE)
    tryCatch(saveRDS(gem, cache_file), error = function(e) NULL)
    gem
}
