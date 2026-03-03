#' List Available Indicators
#'
#' Returns a data frame of available indicators with bilingual support.
#'
#' @param lang Language code: "de" (German) or "en" (English).
#' @return A tibble containing indicator IDs, names, and descriptions.
#' @export
get_indicators <- function(lang = c("de", "en")) {
    lang <- match.arg(lang)

    # Load internal dataset
    # Note: This assumes 'indicators' dataset exists in the package
    if (!exists("indicators", envir = asNamespace("inkaR"))) {
        # If not yet built, return empty or try to load raw file if testing
        warning(
            "Indicators dataset not loaded. Ensure package is installed or data loaded."
        )
        return(tibble::tibble())
    }

    df <- inkaR::indicators

    # Filter only Active indicators to prevent dead/archived indicators from appearing
    if ("Active" %in% names(df)) {
        df <- df |> dplyr::filter(.data$Active == TRUE)
    }

    # Filter/Select columns based on language if the dataset supports it
    # We assume the dataset has ID, Name_DE, Name_EN, etc.

    if (lang == "en") {
        if ("Name_EN" %in% names(df)) {
            df <- df |>
                dplyr::mutate(
                    Name = .data$Name_EN,
                    Unit = .data$Unit_EN,
                    Anmerkungen = if ("Anmerkungen_EN" %in% names(df)) {
                        .data$Anmerkungen_EN
                    } else {
                        .data$Anmerkungen
                    },
                    `Statistische Grundlagen` = if (
                        "Stat_Grund_EN" %in% names(df)
                    ) {
                        .data$Stat_Grund_EN
                    } else {
                        .data$`Statistische Grundlagen`
                    }
                ) |>
                dplyr::select(
                    -dplyr::matches("_DE$"),
                    -dplyr::all_of(c(
                        "Name_EN",
                        "Unit_EN",
                        "Anmerkungen_EN",
                        "Stat_Grund_EN"
                    ))
                )
        }
    } else {
        # Default is DE
        if ("Name_DE" %in% names(df)) {
            df <- df |>
                dplyr::mutate(Name = .data$Name_DE, Unit = .data$Unit_DE) |>
                dplyr::select(
                    -dplyr::matches("_EN$"),
                    -dplyr::all_of(c("Name_DE", "Unit_DE"))
                )
        }
    }

    return(df)
}

#' View Indicators in RStudio Viewer
#'
#' Opens the available indicators in the RStudio data viewer for easy filtering and searching.
#'
#' @param lang Language code: "de" (German) or "en" (English).
#' @return Invokes `View()` on the data frame.
#' @export
view_indicators <- function(lang = c("de", "en")) {
    # Allow unquoted input (e.g. view_indicators(de))
    lang_arg <- substitute(lang)
    if (is.symbol(lang_arg)) {
        lang <- as.character(lang_arg)
    }

    lang <- match.arg(lang)
    df <- get_indicators(lang = lang)

    # Custom formatting for German view based on user request
    if (lang == "de") {
        # Map standardized names back to user-preferred (Excel-like) headers
        df <- df |>
            dplyr::arrange(dplyr::desc(Active)) |> # Sort Active first
            dplyr::select(
                M_ID,
                Aktiv = Active, # NEW: Active Status
                Kurzname = Name, # Name is Name_DE
                Name = Description_DE, # Description_DE is the longer Name
                Gemeinden,
                Kreise,
                Algorithmus,
                "K\\u00FCrzel" = ID,
                Anmerkungen,
                `Statistische Grundlagen` = dplyr::any_of(c(
                    "Statistische Grundlagen",
                    "Statistische_Grundlagen"
                ))
            )
    } else if (lang == "en") {
        # Custom formatting for English view to match DE structure
        df <- df |>
            dplyr::arrange(dplyr::desc(Active)) |> # Sort Active first
            dplyr::select(
                M_ID,
                Active, # NEW: Active Status
                `Short Name` = Name, # Mapping Name (EN) here
                Name = Name, # Reusing Name as we don't have separate Desc yet
                Communities = Gemeinden,
                Circles = Kreise,
                Algorithm = Algorithmus,
                Abbreviation = ID,
                Notes = Anmerkungen, # Now contains English content if available
                `Statistical Principles` = `Statistische Grundlagen` # Now contains English content if available
            )
    }

    # Note: RStudio's View() always displays a row index/name column on the left.
    # We cannot remove it, so we stick to the default numeric index (1, 2, 3...)
    # to avoid confusion or duplication of the M_ID column.

    # Check if running in RStudio to use the viewer
    if (requireNamespace("utils", quietly = TRUE)) {
        utils::View(
            df,
            title = paste0("INKAR Indicators (", toupper(lang), ")")
        )
    } else {
        print(df)
        message("RStudio Viewer not available. Printing data frame instead.")
    }
}

#' Search Indicators and Print Results
#'
#' Search for indicators by keyword. Prints a formatted table and invisibly
#' returns the matches so you can copy the ID for use in `inkaR()`.
#'
#' @param pattern Text to search in names and descriptions.
#' @param lang Language to search in ("de" or "en").
#' @return A filtered tibble of indicators (invisibly).
#' @export
search_indicators <- function(pattern, lang = c("de", "en")) {
    lang <- match.arg(lang)
    df <- get_indicators(lang = lang)

    desc_col <- if ("Description_DE" %in% names(df)) "Description_DE" else NULL

    hits <- df |>
        dplyr::filter(
            grepl(pattern, .data$Name, ignore.case = TRUE) |
                grepl(pattern, .data$ID, ignore.case = TRUE) |
                (if (!is.null(desc_col)) {
                    grepl(pattern, .data[[desc_col]], ignore.case = TRUE)
                } else {
                    FALSE
                })
        )

    if (nrow(hits) == 0) {
        message("No indicators found for: '", pattern, "'")
        return(invisible(tibble::tibble()))
    }

    # Print formatted table
    cat(sprintf("\n  %d indicator(s) matching '%s':\n", nrow(hits), pattern))
    cat(strrep("-", 70), "\n")
    cat(sprintf("  %-15s %-50s\n", "ID", "Name"))
    cat(strrep("-", 70), "\n")
    for (i in seq_len(min(30, nrow(hits)))) {
        cat(sprintf(
            "  %-15s %-50s\n",
            hits$ID[i],
            substr(hits$Name[i], 1, 50)
        ))
    }
    if (nrow(hits) > 30) {
        cat("  ...", nrow(hits) - 30, "more. Use a more specific term.\n")
    }
    cat(strrep("-", 70), "\n")
    cat("  Use: inkaR(\"<ID>\") to download the data\n\n")

    invisible(hits)
}

#' Interactively Select an Indicator
#'
#' Opens a GUI selection list (e.g., in RStudio) to browse and pick an indicator.
#' For code-based workflows, use `inkaR("name")` or `search_indicators()` instead.
#'
#' @param pattern Optional character. Pre-filter the list by a keyword or regex.
#'   If `NULL` (default), the full indicator list is shown.
#' @param lang Language for names: `"de"` (default) or `"en"`.
#' @return Character. The selected indicator ID, or NULL if cancelled.
#' @export
select_indicator <- function(pattern = NULL, lang = c("de", "en")) {
    lang <- match.arg(lang)
    df <- get_indicators(lang)

    # Pre-filter by pattern if supplied (backward compatibility)
    if (!is.null(pattern) && nchar(trimws(pattern)) > 0) {
        search_in <- paste(
            df$Name,
            df$ID,
            if ("Description_DE" %in% names(df)) df$Description_DE else ""
        )
        df <- df[grepl(pattern, search_in, ignore.case = TRUE), ]
        if (nrow(df) == 0) {
            message("No indicators found for: '", pattern, "'")
            return(invisible(NULL))
        }
    }

    options <- paste0(
        df$Name,
        " (ID: ",
        df$ID,
        ")"
    )

    choice <- utils::select.list(options, title = "INKAR - Select Indicator")

    if (choice == "") {
        message("Selection cancelled.")
        return(invisible(NULL))
    }

    m <- regexec("\\(ID: ([^)]+)\\)", choice)
    id <- regmatches(choice, m)[[1]][2]
    message("Selected Indicator ID: ", id)
    return(id)
}

#' Interactively Select a Spatial Level
#'
#' Provides an interactive console menu to choose an INKAR spatial level.
#' If a variable ID is provided, it probes the live API to find which levels
#' actually have data for that indicator.
#'
#' @param variable Optional character. The indicator ID to probe available levels.
#' @return Character. The selected level ID, e.g., "KRE".
#' @export
select_level <- function(variable = NULL) {
    # Full mapping of level names -> API level IDs
    level_map <- list(
        "Bund" = "BND",
        "L\u00E4nder" = "BLD",
        "Raumordnungsregionen" = "ROR",
        "Kreise" = "KRE",
        "Gemeindeverb\u00E4nde" = "GVB",
        "Gemeinden" = "GEM"
    )

    available_levels <- names(level_map)

    # If variable is provided, probe the LIVE API for real availability
    if (!is.null(variable)) {
        # Resolve textual ID (e.g. "bev_korr") to numeric M_ID for API calls
        api_variable <- variable
        if (exists("indicators", envir = asNamespace("inkaR"))) {
            inds <- inkaR::indicators
            # Textual ID match -> get M_ID
            if (variable %in% inds$ID) {
                m <- inds$M_ID[inds$ID == variable]
                if (length(m) > 0 && !is.na(m[1])) {
                    api_variable <- as.character(m[1])
                }
            }
        }

        message("Checking available spatial levels for this indicator (concurrently)...")
        # Build list of requests for all spatial levels
        reqs <- lapply(names(level_map), function(lv_name) {
            lv_id <- level_map[[lv_name]]
            # Always bypass local cache for this check to ensure fresh availability
            cache_key <- paste("times", api_variable, lv_id, sep = "_")
            # We don't remove from persistent cache, we just force a fresh request
            
            body <- list(
                IndicatorCollection = list(list(Gruppe = api_variable)),
                TimeCollection = "",
                SpaceCollection = list(list(level = lv_id))
            )
            
            inkar_request("Wizard/GetM%C3%B6glich") |>
                httr2::req_method("POST") |>
                httr2::req_body_json(body) |>
                # Add metadata to the returning req object to identify it later
                httr2::req_user_agent(lv_name)
        })
        
        # We need a named list or parallel structure. httr2::req_perform_parallel doesn't preserve custom attributes well, 
        # but output is in the same order as input reqs.
        resps <- httr2::req_perform_parallel(
            reqs, 
            on_error = "continue" # Keep going if one fails
        )
        
        valid_keys <- c()
        for (i in seq_along(resps)) {
            resp <- resps[[i]]
            lv_name <- names(level_map)[i]
            
            if (!inherits(resp, "error") && !httr2::resp_is_error(resp)) {
                content <- httr2::resp_body_json(resp, simplifyVector = TRUE)
                # Handle double-json quirk
                if (is.character(content) && length(content) == 1) {
                    try({
                        content <- jsonlite::fromJSON(content, simplifyVector = TRUE)
                    }, silent = TRUE)
                }
                
                moglich_key <- "M\u00f6glich"
                if (!is.null(content[[moglich_key]])) {
                    times <- dplyr::bind_rows(content[[moglich_key]])
                    if (is.data.frame(times) && nrow(times) > 0) {
                        valid_keys <- c(valid_keys, lv_name)
                        # Optionally cache this successful fresh response
                        lv_id <- level_map[[lv_name]]
                        cache_key <- paste("times", api_variable, lv_id, sep = "_")
                        set_cache(cache_key, times)
                    }
                }
            }
        }

        if (length(valid_keys) == 0) {
            message("No spatial levels found for this indicator via API.")
            return(invisible(NULL))
        }

        available_levels <- valid_keys
    }

    # Construct options based on verified levels
    options <- paste0(
        available_levels,
        " (",
        unlist(level_map[available_levels]),
        ")"
    )
    ids <- unlist(level_map[available_levels])

    message("Select a spatial level (0 to cancel):")
    choice <- utils::select.list(options, title = "INKAR Spatial Level")

    if (choice == "") {
        message("Selection cancelled.")
        return(invisible(NULL))
    }

    idx <- match(choice, options)
    level_id <- ids[idx]

    message("Selected Level: ", level_id)
    return(level_id)
}
