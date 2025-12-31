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

#' Search Indicators
#'
#' Search for indicators by name, description, or ID.
#'
#' @param pattern Regex pattern to search for.
#' @param lang Language to search in ("de" or "en").
#' @return A filtered tibble of indicators.
#' @export
search_indicators <- function(pattern, lang = c("de", "en")) {
    lang <- match.arg(lang)
    df <- get_indicators(lang = lang)

    # Determine explanation/description column based on language
    # Note: Description might only be available in DE for now based on dataset
    desc_col <- if ("Description_DE" %in% names(df)) "Description_DE" else NULL

    df |>
        dplyr::filter(
            grepl(pattern, .data$Name, ignore.case = TRUE) |
                grepl(pattern, .data$ID, ignore.case = TRUE) |
                (if (!is.null(desc_col)) {
                    grepl(pattern, .data[[desc_col]], ignore.case = TRUE)
                } else {
                    FALSE
                })
        )
}
