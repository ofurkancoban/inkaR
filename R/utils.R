#' Rename first matching column and remove others
#'
#'Helper to robustly rename *one* column from a set of candidates.
#'If multiple candidates exist, only the first is renamed, others are dropped.
#'
#' @param df Data frame
#' @param new_name New column name
#' @param candidates Vector of potential old column names
#' @noRd
rename_first_found <- function(df, new_name, candidates) {
    existing <- intersect(candidates, names(df))
    if (length(existing) > 0) {
        rename_vec <- setNames(existing[1], new_name)
        df <- dplyr::rename(df, !!!rename_vec)

        if (length(existing) > 1) {
            df <- dplyr::select(df, -dplyr::any_of(existing[-1]))
        }
    }
    df
}

#' Convert API list response to Tibble
#'
#' @param data Raw list data from JSON
#' @return A tibble
#' @noRd
parse_inkar_json <- function(data, lang = "de") {
    if (is.null(data)) {
        return(tibble::tibble())
    }

    if (is.data.frame(data)) {
        df <- tibble::as_tibble(data)
    } else {
        df <- tryCatch(
            {
                dplyr::bind_rows(data)
            },
            error = function(e) {
                message("Could not automatically bind rows: ", e$message)
                return(data)
            }
        )
    }

    # Cleaning
    if (is.data.frame(df)) {
        # Define target names based on language
        if (lang == "en") {
            col_region_id <- "region_id"
            col_region_name <- "region_name"
            col_level <- "level"
            col_year <- "year"
            col_value <- "value"
            col_indicator_id <- "indicator_id"
        } else {
            col_region_id <- "Kennziffer"
            col_region_name <- "Raumeinheit" # Changed from Raumname to match Excel/User expectation
            col_level <- "Raumbezug"
            col_year <- "Zeit"
            col_value <- "Wert"
            col_indicator_id <- "IndikatorID" # Renamed from Indikator to IndikatorID to avoid confusion with Name
        }

        df <- df |>
            rename_first_found(
                col_region_id,
                c("Schl\u00fcssel", "RaumID", "Raum")
            ) |>
            rename_first_found(
                col_region_name,
                c("Raumname", "Name", "Raumeinheit")
            ) |>
            rename_first_found(col_level, c("Raumbezug")) |>
            rename_first_found(col_year, c("ZeitID", "Zeit", "Jahr")) |>
            rename_first_found(col_value, c("Wert", "Indikatorwert")) |>
            rename_first_found(col_indicator_id, c("Indikator", "ID", "IndID"))

        # Convert types (columns exist now with target names)
        # We need to act on the *current* names which are variable
        # So we rename to standard internal names temporarily?
        # No, easier: just use the variables we defined above

        # Convert types (columns exist now with target names)
        # Check if columns exist before mutating to avoid errors on empty/malformed responses
        if (col_value %in% names(df)) {
            df <- df |>
                dplyr::mutate(
                    !!col_value := if (is.character(.data[[col_value]])) {
                        vals <- .data[[col_value]]
                        # Detect German format (comma as decimal, dot as thousands separator):
                        # presence of both "." and "," with "," appearing after "." is a strong signal
                        is_german <- any(grepl(",", vals, fixed = TRUE), na.rm = TRUE) &&
                            any(grepl("\\.", vals), na.rm = TRUE)
                        if (is_german) {
                            clean_str <- gsub("\\.", "", vals)
                            as.numeric(gsub(",", ".", clean_str))
                        } else if (any(grepl(",", vals, fixed = TRUE), na.rm = TRUE)) {
                            # Only comma present: treat as decimal separator
                            as.numeric(gsub(",", ".", vals))
                        } else {
                            as.numeric(vals)
                        }
                    } else {
                        as.numeric(.data[[col_value]])
                    }
                )
        }

        if (col_year %in% names(df)) {
            df <- df |>
                dplyr::mutate(!!col_year := as.integer(.data[[col_year]]))
        }
        df <- df |>
            dplyr::select(
                dplyr::any_of(c(
                    col_region_id,
                    col_region_name,
                    col_year,
                    col_value,
                    col_level,
                    col_indicator_id
                )),
                dplyr::everything()
            )
    }

    return(df)
}

#' Record Indicator Usage History
#' @param id The indicator ID used.
#' @noRd
record_usage <- function(id) {
    if (!interactive()) return(invisible(NULL))
    dir <- tryCatch(
        {
            tools::R_user_dir("inkaR", which = "config")
        },
        error = function(e) {
            file.path(tempdir(), "inkaR")
        }
    )
    if (!dir.exists(dir)) {
        tryCatch(
            {
                dir.create(dir, recursive = TRUE, showWarnings = FALSE)
            },
            error = function(e) NULL
        )
    }
    history_file <- file.path(dir, "usage_history.txt")
    history <- character()
    if (file.exists(history_file)) {
        history <- tryCatch(
            {
                readLines(history_file, warn = FALSE)
            },
            error = function(e) character()
        )
    }
    # Add to front, unique
    history <- unique(c(id, history))
    # Keep last 100
    if (length(history) > 100) history <- history[1:100]
    tryCatch(
        {
            writeLines(history, history_file)
        },
        error = function(e) NULL
    )
}

#' Get Frequent Indicators
#' @return Vector of frequent/recent indicator IDs.
#' @noRd
get_usage_history <- function() {
    if (!interactive()) return(character())
    dir <- tryCatch(
        {
            tools::R_user_dir("inkaR", which = "config")
        },
        error = function(e) {
            file.path(tempdir(), "inkaR")
        }
    )
    history_file <- file.path(dir, "usage_history.txt")
    if (file.exists(history_file)) {
        return(tryCatch(
            {
                readLines(history_file, warn = FALSE)
            },
            error = function(e) character()
        ))
    }
    return(character())
}

#' Bundled indicator metadata
#'
#' With `LazyData: true` the `indicators` dataset lives in the package's
#' lazy-data environment, not in the namespace itself, so
#' `exists("indicators", envir = asNamespace("inkaR"))` is always FALSE in a
#' regular `library(inkaR)` session. Use this helper instead.
#'
#' @return The `indicators` tibble, or NULL if it cannot be loaded.
#' @noRd
inkar_indicators <- function() {
  tryCatch(getExportedValue("inkaR", "indicators"), error = function(e) NULL)
}

#' Translate a user-facing level code to the code the INKAR API expects
#'
#' The API uses `BU` (Bund) and `BL` (Laender); `BND` and `BLD` return no data.
#' The package keeps `BND`/`BLD` as its public codes for backward compatibility.
#'
#' @param level Character vector of level codes.
#' @return Character vector of API level codes.
#' @noRd
to_api_level <- function(level) {
  api_codes <- c(BND = "BU", BLD = "BL")
  out <- as.character(level)
  hit <- out %in% names(api_codes)
  out[hit] <- api_codes[out[hit]]
  out
}

#' Escape regular expression metacharacters
#' @noRd
escape_regex <- function(x) {
  gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", x)
}

#' Case-insensitive literal substring match
#'
#' User input such as "Frankfurt (Oder)" or "Bevoelkerung (mit" contains regex
#' metacharacters, so patterns are matched literally.
#'
#' @param x Character vector to search in.
#' @param patterns Character vector; a value matches if it contains any of them.
#' @return Logical vector the length of `x`.
#' @noRd
text_match <- function(x, patterns) {
  patterns <- patterns[!is.na(patterns)]
  if (length(patterns) == 0) return(rep(FALSE, length(x)))
  hits <- grepl(paste(escape_regex(patterns), collapse = "|"), x, ignore.case = TRUE)
  hits & !is.na(x)
}
