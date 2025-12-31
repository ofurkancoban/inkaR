#' Download Data from INKAR
#'
#' Retrieves statistical data for a given variable and spatial level.
#'
#' @param variable Character. The indicator ID (Shortname), e.g., "001".
#' @param level Character. Spatial level, e.g., "Kreise".
#' @param year Integer/Character. Year to retrieve. Defaults to latest available if NULL.
#' @param level Character. Spatial level code (e.g., "KRE" for Kreise, "GEM" for Gemeinden).
#' @param year Integer/Character. Specific year. Experimental support.
#' @return A tibble containing the data.
#' @export
#' @examples
#' \dontrun{
#'   data <- get_inkar_data("001", "Kreise", 2017)
#' }
#' Get Available Time References (Internal)
#'
#' @param variable Indicator ID
#' @param level Spatial level ID
#' @return A data frame of available times
#' @noRd
get_time_references <- function(variable, level) {
  body <- list(
    IndicatorCollection = list(list(Gruppe = variable)),
    TimeCollection = "",
    SpaceCollection = list(list(level = level))
  )

  # Note: Endpoint is /Wizard/GetM\\u00F6glich (encoded)
  # using unencoded string here, relying on httr2 to handle or pre-encoded
  req <- inkar_request("Wizard/GetM%C3%B6glich") |>
    httr2::req_method("POST") |>
    httr2::req_body_json(body)

  resp <- perform_request(req)

  # Response structure: list(M\u00F6glich = list(...))
  if (!is.null(resp[["M\\u00F6glich"]])) {
    # It returns a list of lists. Bind them.
    times <- dplyr::bind_rows(resp[["M\\u00F6glich"]])
    return(times)
  }

  return(NULL)
}

#' Download Data from INKAR
#'
#' Retrieves statistical data for a given variable and spatial level.
#' Automatically handles time reference lookup.
#'
#' @param variable Character. The indicator ID (Shortname), e.g., "011".
#' @param level Character. Spatial level code (e.g., "KRE" for Kreise).
#' @param year Integer/Character vector. Specific year (e.g., 2021) or range (e.g., 2010:2020). If NULL, fetches all available years.
#' @param lang Character. "de" (default) for German column names, "en" for English.
#' @param format Character. "long" (default) for tidy format, "wide" for years as columns.
#' @param csv Logical. If TRUE, saves the data to a CSV file in the current directory.
#' @return A tibble containing the data.
#' @export
get_inkar_data <- function(
  variable,
  level = "KRE",
  year = NULL,
  lang = "de",
  format = "long",
  csv = FALSE
) {
  # Step 1: Smart ID Resolution (Handle M_ID input)
  # If variable matches a numeric M_ID, convert it to the API textual ID
  if (exists("indicators", envir = asNamespace("inkaR"))) {
    inds <- inkaR::indicators

    # Case 1: Variable is an alphanumeric ID (e.g., "bip", "xbev")
    # The API generally rejects these and needs the numeric M_ID.
    if (variable %in% inds$ID) {
      match_row <- inds[inds$ID == variable, ]
      if (nrow(match_row) >= 1) {
        # Use M_ID from the match
        m_id_val <- match_row$M_ID[1]
        message(
          "Using unique M_ID '",
          m_id_val,
          "' for Indicator Code '",
          variable,
          "'"
        )
        variable <- as.character(m_id_val)
      }
    } else {
      # Case 2: Variable might be the numeric M_ID itself (e.g., "1203", "11")
      var_num <- suppressWarnings(as.numeric(variable))
      if (!is.na(var_num) && var_num %in% inds$M_ID) {
        # It IS a valid M_ID. Use it directly.
        # Note: API accepts "11", "011", "1" etc.
        message("Using M_ID '", var_num, "' directly.")
        variable <- as.character(var_num)
      }
    }
  }

  # Step 1.5: Get available time metadata (Zeitbezug)
  # This is required to construct the TimeCollection for the main request
  times_df <- get_time_references(variable, level)

  if (is.null(times_df) || nrow(times_df) == 0) {
    warning("No data found for this variable/level combination.")
    return(tibble::tibble())
  }

  # Step 2: Filter times if year is specified
  if (!is.null(year)) {
    times_df <- times_df |> dplyr::filter(Zeit %in% as.character(year))
    if (nrow(times_df) == 0) {
      warning("Specified year not available.")
      return(tibble::tibble())
    }
  }

  # Step 3: Construct TimeCollection
  # API expects a list of objects with specific keys that differ from the Wizard output
  # Mappings based on successful existing clients (bonn package):
  # Gruppe -> group
  # IndID -> indicator
  # RaumID -> level
  # ZeitID -> time

  time_collection <- times_df |>
    dplyr::select(
      group = Gruppe,
      indicator = IndID,
      level = RaumID,
      time = ZeitID
    )

  # Step 4: Main Data Request
  body <- list(
    IndicatorCollection = list(list(Gruppe = variable)),
    TimeCollection = time_collection, # httr2/jsonlite will serialize DF as array of objects
    SpaceCollection = list(list(level = level)),
    pageorder = "1"
  )

  req <- inkar_request("Table/GetDataTable") |>
    httr2::req_method("POST") |>
    httr2::req_body_json(body)

  resp <- perform_request(req)

  # Step 5: Parse with language support
  df <- parse_inkar_json(resp, lang = lang)

  # Step 6: Enrich with Metadata (Names, Aggregates)
  # keys: region_id/Kennziffer, indicator_id/IndikatorID

  has_id_en <- "region_id" %in% names(df)
  has_id_de <- "Kennziffer" %in% names(df)

  if (has_id_en || has_id_de) {
    # --- A. Join Region Names (Raumeinheit) ---
    regions <- get_geographies(geography = level)
    if (nrow(regions) > 0) {
      reg_key <- if (has_id_en) "region_id" else "Kennziffer"

      # Prepare regions DF
      # API returns region_id, region_name.
      # If DE, we want "Raumeinheit" as the name col
      name_target <- if (lang == "de") "Raumeinheit" else "region_name"

      regions <- regions |>
        dplyr::rename(!!reg_key := region_id, !!name_target := region_name)

      # Join if not already present
      if (!name_target %in% names(df)) {
        df <- df |> dplyr::left_join(regions, by = reg_key)
      }
    }

    # --- B. Join Indicator Name & Aggregate Level (from times_df) ---
    if (exists("times_df") && nrow(times_df) > 0) {
      # Columns to join
      ind_id_col <- if (has_id_en) "indicator_id" else "IndikatorID"

      if (ind_id_col %in% names(df)) {
        # Extract unique metadata
        meta <- times_df |>
          dplyr::select(ID = IndID, Name = Indikator, Agg = Raum) |>
          dplyr::distinct(ID, .keep_all = TRUE)

        # Rename for join target
        target_ind_name <- if (lang == "de") "Indikator" else "indicator_name"
        target_agg_name <- if (lang == "de") "Aggregat" else "level_name"

        meta <- meta |>
          dplyr::rename(
            !!ind_id_col := ID,
            !!target_ind_name := Name,
            !!target_agg_name := Agg
          )

        df <- df |> dplyr::left_join(meta, by = ind_id_col)

        # [NEW] English Translation Override
        # If language is EN, try to fetch English names from package metadata
        # because the API/times_df often returns German names by default.
        if (lang == "en") {
          # Check if get_indicators can find the name
          # We use tryCatch to avoid failure if data is missing
          try(
            {
              en_inds <- get_indicators(lang = "en")
              if (nrow(en_inds) > 0) {
                # We need to join by ID.
                # The package data has 'ID' and 'Name' (which is now English)

                # Prepare lookup
                # Match primarily on M_ID (Numeric API ID)
                # Prepare lookup
                # Match primarily on M_ID (Numeric API ID)
                lookup <- en_inds |>
                  dplyr::select(M_ID, Name_EN = Name) |>
                  dplyr::mutate(join_id = as.character(M_ID))

                # Update the indicator_name column
                df <- df |>
                  dplyr::mutate(indicator_id = as.character(indicator_id)) |>
                  dplyr::left_join(
                    lookup,
                    by = c("indicator_id" = "join_id")
                  ) |>
                  dplyr::mutate(
                    # Override if English name is available
                    indicator_name = dplyr::if_else(
                      !is.na(Name_EN),
                      Name_EN,
                      indicator_name
                    )
                  ) |>
                  dplyr::select(-Name_EN)
                # Note: we kept M_ID from lookup (it is .y usually or M_ID).
                # Actually M_ID is in lookup.
                # Let's ensure M_ID column is clean.
                if ("M_ID" %in% names(df)) {
                  # Ensure it's not duplicated or messy
                }
              }
            },
            silent = TRUE
          )
        }
      }
    }

    # Ensure M_ID exists for DE as well if possible, or use indicator_id as M_ID fallback
    if (!"M_ID" %in% names(df) && "indicator_id" %in% names(df)) {
      df <- df |> dplyr::mutate(M_ID = indicator_id)
    }

    # --- C. Final Selection & Ordering ---
    # Desired: Kennziffer, Raumeinheit, Aggregat, M_ID, Indikator, [Years... or Year], [Values... or Value]

    # Standard Long Format Columns
    final_cols_de <- c(
      "Kennziffer",
      "Raumeinheit",
      "Aggregat",
      "M_ID",
      "Indikator",
      "Zeit",
      "Wert"
    )
    final_cols_en <- c(
      "region_id",
      "region_name",
      "level_name",
      "M_ID",
      "indicator_name",
      "year",
      "value"
    )

    target_cols <- if (lang == "de") final_cols_de else final_cols_en

    # Capture indicator name for filename (before potentially removing it in wide format)
    # The name is in 'Indikator' or 'indicator_name' depending on lang
    col_ind_name_extract <- if (lang == "de") "Indikator" else "indicator_name"
    raw_ind_name <- "Unknown"
    if (col_ind_name_extract %in% names(df) && nrow(df) > 0) {
      raw_ind_name <- as.character(df[[col_ind_name_extract]][1])
    }

    # Reshape if format is 'wide'
    if (format == "wide") {
      # Determine time and value columns based on language
      col_time <- if (lang == "de") "Zeit" else "year"
      col_value <- if (lang == "de") "Wert" else "value"
      col_ind_name <- if (lang == "de") "Indikator" else "indicator_name"

      # Check if columns exist (safety)
      if (all(c(col_time, col_value) %in% names(df))) {
        # If Indicator Name exists, combine it with Time (Requested: Name_Year)
        if (col_ind_name %in% names(df)) {
          df <- df |>
            dplyr::mutate(
              !!col_time := paste(
                .data[[col_ind_name]],
                .data[[col_time]],
                sep = "_"
              )
            ) |>
            dplyr::select(-dplyr::all_of(col_ind_name))
        }

        df <- df |>
          tidyr::pivot_wider(
            names_from = dplyr::all_of(col_time),
            values_from = dplyr::all_of(col_value)
          )

        # Update target cols for selection: Keep IDs + Everything else (Dynamic Cols)
        # ID cols to keep at start
        # Now including M_ID
        id_cols_de <- c("Kennziffer", "Raumeinheit", "Aggregat", "M_ID")
        id_cols_en <- c("region_id", "region_name", "level_name", "M_ID")

        target_cols <- if (lang == "de") id_cols_de else id_cols_en
      }
    }

    df <- df |>
      dplyr::select(dplyr::any_of(target_cols), dplyr::everything())
  }

  # CSV Export if requested
  if (csv) {
    # Generate a sensible filename: inkar_{ID}_{LEVEL}_{NAME}.csv
    # Sanitize variable/name for filename
    safe_id <- gsub("[^a-zA-Z0-9]", "", variable)

    # Sanitize indicator name (spaces to underscores, remove special chars)
    safe_name <- gsub("[^a-zA-Z0-9]", "_", raw_ind_name)
    # Collapse multiple underscores
    safe_name <- gsub("_+", "_", safe_name)
    # Truncate if too long (filesystem limits)
    if (nchar(safe_name) > 50) {
      safe_name <- substr(safe_name, 1, 50)
    }

    filename <- paste0("inkar_", safe_id, "_", level, "_", safe_name, ".csv")

    utils::write.csv(df, filename, row.names = FALSE)
    message("Data saved to: ", filename)
  }

  return(df)
}

#' Get Available Geographies or Region List
#'
#' Retrieves a list of available spatial levels (if `geography` is NULL) or
#' a list of regions for a specific level (e.g., "KRE").
#'
#' @param geography Character. Spatial level code (e.g. "KRE"). If NULL, returns all levels.
#' @return A data frame with `ID` and `Name`.
#' @export
get_geographies <- function(geography = NULL) {
  if (is.null(geography)) {
    # Return hardcoded list for now as a named vector for easy lookup,
    # or implement the API call similar to bonn if needed.
    # For user convenience in arguments, we kept the vector, but if they want a DF:
    return(tibble::tibble(
      Name = c(
        "Bund",
        "L\\u00E4nder",
        "Kreise",
        "Gemeindeverb\\u00E4nde",
        "Gemeinden",
        "Raumordnungsregionen"
      ),
      ID = c("BND", "BLD", "KRE", "GVB", "GEM", "ROR")
    ))
  } else {
    # Fetch specific regions for a level
    # Endpoint: /Wizard/GetGebieteZumRaumbezug/{Level}

    # Ensure using the ID (e.g. KRE) not the full name
    req <- inkar_request(paste0("Wizard/GetGebieteZumRaumbezug/", geography))
    resp <- perform_request(req)

    # Response is normally a list of lists or DF inside a list
    if (is.list(resp) && length(resp) > 0) {
      # Often it's resp[[1]] or similar
      # parse_inkar_json can handle list of lists
      regions <- parse_inkar_json(resp, lang = "de") # Keep original German names first

      # Standardize output to ID and Name
      regions <- regions |>
        dplyr::select(
          region_id = dplyr::any_of(c("Schl\\u00FCssel", "Kennziffer", "ID")),
          region_name = dplyr::any_of(c(
            "Name",
            "Raumname",
            "Titel",
            "Raumeinheit"
          ))
        )

      return(regions)
    }
    return(tibble::tibble())
  }
}
