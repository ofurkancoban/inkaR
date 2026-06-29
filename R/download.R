# Normalize an indicator ID for fuzzy matching (strip punctuation, lowercase)
normalize_id <- function(x) tolower(gsub("[^[:alnum:]]", "", x))

# Internal: Get Available Time References
# Checks persistent disk cache first; fetches from API if not cached.
get_time_references <- function(variable, level) {
  cache_key <- paste("times", variable, level, sep = "_")

  cached_times <- get_cache(cache_key)
  if (!is.null(cached_times)) {
    return(cached_times)
  }

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

  # Response key is actual Unicode string "M\u00f6glich" (Möglich)
  moglich_key <- "M\u00f6glich"
  if (!is.null(resp[[moglich_key]])) {
    times <- dplyr::bind_rows(resp[[moglich_key]])
    set_cache(cache_key, times)
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
#' @param csv Logical. If TRUE, saves the data to a CSV file in the directory specified by `export_dir`.
#' @param export_dir Character. Directory to save the CSV file if `csv = TRUE`. If `NULL` (default), it saves to the current working directory (`"."`).
#' @return A tibble containing the data.
#' @export
get_inkar_data <- function(
  variable,
  level = "KRE",
  year = NULL,
  lang = c("de", "en"),
  format = c("long", "wide"),
  csv = FALSE,
  export_dir = NULL
) {
  # Input Validation
  valid_levels <- c("BND", "BLD", "ROR", "KRE", "GVB", "GEM")
  level <- match.arg(toupper(level), valid_levels)
  lang <- match.arg(lang)
  format <- match.arg(format)

  # B5: Validate year argument early ("latest" is allowed, numerics required otherwise)
  if (!is.null(year) && !identical(year, "latest")) {
    year_num <- suppressWarnings(as.numeric(year))
    if (any(is.na(year_num))) {
      stop(
        "Invalid 'year' argument: must be numeric (e.g. 2021 or 2010:2020) or \"latest\", got: ",
        paste(year[is.na(year_num)], collapse = ", ")
      )
    }
  }

  # Multi-Indicator Support: If 'variable' is a vector, loop and merge
  if (length(variable) > 1) {
    cli::cli_alert_info("Downloading multiple indicators: {.val {variable}}")
    n_vars <- length(variable)
    cli::cli_progress_bar("Downloading indicators", total = n_vars)
    results <- vector("list", n_vars)
    for (i in seq_along(variable)) {
      results[[i]] <- get_inkar_data(
        variable = variable[[i]],
        level = level,
        year = year,
        lang = lang,
        format = "long",
        csv = FALSE,
        export_dir = export_dir
      )
      cli::cli_progress_update()
    }
    cli::cli_progress_done()

    # Filter out NULLs (failed selections)
    results <- results[!sapply(results, is.null)]
    if (length(results) == 0) return(invisible(NULL))

    # Check if they are empty
    results <- results[sapply(results, nrow) > 0]
    if (length(results) == 0) {
      warning("No data found for any of the selected indicators.")
      return(tibble::tibble())
    }

    final_df <- dplyr::bind_rows(results)

    # HORIZONTAL JOIN (Wide) for multiple indicators
    # Pivot: region x year per row, one column per indicator (named ind_<M_ID>)
    if (format == "wide") {
      cli::cli_alert_info("Pivoting to wide format (one column per indicator)...")
      col_id   <- if (lang == "de") "Kennziffer" else "region_id"
      col_name <- if (lang == "de") "Raumeinheit" else "region_name"
      col_lev  <- if (lang == "de") "Aggregat" else "level_name"
      col_time <- if (lang == "de") "Zeit" else "year"
      col_val  <- if (lang == "de") "Wert" else "value"

      # Normalize level_name so "Kreise" and "KRE" don't create duplicate rows
      if (col_lev %in% names(final_df)) {
        final_df[[col_lev]] <- level
      }

      # Build a short, readable column label per M_ID from the indicator name
      col_ind <- if (lang == "de") "Indikator" else "indicator_name"
      make_col_label <- function(name, mid) {
        name <- if (is.na(name) || !nzchar(trimws(name))) "" else trimws(name)
        label <- gsub("[^A-Za-z0-9]+", "_", name)
        label <- gsub("_+$|^_+", "", label)
        label <- substr(label, 1, 40)
        if (!nzchar(label)) label <- paste0("ind_", mid)
        label
      }
      ind_names <- if (col_ind %in% names(final_df)) {
        final_df[[col_ind]]
      } else {
        paste0("ind_", final_df$M_ID)
      }
      # Fall back to local metadata name when API name is missing
      local_name_col <- if (lang == "en") "Name_EN" else "Name_DE"
      if (exists("indicators", envir = asNamespace("inkaR"))) {
        local_meta <- inkaR::indicators[, c("M_ID", local_name_col)]
        names(local_meta) <- c("M_ID", "local_name")
        local_meta$M_ID <- as.character(local_meta$M_ID)
        name_df <- data.frame(M_ID = as.character(final_df$M_ID), ind_name = ind_names,
                              stringsAsFactors = FALSE) |>
          dplyr::distinct() |>
          dplyr::left_join(local_meta, by = "M_ID") |>
          dplyr::mutate(
            ind_name = dplyr::if_else(
              is.na(.data$ind_name) | !nzchar(trimws(.data$ind_name)),
              .data$local_name, .data$ind_name
            )
          )
        ind_names_final <- name_df$ind_name
        mid_vals        <- name_df$M_ID  # already character
      } else {
        ind_names_final <- ind_names
        mid_vals        <- as.character(final_df$M_ID)
      }
      label_map <- data.frame(
        M_ID      = as.character(mid_vals),
        ind_name  = ind_names_final,
        stringsAsFactors = FALSE
      ) |>
        dplyr::distinct() |>
        dplyr::mutate(col_label = mapply(make_col_label, .data$ind_name, .data$M_ID)) |>
        dplyr::select(M_ID, col_label)

      final_df <- final_df |>
        dplyr::mutate(M_ID = as.character(.data$M_ID)) |>
        dplyr::left_join(label_map, by = "M_ID") |>
        dplyr::select(
          dplyr::any_of(c(col_id, col_name, col_lev, col_time)),
          col_label,
          dplyr::all_of(col_val)
        ) |>
        tidyr::pivot_wider(
          names_from  = col_label,
          values_from = dplyr::all_of(col_val)
        )
    }
    
    # Save to CSV if requested for the combined set
    if (csv) {
      dir <- if (is.null(export_dir)) "." else export_dir
      prefix <- if (format == "wide") "inkar_wide_" else "inkar_long_"
      filename <- paste0(prefix, format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      path <- file.path(dir, filename)
      utils::write.csv(final_df, path, row.names = FALSE)
      cli::cli_alert_success("Combined data saved to {.path {path}}")
    }
    
    return(final_df)
  }

  # Step 1: Smart ID Resolution (handles exact ID, numeric M_ID, normalized, name search, fuzzy)
  if (exists("indicators", envir = asNamespace("inkaR"))) {
    inds <- inkaR::indicators
    resolved <- FALSE

    # Case 1: Exact textual ID (e.g., "bip", "xbev")
    if (!resolved && variable %in% inds$ID) {
      match_row <- inds[inds$ID == variable, ]
      m_id_val  <- match_row$M_ID[1]
      if (isFALSE(match_row$Active[1])) {
        cli::cli_warn("Indicator {.val {variable}} (M_ID={m_id_val}) is marked inactive and may not return data.")
      }
      message("Using M_ID '", m_id_val, "' for Indicator '", variable, "'")
      variable <- as.character(m_id_val)
      resolved <- TRUE
    }

    # Case 2: Numeric M_ID (e.g., "11", "011", 1203)
    if (!resolved) {
      var_num <- suppressWarnings(as.numeric(variable))
      if (!is.na(var_num) && var_num %in% inds$M_ID) {
        message("Using M_ID '", var_num, "' directly.")
        variable <- as.character(var_num)
        resolved <- TRUE
      }
    }

    # Case 3: Normalized ID match (e.g., "q_alo" -> "qalo", "Q_ALO" -> "qalo")
    if (!resolved) {
      norm_var  <- normalize_id(variable)
      norm_ids  <- normalize_id(inds$ID)
      norm_hits <- which(norm_ids == norm_var)
      if (length(norm_hits) == 1) {
        match_row <- inds[norm_hits, ]
        m_id_val  <- match_row$M_ID[1]
        if (isFALSE(match_row$Active[1])) {
          cli::cli_warn("Indicator {.val {variable}} (M_ID={m_id_val}) is marked inactive and may not return data.")
        }
        message("Normalized match: '", inds$ID[norm_hits], "' (M_ID=", m_id_val, ")")
        variable <- as.character(m_id_val)
        resolved <- TRUE
      } else if (length(norm_hits) > 1) {
        message(length(norm_hits), " normalized matches for '", variable, "'. Please use an exact ID.")
        return(invisible(NULL))
      }
    }

    # Case 4: Natural-language / partial name search in Name_DE, Name_EN
    if (!resolved) {
      search_cols <- intersect(c("Name_DE", "Name_EN", "Description_DE"), names(inds))
      pattern     <- variable
      hits <- inds[
        Reduce("|", lapply(search_cols, function(col) {
          grepl(pattern, inds[[col]], ignore.case = TRUE)
        })),
      ]

      if (nrow(hits) == 1) {
        m_id_val <- hits$M_ID[1]
        if (isFALSE(hits$Active[1])) {
          cli::cli_warn("Indicator {.val {variable}} (M_ID={m_id_val}) is marked inactive and may not return data.")
        }
        message("Found indicator: '", hits$Name_DE[1], "' (M_ID=", m_id_val, ")")
        variable <- as.character(m_id_val)
        resolved <- TRUE
      } else if (nrow(hits) > 1) {
        message(
          nrow(hits), " indicators matched '", pattern,
          "'. Use a more specific name or an exact ID.\n",
          paste0("  ", hits$ID[seq_len(min(10, nrow(hits)))],
                 "  ", hits$Name_DE[seq_len(min(10, nrow(hits)))],
                 collapse = "\n"),
          if (nrow(hits) > 10) paste0("\n  ... and ", nrow(hits) - 10, " more.") else "",
          "\n\nTip: Use search_indicators(\"", pattern, "\") to explore."
        )
        return(invisible(NULL))
      } else {
        # Case 5: Fuzzy / Jaro-Winkler suggestion (stringdist in Suggests)
        if (requireNamespace("stringdist", quietly = TRUE)) {
          search_pool <- paste(inds$Name_DE, inds$Name_EN, sep = " ")
          dists       <- stringdist::stringdist(tolower(pattern), tolower(search_pool), method = "jw")
          best_idx    <- which(dists < 0.15)
          if (length(best_idx) > 0) {
            best_idx <- best_idx[order(dists[best_idx])]
            cli::cli_alert_info(
              "No match for {.val {pattern}}. Did you mean one of these?\n{paste0('  ', inds$ID[best_idx[seq_len(min(3, length(best_idx)))]], '  ', inds$Name_EN[best_idx[seq_len(min(3, length(best_idx)))]], collapse = '\n')}"
            )
          } else {
            message("No match found for '", pattern, "'. Passing to API as-is.")
          }
        } else {
          message("No match found for '", pattern, "'. Passing to API as-is.")
        }
      }
    }
  }

  # Step 1.5: Get available time metadata (Zeitbezug)
  # This is required to construct the TimeCollection for the main request
  times_df <- get_time_references(variable, level)

  if (is.null(times_df) || nrow(times_df) == 0) {
    if (level == "BLD") {
      cli::cli_alert_warning(
        "No data found for level {.val BLD}. Most INKAR indicators are only available at KRE or GEM level. Try {.code level = \"KRE\"}."
      )
    } else {
      warning("No data found for this variable/level combination.")
    }
    return(tibble::tibble())
  }

  # Step 2: Filter times if year is specified
  if (!is.null(year)) {
    if (identical(year, "latest")) {
      max_year <- max(suppressWarnings(as.integer(times_df$Zeit)), na.rm = TRUE)
      times_df <- times_df |> dplyr::filter(suppressWarnings(as.integer(.data$Zeit)) == max_year)
    } else {
      times_df <- times_df |> dplyr::filter(.data$Zeit %in% as.character(year))
    }
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
    if (!is.null(times_df) && nrow(times_df) > 0) {
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
              }
            },
            silent = TRUE
          )
        }
      }
    }

    # Ensure M_ID exists for DE as well if possible, or use indicator_id as M_ID fallback
    if (!"M_ID" %in% names(df) && "IndikatorID" %in% names(df)) {
      df <- df |> dplyr::mutate(M_ID = as.character(.data$IndikatorID))
    } else if (!"M_ID" %in% names(df) && "indicator_id" %in% names(df)) {
      df <- df |> dplyr::mutate(M_ID = as.character(.data$indicator_id))
    }

    # --- B2. Enrich with Description & Unit from local indicators metadata ---
    if (
      exists("indicators", envir = asNamespace("inkaR")) &&
        "M_ID" %in% names(df)
    ) {
      inds_local <- inkaR::indicators

      desc_target <- if (lang == "de") "Beschreibung" else "description"
      unit_target <- if (lang == "de") "Einheit" else "unit"

      if ("Description_DE" %in% names(inds_local)) {
        lookup <- inds_local |>
          dplyr::select(
            join_key = M_ID,
            desc_en = dplyr::any_of("Description_EN"),
            desc_de = dplyr::any_of("Description_DE"),
            unit_en = dplyr::any_of("Unit_EN"),
            unit_de = dplyr::any_of("Unit_DE")
          ) |>
          dplyr::mutate(
            !!desc_target := if (lang == "en" && "desc_en" %in% names(dplyr::pick(dplyr::everything()))) {
              dplyr::if_else(!is.na(.data$desc_en) & .data$desc_en != "",
                             .data$desc_en, .data$desc_de)
            } else {
              .data$desc_de
            }
          ) |>
          dplyr::select(-dplyr::any_of(c("desc_en", "desc_de"))) |>
          dplyr::mutate(
            join_key = as.character(.data$join_key),
            # EN: prefer Unit_EN, fall back to Unit_DE when EN is missing
            !!unit_target := if (lang == "en" && "unit_en" %in% names(dplyr::pick(dplyr::everything()))) {
              dplyr::if_else(!is.na(.data$unit_en), .data$unit_en, .data$unit_de)
            } else {
              .data$unit_de
            }
          ) |>
          dplyr::select(-dplyr::any_of(c("unit_en", "unit_de")))

        df <- df |>
          dplyr::mutate(join_key = as.character(.data$M_ID)) |>
          dplyr::left_join(lookup, by = "join_key") |>
          dplyr::select(-"join_key")
      }
    }

    # --- C0. Fallback: fill level column from 'level' parameter if still NA ---
    agg_col <- if (lang == "de") "Aggregat" else "level_name"
    if (agg_col %in% names(df) && all(is.na(df[[agg_col]]))) {
      df[[agg_col]] <- level
    }

    # --- C0b. Schema guarantee: ensure all expected output columns are present ---
    expected_cols <- if (lang == "en") {
      c("region_id", "region_name", "level_name", "M_ID",
        "indicator_name", "description", "unit", "year", "value")
    } else {
      c("Kennziffer", "Raumeinheit", "Aggregat", "M_ID",
        "Indikator", "Beschreibung", "Einheit", "Zeit", "Wert")
    }
    for (col in expected_cols) {
      if (!col %in% names(df)) df[[col]] <- NA
    }

    # --- C. Final Selection & Ordering ---
    final_cols_de <- c(
      "Kennziffer",
      "Raumeinheit",
      "Aggregat",
      "M_ID",
      "Indikator",
      "Beschreibung",
      "Einheit",
      "Zeit",
      "Wert"
    )
    final_cols_en <- c(
      "region_id",
      "region_name",
      "level_name",
      "M_ID",
      "indicator_name",
      "description",
      "unit",
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
      col_time <- if (lang == "de") "Zeit" else "year"
      col_value <- if (lang == "de") "Wert" else "value"

      # Static id cols for wide: identifiers + indicator metadata
      id_cols_de <- c(
        "Kennziffer",
        "Raumeinheit",
        "Aggregat",
        "M_ID",
        "Indikator",
        "Beschreibung",
        "Einheit"
      )
      id_cols_en <- c(
        "region_id",
        "region_name",
        "level_name",
        "M_ID",
        "indicator_name",
        "description",
        "unit"
      )
      id_cols <- if (lang == "de") id_cols_de else id_cols_en

      if (all(c(col_time, col_value) %in% names(df))) {
        # Pivot: use only year as column name (B2 fix)
        df <- df |>
          tidyr::pivot_wider(
            id_cols = dplyr::any_of(id_cols),
            names_from = dplyr::all_of(col_time),
            values_from = dplyr::all_of(col_value)
          )
        # Update target_cols so static id cols come first (B1 fix — Indikator preserved)
        target_cols <- id_cols
      }
    }

    df <- df |>
      dplyr::select(dplyr::any_of(target_cols), dplyr::everything()) |>
      dplyr::select(-dplyr::any_of(c(
        "Raumbezug", "IndikatorID",   # DE internal columns
        "level", "indicator_id"        # EN internal columns
      )))
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

    dir_to_use <- if (is.null(export_dir)) "." else export_dir
    filename <- file.path(dir_to_use, paste0("inkar_", safe_id, "_", level, "_", safe_name, ".csv"))

    utils::write.csv(df, filename, row.names = FALSE)
    message("Data saved to: ", filename)
  }

  # Print download summary
  if (nrow(df) > 0) {
    ind_name_col <- if (lang == "de") "Indikator" else "indicator_name"
    unit_col <- if (lang == "de") "Einheit" else "unit"
    year_col <- if (lang == "de") "Zeit" else "year"
    region_col <- if (lang == "de") "Kennziffer" else "region_id"

    ind_name <- if (ind_name_col %in% names(df)) {
      df[[ind_name_col]][1]
    } else {
      raw_ind_name
    }
    unit_val <- if (unit_col %in% names(df)) df[[unit_col]][1] else ""
    n_reg <- if (region_col %in% names(df)) {
      length(unique(df[[region_col]]))
    } else {
      nrow(df)
    }

    # B3 fix: detect years from Zeit col (long) or numeric column names (wide)
    if (year_col %in% names(df)) {
      years <- sort(unique(df[[year_col]]))
    } else {
      # Wide format: year cols are named with integers
      year_col_names <- suppressWarnings(as.integer(names(df)))
      years <- sort(year_col_names[!is.na(year_col_names)])
    }

    year_str <- if (length(years) > 1) {
      paste0(min(years), "\u2013", max(years))
    } else if (length(years) == 1) {
      as.character(years[1])
    } else {
      "?"
    }
    unit_str <- if (
      !is.null(unit_val) && !is.na(unit_val) && nchar(unit_val) > 0
    ) {
      paste0(" [", unit_val, "]")
    } else {
      ""
    }

    level_labels <- c(
      BND = "Bund",
      BLD = "L\u00e4nder",
      ROR = "Raumordnungsregionen",
      KRE = "Kreise",
      GVB = "Gemeindeverb\u00e4nde",
      GEM = "Gemeinden"
    )
    level_label <- if (level %in% names(level_labels)) {
      level_labels[[level]]
    } else {
      level
    }

    message(
      "\u2714 Downloaded: ",
      ind_name,
      unit_str,
      "\n",
      "  Level: ",
      level_label,
      " | Regions: ",
      n_reg,
      " | Year(s): ",
      year_str,
      " | Rows: ",
      nrow(df)
    )
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
        "L\u00e4nder",
        "Kreise",
        "Gemeindeverb\u00e4nde",
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
          region_id = dplyr::any_of(c("Schl\u00fcssel", "Kennziffer", "ID")),
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

#' Download Data from INKAR (Interactive Alias)
#'
#' A full-featured alias for [get_inkar_data()] with bilingual support and an
#' interactive wizard when called without arguments (in interactive sessions).
#' Call `inkaR("011")` to download directly, or `inkaR()` to open the wizard.
#'
#' For a simpler English-first shortcut, see [inkar()].
#'
#' @param variable Character. Indicator ID, shortname, or partial name.
#'   If `NULL` (default), opens an interactive selection menu (interactive sessions only).
#' @param level Character. Spatial level code (e.g., `"KRE"` for Kreise).
#'   If `NULL` and `variable` is also `NULL`, an interactive level menu is shown.
#' @param year Integer/Character vector or `"latest"`. Specific year (e.g. 2021) or range.
#' @param lang Character. `"de"` (default) for German column names, `"en"` for English.
#' @param ... Additional arguments passed to [get_inkar_data()], such as
#'   `format` or `csv`.
#' @return A tibble containing the downloaded data, or `NULL` if selection was cancelled.
#' @export
#' @examples
#' if (interactive()) {
#'   df <- inkaR()  # opens interactive menu
#' }
#'
#' \donttest{
#'   try(df <- inkaR("bip", level = "KRE", year = 2021))
#'   try(df <- inkaR("Bruttoinlandsprodukt", level = "KRE"))
#' }
inkaR <- function(variable = NULL, level = NULL, year = NULL, lang = c("de", "en"), ...) {
  lang <- match.arg(lang)
  
  if (is.null(variable)) {
    # Check if running interactively
    if (interactive()) {
      variable <- select_indicator(lang = lang)
      if (is.null(variable)) {
        return(invisible(NULL))
      }

      # Interactive level selection if not specified
      if (is.null(level)) {
        level <- select_level(variable)
        if (is.null(level)) return(invisible(NULL))
      }

      # Interactive year selection if not provided
      if (is.null(year)) {
        year <- select_years(variable, level)
        if (length(year) == 0) return(invisible(NULL))
      }
      
      return(get_inkar_data(variable = variable, level = level, year = year, lang = lang, ...))
    } else {
      stop("Argument 'variable' is missing, with no default.")
    }
  }

  if (is.null(level)) {
    level <- "KRE"
  }

  get_inkar_data(variable = variable, level = level, year = year, lang = lang, ...)
}

#' Download INKAR Data (English Shortcut)
#'
#' A convenience wrapper around [get_inkar_data()] with English output and
#' `year = "latest"` as defaults. Equivalent to calling
#' `get_inkar_data(variable, level, year = "latest", lang = "en")`.
#'
#' @param variable Character. Indicator ID, short name, or partial name.
#' @param level Character. Spatial level code (default `"KRE"`).
#' @param year Integer/Character vector or `"latest"` (default). Year(s) to download.
#' @param lang Character. Output language (default `"en"`).
#' @param ... Additional arguments passed to [get_inkar_data()].
#' @return A tibble with English column names.
#' @name inkar_shortcut
#' @aliases inkar
#' @export
#' @examples
#' \donttest{
#'   try(df <- inkar("011", level = "KRE"))
#'   try(df <- inkar("011", level = "KRE", year = 2019:2021))
#' }
inkar <- function(variable, level = "KRE", year = "latest", lang = "en", ...) {
  get_inkar_data(variable = variable, level = level, year = year, lang = lang, ...)
}

#' Filter Downloaded Data to Specific Regions
#'
#' Filters a data frame returned by [get_inkar_data()] to rows matching the
#' supplied region names (partial, case-insensitive match by default).
#'
#' @param data A data frame returned by [get_inkar_data()].
#' @param regions Character vector. Region names or partial patterns to keep.
#' @param exact Logical. If `TRUE`, require exact string match. Default `FALSE`.
#' @return A filtered tibble.
#' @export
#' @examples
#' \donttest{
#'   df <- try(get_inkar_data("011", level = "KRE", year = 2021, lang = "en"))
#'   if (is.data.frame(df)) compare_regions(df, c("Berlin", "Hamburg"))
#' }
compare_regions <- function(data, regions, exact = FALSE) {
  is_en <- "region_name" %in% names(data)
  name_col <- if (is_en) "region_name" else "Raumeinheit"
  if (!name_col %in% names(data)) {
    stop("Data must contain a 'region_name' (EN) or 'Raumeinheit' (DE) column.")
  }
  if (exact) {
    mask <- data[[name_col]] %in% regions
  } else {
    pattern <- paste(regions, collapse = "|")
    mask <- grepl(pattern, data[[name_col]], ignore.case = TRUE)
  }
  result <- data[mask, , drop = FALSE]
  if (nrow(result) == 0) {
    cli::cli_alert_warning("No regions matched: {.val {regions}}")
  }
  result
}

#' @rdname compare_regions
#' @export
compare_region <- compare_regions


#' Filter Downloaded Data to Specific Districts
#'
#' A specialized wrapper around [compare_regions()] to filter a data frame returned
#' by [get_inkar_data()] to rows matching specific district names or IDs (Kennziffer).
#'
#' @param data A data frame returned by [get_inkar_data()].
#' @param districts Character/Numeric vector. District names, IDs, or partial patterns to keep.
#' @param exact Logical. If `TRUE`, require exact string match. Default `FALSE`.
#' @return A filtered tibble.
#' @export
#' @examples
#' \donttest{
#'   df <- try(get_inkar_data("011", level = "KRE", year = 2021, lang = "en"))
#'   if (is.data.frame(df)) compare_districts(df, c("Berlin", "Hamburg"))
#' }
compare_districts <- function(data, districts, exact = FALSE) {
  is_en <- "region_name" %in% names(data)
  name_col <- if (is_en) "region_name" else "Raumeinheit"
  id_col <- if (is_en) "region_id" else "Kennziffer"
  
  if (!name_col %in% names(data) || !id_col %in% names(data)) {
    stop("Data must contain name and ID columns.")
  }
  
  districts_char <- as.character(districts)
  
  if (exact) {
    mask <- (data[[name_col]] %in% districts_char) | (data[[id_col]] %in% districts_char)
  } else {
    pattern <- paste(districts_char, collapse = "|")
    mask <- grepl(pattern, data[[name_col]], ignore.case = TRUE) | 
            grepl(pattern, data[[id_col]], ignore.case = TRUE)
  }
  
  result <- data[mask, , drop = FALSE]
  if (nrow(result) == 0) {
    cli::cli_alert_warning("No districts matched: {.val {districts}}")
  }
  result
}

#' @rdname compare_districts
#' @export
compare_district <- compare_districts


#' Plot Time Series Trends for INKAR Indicators
#'
#' Creates a `ggplot2` line chart showing how indicator values change over time
#' for selected regions. Input must be a long-format data frame from
#' [get_inkar_data()].
#'
#' @param data A long-format data frame from [get_inkar_data()].
#' @param regions Optional character vector. Region names (partial match) to
#'   include. If `NULL`, all regions are plotted.
#' @param title Optional character. Custom plot title. Defaults to indicator name.
#' @param mode Character. `"light"` (default) or `"dark"` theme.
#' @return A `ggplot2` object.
#' @export
#' @examples
#' \donttest{
#'   df <- try(get_inkar_data("011", level = "KRE", lang = "en"))
#'   if (is.data.frame(df)) {
#'     inkar_trends(df, regions = c("Berlin", "Hamburg", "München"))
#'   }
#' }
inkar_trends <- function(data, regions = NULL, title = NULL, mode = c("light", "dark")) {
  mode <- match.arg(mode)
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required. Install with: install.packages('ggplot2')")
  }
  is_en    <- "region_name" %in% names(data)
  name_col <- if (is_en) "region_name" else "Raumeinheit"
  year_col <- if (is_en) "year" else "Zeit"
  val_col  <- if (is_en) "value" else "Wert"
  ind_col  <- if (is_en) "indicator_name" else "Indikator"

  if (!all(c(name_col, year_col, val_col) %in% names(data))) {
    stop("Data must be in long format with region, year, and value columns.")
  }

  plot_data <- data
  if (!is.null(regions)) {
    pattern   <- paste(regions, collapse = "|")
    plot_data <- plot_data[grepl(pattern, plot_data[[name_col]], ignore.case = TRUE), , drop = FALSE]
    if (nrow(plot_data) == 0) {
      cli::cli_alert_warning("No matching regions found for: {.val {regions}}")
      return(invisible(NULL))
    }
  }

  plot_data[[year_col]] <- as.integer(as.character(plot_data[[year_col]]))
  plot_data[[val_col]]  <- suppressWarnings(as.numeric(plot_data[[val_col]]))

  ind_name   <- if (ind_col %in% names(plot_data) && nrow(plot_data) > 0) {
    unique(plot_data[[ind_col]])[1]
  } else {
    "Indicator"
  }
  plot_title <- if (!is.null(title)) title else ind_name

  unit_col <- if (is_en) "unit" else "Einheit"
  unit_str <- if (unit_col %in% names(plot_data) && !is.na(plot_data[[unit_col]][1])) {
    paste0(" (", plot_data[[unit_col]][1], ")")
  } else {
    ""
  }

  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x     = .data[[year_col]],
      y     = .data[[val_col]],
      color = .data[[name_col]],
      group = .data[[name_col]]
    )
  ) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::geom_point(size = 1.5) +
    theme_inkaR(mode = mode) +
    ggplot2::labs(
      title = plot_title,
      x     = if (is_en) "Year" else "Jahr",
      y     = paste0(if (is_en) "Value" else "Wert", unit_str),
      color = if (is_en) "Region" else "Region"
    )
}
