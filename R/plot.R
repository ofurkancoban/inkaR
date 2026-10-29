#' Premium ggplot2 theme for inkaR
#'
#' @param mode Character. "light" or "dark".
#' @param base_size Numeric. Base font size.
#' @export
theme_inkaR <- function(mode = c("light", "dark"), base_size = 11) {
    mode <- match.arg(mode)
    
    bg_color <- if (mode == "dark") "#1e1e1e" else "white"
    text_color <- if (mode == "dark") "#e0e0e0" else "#2c3e50"
    grid_color <- if (mode == "dark") "#333333" else "#f0f0f0"
    
    ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
        plot.background = ggplot2::element_rect(fill = bg_color, color = NA),
        panel.background = ggplot2::element_rect(fill = bg_color, color = NA),
        text = ggplot2::element_text(color = text_color),
        plot.title = ggplot2::element_text(face = "bold", size = 14, margin = ggplot2::margin(b = 10)),
        plot.subtitle = ggplot2::element_text(color = if (mode == "dark") "#aaaaaa" else "#7f8c8d", size = 10, margin = ggplot2::margin(b = 20)),
        panel.grid.major = ggplot2::element_line(color = grid_color),
        panel.grid.minor = ggplot2::element_blank(),
        legend.background = ggplot2::element_rect(fill = bg_color, color = NA),
        legend.text = ggplot2::element_text(color = text_color),
        axis.text = ggplot2::element_text(color = text_color)
    )
}

#' Plot INKAR Data on German Maps
#'
#' Automatically projects regional INKAR data onto administrative boundaries of Germany.
#' Supports Bund (BND), Bundeslaender (BLD), Kreise (KRE), Gemeindeverbaende (GVB) and
#' Gemeinden (GEM). Boundaries come from Eurostat GISCO with the same territorial status as
#' INKAR (see [get_inkar_geometry()]); they are downloaded once and cached. Alternatively, a
#' custom `sf` geometry can be provided, for example for Raumordnungsregionen (ROR).
#'
#' @param data A data frame returned by `get_inkar_data()`.
#' @param variable Character. For wide-format data with multiple indicators, specify which indicator column to plot.
#' @param year Integer/Character. If the data contains multiple years, specify which year to plot. If NULL and multiple years exist, the most recent year is plotted.
#' @param mode Character. "light" (default) or "dark" theme.
#' @param highlight Character vector. Region names (partial match) to highlight;
#'   all other regions are shown at reduced opacity.
#' @param breaks Character. Color scale break method: `"equal"` (default) or
#'   `"quantile"` for five quantile classes, each with its own color step.
#' @param title Character. Custom plot title. Defaults to the indicator name.
#' @param geom Optional `sf` object (spatial data frame) to use for plotting. If supplied,
#'   no boundaries are downloaded, and the data is merged directly with this object.
#' @return A `ggplot2` object displaying the mapped data.
#' @export
plot_inkar <- function(data, variable = NULL, year = NULL, mode = c("light", "dark"),
                       highlight = NULL, breaks = c("equal", "quantile"), title = NULL, geom = NULL) {
    mode   <- match.arg(mode)
    breaks <- match.arg(breaks)
    
    if (!requireNamespace("sf", quietly = TRUE)) {
        stop("Package 'sf' is required for plotting. Install it with: install.packages('sf')")
    }
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop("Package 'ggplot2' is required for plotting. Install it with: install.packages('ggplot2')")
    }

    if (!is.data.frame(data) || nrow(data) == 0) {
        stop("Provided data is empty or not a valid data frame.")
    }

    # Identify language and keys
    is_en <- "region_id" %in% names(data)
    id_col <- if (is_en) "region_id" else "Kennziffer"
    agg_col <- if (is_en) "level_name" else "Aggregat"
    
    if (!id_col %in% names(data)) {
        stop("Data must contain a valid ID column (region_id or Kennziffer).")
    }

    # Identify spatial level
    level_raw <- if (agg_col %in% names(data)) unique(data[[agg_col]])[1] else "KRE"

    # Handle Time/Value column identification
    val_col <- if (is_en) "value" else "Wert"
    time_col <- if (is_en) "year" else "Zeit"
    
    # 1. Detection: Is this the NEW Analytical Wide Format? 
    # (Indicators as columns, specific time column exists)
    is_wide_analytical <- time_col %in% names(data) && !(val_col %in% names(data))
    
    if (is_wide_analytical) {
        meta_cols <- c(id_col, agg_col, time_col, 
                       if(is_en) "region_name" else "Raumeinheit",
                       "M_ID", "indicator_name", "Indikator", "unit", "Einheit", "description", "Beschreibung")
        potential_vars <- setdiff(names(data), meta_cols)
        
        if (length(potential_vars) == 0) stop("No indicator columns found in wide data.")
        
        if (is.null(variable)) {
            variable <- potential_vars[1]
            if (length(potential_vars) > 1) {
                cli::cli_alert_warning("Multi-indicator data detected. Plotting first variable: {.val {variable}}")
            }
        }
        
        if (!variable %in% potential_vars) {
            stop("Variable '", variable, "' not found. Available indicators: ", paste(potential_vars, collapse = ", "))
        }
        
        val_col <- variable
        indicator_title <- variable
        
        # Filter year (coerce both sides to integer to avoid type mismatch)
        if (!is.null(year)) {
            data <- data[as.integer(as.character(data[[time_col]])) == as.integer(year), ]
            if (nrow(data) == 0) stop("No data found for year: ", year)
        } else {
            max_year <- max(as.integer(as.character(data[[time_col]])), na.rm = TRUE)
            data <- data[as.integer(as.character(data[[time_col]])) == max_year, ]
            year <- max_year
        }
    } else if (val_col %in% names(data) && time_col %in% names(data)) {
        # Long format
        if (!is.null(year)) {
            data <- data[as.integer(as.character(data[[time_col]])) == as.integer(year), ]
            if (nrow(data) == 0) stop("No data found for year: ", year)
        } else {
            max_year <- max(as.integer(as.character(data[[time_col]])), na.rm = TRUE)
            data <- data[as.integer(as.character(data[[time_col]])) == max_year, ]
            year <- max_year
        }
        
        ind_name_col <- if (is_en) "indicator_name" else "Indikator"
        indicator_title <- if (ind_name_col %in% names(data)) unique(data[[ind_name_col]])[1] else "Indicator Value"
    } else {
        # Wide format - years as columns (older style)
        year_cols <- grep("^[0-9]{4}$", names(data), value = TRUE)
        if (length(year_cols) == 0) stop("No valid value/year columns found.")
        
        if (!is.null(year)) {
            val_col <- as.character(year)
            if (!val_col %in% year_cols) stop("No data found for year: ", year)
        } else {
            val_col <- max(year_cols)
            year <- val_col
        }
        indicator_title <- "Indicator Value"
    }

    unit_col <- if (is_en) "unit" else "Einheit"
    unit_str <- if (unit_col %in% names(data) && !is.na(data[[unit_col]][1])) paste0(" (", data[[unit_col]][1], ")") else ""

    caption <- NULL
    if (!is.null(geom)) {
        if (!inherits(geom, "sf")) {
            stop("The 'geom' argument must be an sf object.")
        }
        map_sf <- geom
        if (id_col %in% names(map_sf)) {
            join_key <- id_col
        } else {
            potential_keys <- c("Kennziffer", "region_id", "ID", "id", "Schl\u00fcssel", "schluessel", "CC_1", "CC_2", "CC_3")
            found_keys <- intersect(potential_keys, names(map_sf))
            if (length(found_keys) > 0) {
                join_key <- found_keys[1]
                message("Joining custom geometry using column '", join_key, "'")
            } else {
                join_key <- names(map_sf)[1]
                warning("No clear ID column found in custom geometry. Joining on the first column: '", join_key, "'")
            }
        }
        map_sf[[join_key]] <- as.character(map_sf[[join_key]])
        data[[id_col]] <- as.character(data[[id_col]])
        map_sf <- merge(map_sf, data, by.x = join_key, by.y = id_col, all.x = TRUE)
    } else {
        map_level <- detect_map_level(level_raw)
        if (is.na(map_level)) {
            stop(
                "plot_inkar() only supports 'BND', 'BLD', 'KRE', 'GVB' and 'GEM' spatial levels for automatic maps. ",
                "Detected level: '", level_raw, "'. ",
                "For other levels (like ROR) please provide your own sf geometry via the 'geom' parameter."
            )
        }

        message("Loading map geometries...")
        map_sf <- get_inkar_geometry(map_level)
        join_key <- "Kennziffer"

        # Restore leading zeros if the ID column was read as a number
        pad_width <- c(BND = 1L, BLD = 2L, KRE = 5L, GVB = 9L, GEM = 8L)[[map_level]]
        parsed_ids <- suppressWarnings(as.numeric(data[[id_col]]))
        if (any(is.na(parsed_ids))) {
            data[[id_col]] <- as.character(data[[id_col]])
        } else {
            data[[id_col]] <- formatC(parsed_ids, width = pad_width, flag = "0", format = "d", big.mark = "")
        }

        unmatched <- setdiff(unique(data[[id_col]]), map_sf[[join_key]])
        if (length(unmatched) > 0) {
            warning(
                length(unmatched), " region(s) have no boundary and are not drawn: ",
                paste(utils::head(unmatched, 5), collapse = ", "),
                if (length(unmatched) > 5) ", ..." else "",
                ". INKAR may use a newer territorial status; pass your own boundaries via 'geom'.",
                call. = FALSE
            )
        }
        map_sf <- merge(map_sf, data, by.x = join_key, by.y = id_col, all.x = TRUE)
        caption <- gisco_attribution
    }

    # Highlight: add alpha column
    if (!is.null(highlight)) {
        name_col_map <- if (is_en) "region_name" else "Raumeinheit"
        hl_mask <- text_match(map_sf[[name_col_map]], highlight)
        map_sf$.hl_alpha <- ifelse(hl_mask, 1, 0.25)
    } else {
        map_sf$.hl_alpha <- 1
    }

    # Styling colors
    fill_pal <- if (mode == "dark") "magma" else "viridis"

    # Build fill scale: quantile or equal
    fill_values <- map_sf[[val_col]]
    fill_values <- fill_values[!is.na(fill_values)]
    na_fill <- if (mode == "dark") "grey10" else "grey95"
    fill_scale <- if (breaks == "quantile" && length(fill_values) >= 5) {
        # Binned scale: each quintile gets its own color step, and the legend
        # spaces the steps evenly. A continuous scale with quantile ticks kept
        # the colors linear and crowded the labels together.
        q_breaks <- unique(stats::quantile(fill_values, probs = seq(0.2, 0.8, by = 0.2), names = FALSE))
        ggplot2::scale_fill_viridis_b(
            option   = fill_pal,
            breaks   = q_breaks,
            labels   = format_legend_number,
            na.value = na_fill
        )
    } else {
        ggplot2::scale_fill_viridis_c(
            option   = fill_pal,
            labels   = format_legend_number,
            na.value = na_fill
        )
    }

    plot_title <- if (!is.null(title)) title else indicator_title

    p <- ggplot2::ggplot(map_sf) +
        ggplot2::geom_sf(
            ggplot2::aes(fill = .data[[val_col]], alpha = .data$.hl_alpha),
            color    = if (mode == "dark") "grey20" else "white",
            linewidth = 0.05
        ) +
        ggplot2::scale_alpha_identity() +
        fill_scale +
        theme_inkaR(mode = mode) +
        ggplot2::theme(axis.title = ggplot2::element_blank(), axis.text = ggplot2::element_blank(), panel.grid = ggplot2::element_blank()) +
        ggplot2::labs(
            title    = plot_title,
            subtitle = paste0("Spatial Level: ", level_raw, " | Year: ", year),
            fill     = unit_str,
            caption  = caption
        )

    return(p)
}

#' Map an INKAR level label to a level code supported by GADM maps
#'
#' Matches whole labels, because "Bundeslaender" must not be read as "Bund".
#'
#' @param level_raw Level code or label from the `Aggregat` / `level_name` column.
#' @return One of "BND", "BLD", "KRE", "GVB", "GEM", or NA.
#' @noRd
detect_map_level <- function(level_raw) {
    key <- tolower(trimws(as.character(level_raw)))
    # Names are built with setNames(): a non-ASCII argument name is mangled to
    # "<U+00E4>" when the package is installed in a non-UTF-8 locale
    lookup <- stats::setNames(
        c("BND", "BND", "BND", "BND",
          "BLD", "BLD", "BLD", "BLD", "BLD", "BLD", "BLD", "BLD",
          "KRE", "KRE", "KRE",
          "GVB", "GVB", "GVB", "GVB",
          "GEM", "GEM", "GEM"),
        c("bnd", "bu", "bund", "germany",
          "bld", "bl", "bundesl\u00e4nder", "l\u00e4nder", "bundeslaender", "laender",
          "federal states", "states",
          "kre", "kreise", "districts",
          "gvb", "gemeindeverb\u00e4nde", "gemeindeverbaende", "municipal associations",
          "gem", "gemeinden", "municipalities")
    )
    if (length(key) == 0 || is.na(key) || !key %in% names(lookup)) return(NA_character_)
    unname(lookup[[key]])
}

#' Format legend values without scientific notation
#' @noRd
format_legend_number <- function(x) {
    x <- signif(x, 3)
    format(x, big.mark = ",", scientific = FALSE, trim = TRUE, drop0trailing = TRUE)
}
