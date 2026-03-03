#' Plot INKAR Data on German Maps
#'
#' Automatically projects regional INKAR data onto administrative boundaries of Germany using
#' the `geodata` and `sf` packages. Supports Kreise (KRE) and Bundesländer (BLD) levels.
#'
#' @param data A data frame returned by `get_inkar_data()`.
#' @param year Integer/Character. If the data contains multiple years, specify which year to plot. If NULL and multiple years exist, the most recent year is plotted.
#' @return A `ggplot2` object displaying the mapped data.
#' @export
plot_inkar <- function(data, year = NULL) {
    if (!requireNamespace("sf", quietly = TRUE)) {
        stop(
            "Package 'sf' is required for plotting. Install it with: install.packages('sf')"
        )
    }
    if (!requireNamespace("geodata", quietly = TRUE)) {
        stop(
            "Package 'geodata' is required for plotting. Install it with: install.packages('geodata')"
        )
    }
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop(
            "Package 'ggplot2' is required for plotting. Install it with: install.packages('ggplot2')"
        )
    }

    if (!is.data.frame(data) || nrow(data) == 0) {
        stop("Provided data is empty or not a valid data frame.")
    }

    if (!"Kennziffer" %in% names(data) || !"Aggregat" %in% names(data)) {
        stop(
            "Data must contain 'Kennziffer' and 'Aggregat' columns (standard INKAR output)."
        )
    }

    # Identify spatial level from Aggregat column
    level <- unique(data$Aggregat)[1]

    val_col <- "Wert"
    if ("Wert" %in% names(data) && "Zeit" %in% names(data)) {
        # Long format
        if (!is.null(year)) {
            data <- data[data$Zeit == year, ]
            if (nrow(data) == 0) stop("No data found for year: ", year)
        } else {
            # Default to most recent year
            max_year <- max(as.integer(as.character(data$Zeit)), na.rm = TRUE)
            message("Plotting data for most recent year: ", max_year)
            data <- data[data$Zeit == max_year, ]
        }
    } else {
        # Wide format
        # If wide, value columns are just years. Identify them.
        year_cols <- grep("^[0-9]{4}$", names(data), value = TRUE)
        if (length(year_cols) == 0) {
            stop("No valid 'Wert' or year columns found in data.")
        }

        if (!is.null(year)) {
            val_col <- as.character(year)
            if (!val_col %in% year_cols) stop("No data found for year: ", year)
        } else {
            val_col <- max(year_cols)
            message("Plotting data for most recent year column: ", val_col)
        }
    }

    indicator_name <- "Value"
    if ("Indikator" %in% names(data)) {
        indicator_name <- unique(data$Indikator)[1]
    }

    unit_str <- ""
    if ("Einheit" %in% names(data)) {
        unit_val <- data$Einheit[1]
        if (!is.na(unit_val)) unit_str <- paste0(" [", unit_val, "]")
    }

    plot_title <- paste0(indicator_name, unit_str)

    # Mapping based on level
    message("Downloading/loading map polygons via geodata (GADM)...")
    if (level %in% c("Kreise", "KRE")) {
        map_raw <- geodata::gadm("DEU", level = 2, path = tempdir())
        map_sf <- sf::st_as_sf(map_raw)

        # INKAR KRE Kennziffer directly matches CC_2
        map_sf <- merge(
            map_sf,
            data,
            by.x = "CC_2",
            by.y = "Kennziffer",
            all.x = TRUE
        )
    } else if (level %in% c("L\u00E4nder", "Bundesl\u00E4nder", "BLD")) {
        map_raw <- geodata::gadm("DEU", level = 1, path = tempdir())
        map_sf <- sf::st_as_sf(map_raw)

        # INKAR BLD Kennziffer directly matches CC_1
        map_sf <- merge(
            map_sf,
            data,
            by.x = "CC_1",
            by.y = "Kennziffer",
            all.x = TRUE
        )
    } else {
        stop(
            "plot_inkar currently only supports 'Kreise' (KRE) and 'L\u00e4nder' (BLD) levels."
        )
    }

    # Plot
    p <- ggplot2::ggplot(map_sf) +
        ggplot2::geom_sf(
            ggplot2::aes(fill = .data[[val_col]]),
            color = "white",
            linewidth = 0.1
        ) +
        ggplot2::scale_fill_viridis_c(na.value = "grey90", name = "Value") +
        ggplot2::theme_void() +
        ggplot2::theme(
            legend.position = "right",
            plot.title = ggplot2::element_text(
                size = 12,
                face = "bold",
                hjust = 0.5
            ),
            plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5)
        ) +
        ggplot2::labs(
            title = plot_title,
            fill = "Value"
        )

    return(p)
}
