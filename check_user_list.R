devtools::load_all()

# User's codes
codes <- c(
    "398",
    "11",
    "399",
    "304",
    "407",
    "12",
    "33",
    "2",
    "1",
    "143",
    "162",
    "231",
    "244"
)
levels <- c("BND", "LND", "ROR", "KRE", "GEM")

cat(sprintf("%-6s | %-30s | %s\n", "Code", "Name (Short)", "Available Levels"))
cat(paste(rep("-", 60), collapse = ""), "\n")

for (code in codes) {
    # Get name from metadata if possible
    ind_meta <- subset(inkaR::indicators, M_ID == as.numeric(code))
    name <- if (nrow(ind_meta) > 0) {
        substr(ind_meta$Name_DE, 1, 25)
    } else {
        "Unknown"
    }

    found_levels <- c()

    for (lvl in levels) {
        # Try fetch
        res <- try(inkaR:::get_time_references(code, lvl), silent = TRUE)
        if (!inherits(res, "try-error") && nrow(res) > 0) {
            found_levels <- c(found_levels, lvl)
        }
    }

    avail_str <- if (length(found_levels) > 0) {
        paste(found_levels, collapse = ", ")
    } else {
        "NONE"
    }
    cat(sprintf("%-6s | %-30s | %s\n", code, name, avail_str))
}
