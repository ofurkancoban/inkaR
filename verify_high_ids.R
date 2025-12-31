devtools::load_all()

# Get indicators with high M_IDs appearing to have data (Kreise not NA)
inds <- inkaR::indicators
high_inds <- subset(inds, M_ID > 1000 & !is.na(Kreise))

cat("Total high M_ID candidates:", nrow(high_inds), "\n")

if (nrow(high_inds) > 0) {
    # Test first 5
    to_test <- head(high_inds$M_ID, 5)

    for (id in to_test) {
        cat("Testing M_ID:", id, " (", subset(inds, M_ID == id)$Name_DE, ")\n")
        # Use internal get_time_references for direct check
        # Note: get_inkar_data now handles numeric/string ID conversion
        res <- try(
            inkaR:::get_time_references(as.character(id), "KRE"),
            silent = TRUE
        )

        if (inherits(res, "try-error")) {
            cat("  -> ERROR\n")
        } else if (nrow(res) > 0) {
            cat("  -> SUCCESS (", nrow(res), " rows)\n")
        } else {
            cat("  -> NO DATA\n")
        }
    }
} else {
    cat("No high M_ID indicators found with Kreise data.\n")
}
