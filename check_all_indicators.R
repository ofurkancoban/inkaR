devtools::load_all()

# Get all indicators
inds <- inkaR::indicators
n <- nrow(inds)

results <- data.frame(
    M_ID = numeric(n),
    ID = character(n),
    Name = character(n),
    Status = character(n),
    Description = character(n),
    stringsAsFactors = FALSE
)

cat("Starting test of all", n, "indicators...\n")

for (i in 1:n) {
    row <- inds[i, ]
    m_id <- row$M_ID
    # Use character M_ID for the call
    id_str <- as.character(m_id)

    # Print progress every 10 indicators
    if (i %% 10 == 0) {
        cat(sprintf("Processing %d/%d (M_ID: %s)...\n", i, n, id_str))
    }

    # Try fetching time references for Kreise (KRE)
    # using internal function to be fast and direct
    res <- try(inkaR:::get_time_references(id_str, "KRE"), silent = TRUE)

    if (inherits(res, "try-error")) {
        status <- "ERROR"
    } else if (nrow(res) > 0) {
        status <- "ACTIVE"
    } else {
        status <- "NO_DATA"
    }

    results$M_ID[i] <- m_id
    results$ID[i] <- row$ID
    results$Name[i] <- row$Name_DE
    results$Status[i] <- status
}

# Save results
write.csv(results, "indicator_status.csv", row.names = FALSE)
active_count <- sum(results$Status == "ACTIVE")

cat("\nTest Complete.\n")
cat("Total Active Indicators:", active_count, "\n")
cat("Results saved to 'indicator_status.csv'\n")

# Print active M_IDs for immediate view
if (active_count > 0) {
    cat(
        "Active M_IDs sample:",
        paste(
            head(subset(results, Status == "ACTIVE")$M_ID, 20),
            collapse = ", "
        ),
        "...\n"
    )
}
