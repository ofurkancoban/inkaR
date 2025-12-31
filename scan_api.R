
devtools::load_all()

# Scan range 1..800 based on previous findings
range_to_test <- 1:800
active_list <- data.frame(ID = integer(), Active = logical())

cat("Scanning IDs 1 to", max(range_to_test), "to find all active API indicators...\n")

for (id in range_to_test) {
    id_str <- as.character(id)
    if (id %% 50 == 0) cat(sprintf("Checking ID: %d...\n", id))
    
    # Check specifically at KRE (Kreise) level as it's the most common
    res <- try(inkaR:::get_time_references(id_str, "KRE"), silent = TRUE)
    
    if (!inherits(res, "try-error") && nrow(res) > 0) {
        # Fetch name if possible to be helpful
        # Note: Since get_inkar_data fetches metadata, we might try to grab the name from there
        # But get_inkar_data returns a dataframe with 'indicator_name' column.
        # Let's try to get a sample data point to extract the name.
        
        # Checking metadata endpoint directly might be faster?
        # Metadata endpoint: /Wizard/GetZeitscheiben returns 'Indikator' name column
        name <- res$Indikator[1]
        
        cat(sprintf("FOUND ACTIVE: %d -> %s\n", id, name))
        active_list <- rbind(active_list, data.frame(ID = id, Name = name, Active = TRUE))
    }
}

cat("\nScan Complete.\n")
cat("Total Active found:", nrow(active_list), "\n")
write.csv(active_list, "api_catalog.csv", row.names = FALSE)
print(head(active_list, 20))
