# This script processes the raw metadata file into an .rda file for the package
# Run this manually when the source Excel file changes.

library(readxl)
library(dplyr)
library(usethis)


# Read German Data
raw_de <- readxl::read_excel(
  "Uebersicht der Indikatoren DE.xlsx",
  sheet = "Raumbeobachtung DE",
  skip = 1
)

# Read English Data
raw_en <- readxl::read_excel(
  "Uebersicht der Indikatoren EN.xlsx",
  sheet = " Spatial observation EN",
  skip = 1
)

# Process DE (Primary)
df_de <- raw_de |>
  dplyr::rename(
    ID = any_of(c("Kürzel", "Code")),
    Name_DE = any_of(c("Kurzname", "Indikator")),
    Description_DE = any_of(c("Name", "Beschreibung")),
    Theme = any_of(c("Bereich", "Theme"))
  ) |>
  dplyr::filter(!is.na(ID), !is.na(Name_DE))

# Process EN (Lookup)
df_en <- raw_en |>
  dplyr::select(
    M_ID,
    Name_EN = name # Extract English Name
  ) |>
  dplyr::filter(!is.na(M_ID))

# Join and Finalize
indicators <- df_de |>
  dplyr::left_join(df_en, by = "M_ID") |>
  dplyr::mutate(
    # Fallback if specific EN name missing
    Name_EN = ifelse(is.na(Name_EN), paste(Name_DE, "(EN)"), Name_EN),
    Theme = if("Theme" %in% names(df_de)) Theme else NA_character_,
    Unit_DE = NA_character_,
    Unit_EN = NA_character_
  ) |>
  dplyr::select(
    ID,
    Name_DE,
    Name_EN,
    Description_DE,
    Theme,
    dplyr::everything()
  )

# Save to data/ directory
usethis::use_data(indicators, overwrite = TRUE)

message("Processed ", nrow(indicators), " indicators.")
