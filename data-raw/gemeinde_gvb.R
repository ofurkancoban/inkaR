# --- Gemeinde -> Gemeindeverband lookup ---
# Builds the internal table `gvb_lookup` (R/sysdata.rda) that maps every
# Gemeinde AGS (8 digits) to its Gemeindeverband code (9 digits). plot_inkar()
# uses it to dissolve GISCO LAU municipality polygons into GVB polygons,
# because GISCO does not publish Gemeindeverbaende.
#
# Source: Destatis Gemeindeverzeichnis (GV-ISys), Gebietsstand 31.12.2023.
# This matches GISCO LAU 2024 and the territorial status used by INKAR.
# When INKAR moves to a newer Gebietsstand, update `gv_date` together with
# `inkar_gisco_year` in R/geometry.R and rerun this script.

gv_date <- "31122023"
gv_url <- paste0(
  "https://www.destatis.de/DE/Themen/Laender-Regionen/Regionales/Gemeindeverzeichnis/",
  "Administrativ/Archiv/GVAuszugJ/", gv_date, "_Auszug_GV.xlsx?__blob=publicationFile"
)

gv_file <- tempfile(fileext = ".xlsx")
utils::download.file(gv_url, gv_file, mode = "wb", quiet = TRUE)

# Sheet 2 holds the records. Columns: 1 Satzart, 3 Land, 4 RB, 5 Kreis,
# 6 Gemeindeverband (VB), 7 Gemeinde, 8 name. Satzart 60 = Gemeinde.
gv <- readxl::read_excel(gv_file, sheet = 2, col_names = FALSE, col_types = "text",
                         .name_repair = "minimal")
names(gv) <- paste0("c", seq_along(gv))
gem <- gv[gv$c1 %in% "60", ]

gvb_lookup <- data.frame(
  AGS = paste0(gem$c3, gem$c4, gem$c5, gem$c7),
  GVB = paste0(gem$c3, gem$c4, gem$c5, gem$c6),
  stringsAsFactors = FALSE
)
gvb_lookup <- gvb_lookup[!duplicated(gvb_lookup$AGS), ]

stopifnot(
  all(nchar(gvb_lookup$AGS) == 8),
  all(nchar(gvb_lookup$GVB) == 9),
  nrow(gvb_lookup) > 10000
)

attr(gvb_lookup, "gebietsstand") <- "2023-12-31"
usethis::use_data(gvb_lookup, internal = TRUE, overwrite = TRUE, compress = "xz")
