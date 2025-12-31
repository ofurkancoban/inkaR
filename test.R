# R projenizi açın, ardından:
source("data-raw/process_metadata.R") # Excel verisini işle
devtools::install() # Paketi kur

# Test edin
library(inkaR)
get_indicators() # Listeyi görmelisiniz
