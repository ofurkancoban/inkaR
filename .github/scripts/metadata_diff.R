# Summarise how data/indicators.rda changed, for the metadata sync pull request.
#
# Usage: Rscript metadata_diff.R <old.rda> <new.rda> <report.md>
# Writes the Markdown summary and, on GitHub Actions, the step output
# `changed` ("true" or "false"). Exits with an error if the files cannot be
# compared.

args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 3)

load_indicators <- function(path) {
  env <- new.env()
  load(path, envir = env)
  env$indicators
}
old <- load_indicators(args[1])
new <- load_indicators(args[2])
report_file <- args[3]

changed <- !identical(old, new)
output <- Sys.getenv("GITHUB_OUTPUT")
if (nzchar(output)) cat(sprintf("changed=%s\n", tolower(changed)), file = output, append = TRUE)

label <- function(df) paste0("`", df$ID, "` (M_ID ", df$M_ID, ") ", df$Name_DE)
section <- function(title, items, max = 50) {
  if (length(items) == 0) return(character())
  shown <- utils::head(items, max)
  more <- if (length(items) > max) sprintf("- ... and %d more", length(items) - max) else character()
  c(sprintf("### %s (%d)", title, length(items)), "", paste("-", shown), more, "")
}

lines <- c(
  "Automated sync of `data/indicators.rda` with the live INKAR wizard",
  "(`data-raw/sync_indicators_api.R`).",
  "",
  sprintf("| | Before | After |"),
  "|---|---|---|",
  sprintf("| Active indicators | %d | %d |", sum(old$Active), sum(new$Active)),
  sprintf("| Discontinued | %d | %d |", sum(!old$Active), sum(!new$Active)),
  ""
)

if (!changed) {
  lines <- c(lines, "No changes.")
} else {
  added <- new[new$Active & !new$M_ID %in% old$M_ID[old$Active], ]
  retired <- new[!new$Active & new$M_ID %in% old$M_ID[old$Active], ]
  placeholder_en <- added[!is.na(added$Name_EN) & added$Name_EN == added$Name_DE, ]

  lines <- c(lines,
    section("New indicators", label(added)),
    section("No longer offered by INKAR (kept as discontinued)", label(retired))
  )
  if (nrow(placeholder_en) > 0) {
    lines <- c(lines,
      "> **Before merging:** the new indicators have the German name as English",
      "> placeholder (`Name_EN`) and no official short ID. Add translations in",
      "> `data-raw/process_metadata.R` if needed.",
      "")
  }

  # Field changes of indicators present in both versions
  common <- intersect(old$M_ID, new$M_ID)
  o <- old[match(common, old$M_ID), ]
  n <- new[match(common, new$M_ID), ]
  cols <- intersect(names(o), names(n))
  field_lines <- character()
  for (col in cols) {
    a <- as.character(o[[col]])
    b <- as.character(n[[col]])
    same <- (is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & a == b)
    diff_idx <- which(!same)
    if (length(diff_idx) == 0) next
    examples <- utils::head(diff_idx, 5)
    shorten <- function(x) ifelse(is.na(x), "NA", ifelse(nchar(x) > 60, paste0(substr(x, 1, 57), "..."), x))
    field_lines <- c(field_lines,
      sprintf("- **%s**: %d indicator(s), e.g.", col, length(diff_idx)),
      sprintf("  - `%s`: %s -> %s", n$ID[examples], shorten(a[examples]), shorten(b[examples])))
  }
  if (length(field_lines) > 0) {
    lines <- c(lines, "### Changed fields", "", field_lines, "")
  }
  lines <- c(lines,
    "Merging updates the bundled metadata; it reaches users with the next release.",
    "",
    "Pull requests opened by GitHub Actions do not start the R-CMD-check workflow.",
    "Close and reopen this pull request to run it before merging.")
}

writeLines(lines, report_file, useBytes = TRUE)
cat(lines, sep = "\n")
