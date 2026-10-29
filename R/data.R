#' INKAR Indicators Metadata
#'
#' A comprehensive list of available indicators from the INKAR database.
#' This dataset is used to lookup indicator IDs, names, and descriptions.
#'
#' @format A data frame with the following columns:
#' \describe{
#'   \item{ID}{Short identifier from the INKAR indicator overview (e.g., "q_alo"),
#'     or the M_ID as text where INKAR publishes none}
#'   \item{M_ID}{Numeric key the INKAR API expects (the wizard "Gruppe")}
#'   \item{IndID}{Official INKAR indicator number from the indicator overview. It
#'     differs from M_ID for most indicators (e.g. 1101 vs 12) and is accepted by
#'     [get_inkar_data()] as well}
#'   \item{Name_DE}{German name of the indicator}
#'   \item{Short_DE}{Official German short name (e.g. "Arbeitslosenquote")}
#'   \item{Name_EN}{English name (translated or placeholder)}
#'   \item{Description_DE}{Detailed German description}
#'   \item{Description_EN}{Detailed English description (missing for about a quarter
#'     of the indicators)}
#'   \item{Theme}{Topic area of the INKAR wizard in German (e.g. "Arbeitslosigkeit");
#'     see [get_themes()]}
#'   \item{Active}{Logical. TRUE if the indicator is offered by the INKAR API.
#'     FALSE marks discontinued indicators kept for backward compatibility}
#'   \item{Algorithmus}{Algorithm used (if any)}
#'   \item{Anmerkungen}{Notes in German}
#'   \item{Anmerkungen_EN}{Notes in English}
#'   \item{Gemeinden}{Availability for Municipalities}
#'   \item{Kreise}{Availability for Districts}
#'   \item{Statistische Grundlagen}{Statistical basis (DE)}
#'   \item{Stat_Grund_EN}{Statistical basis (EN)}
#'   \item{Unit_DE}{Unit of measurement (DE)}
#'   \item{Unit_EN}{Unit of measurement (EN)}
#' }
#' @source \url{https://www.inkar.de/}
"indicators"
