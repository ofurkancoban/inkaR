# INKAR Indicators Metadata

A comprehensive list of available indicators from the INKAR database.
This dataset is used to lookup indicator IDs, names, and descriptions.

## Usage

``` r
indicators
```

## Format

A data frame with the following columns:

- ID:

  Short identifier from the INKAR indicator overview (e.g., "q_alo"), or
  the M_ID as text where INKAR publishes none

- M_ID:

  Numeric key the INKAR API expects (the wizard "Gruppe")

- IndID:

  Official INKAR indicator number from the indicator overview. It
  differs from M_ID for most indicators (e.g. 1101 vs 12) and is
  accepted by
  [`get_inkar_data()`](https://ofurkancoban.github.io/inkaR/reference/get_inkar_data.md)
  as well

- Name_DE:

  German name of the indicator

- Short_DE:

  Official German short name (e.g. "Arbeitslosenquote")

- Name_EN:

  English name (translated or placeholder)

- Description_DE:

  Detailed German description

- Description_EN:

  Detailed English description (missing for about a quarter of the
  indicators)

- Theme:

  Topic area of the INKAR wizard in German (e.g. "Arbeitslosigkeit");
  see
  [`get_themes()`](https://ofurkancoban.github.io/inkaR/reference/get_themes.md)

- Active:

  Logical. TRUE if the indicator is offered by the INKAR API. FALSE
  marks discontinued indicators kept for backward compatibility

- Algorithmus:

  Algorithm used (if any)

- Anmerkungen:

  Notes in German

- Anmerkungen_EN:

  Notes in English

- Gemeinden:

  Availability for Municipalities

- Kreise:

  Availability for Districts

- Statistische Grundlagen:

  Statistical basis (DE)

- Stat_Grund_EN:

  Statistical basis (EN)

- Unit_DE:

  Unit of measurement (DE)

- Unit_EN:

  Unit of measurement (EN)

## Source

<https://www.inkar.de/>
