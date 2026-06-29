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

  Short identifier (e.g., "001")

- M_ID:

  Numeric internal ID used by API

- Name_DE:

  German name of the indicator

- Name_EN:

  English name (translated or placeholder)

- Description_DE:

  Detailed German description

- Description_EN:

  Detailed English description (available for 413 indicators)

- Theme:

  Group/Domain of the indicator

- Active:

  Logical. TRUE if verified as active in the API

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
