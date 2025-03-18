# Steam Game Dataset Variables Documentation

## Overview

This document provides a comprehensive explanation of the variables in the `steam_games_data.csv` dataset, which contains information about Steam games collected using the updated `steam_data_collector` script. The dataset focuses on game characteristics, localisation status (particularly Chinese), pricing, and user reviews across different languages. The data is available in both CSV format and as an R data file (RDS) created using the `csv_to_rds.R` script.

## Data Collection Methodology

The data was collected using the Steam Web API with the following methodology:

-   Multiple API keys were used to work within rate limits
-   Caching mechanisms implemented to prevent redundant API calls
-   A simple three-step filtering approach to identify genuine games:
    1. First checking if total review number is no less than 100 (reduced from previous 200 threshold)
    2. Checking if the app has required gaming tags (Single-player, Multi-player, PvP, etc.)
    3. Excluding apps with specific non-game tags (Downloadable Content, Demos, Soundtracks, etc.)
-   Only games released since 2010 were included
-   Special attention to language support, particularly Chinese localisation
-   Review data collected for multiple languages (English, Simplified Chinese)

## R Data Format Conversion

The dataset has been converted from CSV to RDS format using the `csv_to_rds.R` script, which offers several advantages for R users:

-   **Improved Data Types**: The R script properly converts list columns (genres, developers, publishers, user_tags) from string representation to native R lists
-   **International Character Support**: Ensures proper encoding of international characters, particularly Chinese text
-   **Efficient Storage**: RDS format is more efficient for R users as it preserves R-specific data types and structures
-   **Direct Loading**: Can be loaded directly into R using the `readRDS()` function without needing to specify column types

To use the RDS file in R:

```r
steam_data <- readRDS("steam_games_data.rds")
```

## Dataset Variables

The dataset contains 35 variables across the games. Below is a detailed explanation of each variable:

### Game Identification

| Variable       | Type    | Description                          |
|----------------|---------|--------------------------------------|
| `app_id`       | Integer | Unique Steam application identifier  |
| `english_name` | String  | Game title in English                |
| `chinese_name` | String  | Game title in Chinese (if available) |

### Game Creators

| Variable     | Type          | Description                               |
|--------------|---------------|-------------------------------------------|
| `developers` | String/List\* | List of development companies/individuals |
| `publishers` | String/List\* | List of publishing companies              |

\*In the RDS file, these are converted to native R lists

### Game Metadata

| Variable               | Type          | Description                                                    |
|------------------------|---------------|----------------------------------------------------------------|
| `release_date`         | String        | Date when the game was released (YYYY-MM-DD format)            |
| `supported_languages`  | String        | Full list of languages supported by the game                   |
| `genres`               | String/List\* | Game genres as defined by Steam                                |
| `user_tags`            | String/List\* | User-defined tags describing the game                          |
| `current_player_count` | Integer       | Number of players currently playing at time of data collection |

\*In the RDS file, these are converted to native R lists

### Pricing Information

| Variable    | Type    | Description                                                    |
|-------------|---------|----------------------------------------------------------------|
| `price_usd` | String  | Price in US dollars (numeric string without currency symbol)   |
| `price_cny` | String  | Price in Chinese Yuan (numeric string without currency symbol) |
| `is_free`   | Boolean | Whether the game is free-to-play (`True`/`False`)              |

### Language Support

| Variable                                 | Type    | Description                                        |
|------------------------------------------|---------|----------------------------------------------------|
| `chinese_simplified_interface_subtitles` | Boolean | Game includes Simplified Chinese UI/text/subtitles |
| `chinese_simplified_audio`               | Boolean | Game includes Simplified Chinese voice acting      |
| `english_interface_subtitles`            | Boolean | Game includes English UI/text/subtitles            |
| `english_audio`                          | Boolean | Game includes English voice acting                 |

### Global Review Metrics

| Variable         | Type    | Description                                           |
|------------------|---------|-------------------------------------------------------|
| `total_reviews`  | Integer | Total number of user reviews across all languages     |
| `total_positive` | Integer | Total number of positive reviews across all languages |
| `total_negative` | Integer | Total number of negative reviews across all languages |

### Simplified Chinese Review Metrics

| Variable                  | Type    | Description                                                 |
|---------------------------|---------|-------------------------------------------------------------|
| `schinese_reviews`        | Integer | Total number of Simplified Chinese reviews                  |
| `schinese_positive`       | Integer | Number of positive Simplified Chinese reviews               |
| `schinese_negative`       | Integer | Number of negative Simplified Chinese reviews               |
| `schinese_positive_ratio` | Float   | Ratio of positive to total Simplified Chinese reviews (0-1) |

### English Review Metrics

| Variable                 | Type    | Description                                      |
|--------------------------|---------|--------------------------------------------------|
| `english_reviews`        | Integer | Total number of English reviews                  |
| `english_positive`       | Integer | Number of positive English reviews               |
| `english_negative`       | Integer | Number of negative English reviews               |
| `english_positive_ratio` | Float   | Ratio of positive to total English reviews (0-1) |

### Other Reviews

| Variable        | Type    | Description                                                                                   |
|-----------------|---------|-----------------------------------------------------------------------------------------------|
| `other_reviews` | Integer | Number of reviews in languages other than English, Simplified Chinese, or Traditional Chinese |

## Data Filtering Criteria

The dataset was created with the following simplified filtering criteria:

1. **Review Count Threshold**: Only games with at least 100 total reviews were included (reduced from previous 200 threshold)

2. **Required Gaming Tags**: Games must have at least one of these gaming-related tags:
   - Single-player
   - Multi-player (or Multiplayer)
   - PvP
   - Online PvP
   - Online Co-op

3. **Exclusion Tags**: Games were excluded if they contained any of these non-game tags:
   - Downloadable Content
   - Demos
   - Soundtracks
   - Playtests
   - Videos
   - Mods
   - Software
   - Utilities

4. **Additional Filters**:
   - Games must have a valid release date from 2010 onwards
   - Games must have valid genre information

This simplified approach ensures that the dataset contains actual playable games rather than other types of Steam content while removing complex regex-based filtering and relationship checks from previous versions.

## Technical Notes

-   In the CSV file, list fields (developers, publishers, genres, user_tags) are stored as comma-separated strings
-   In the RDS file, these list fields are properly converted to R list objects
-   Boolean values are represented as `True`/`False`
-   Price fields may contain "Free" for free-to-play games or "Unknown" if price data couldn't be retrieved
-   Positive ratio fields range from 0-1 (multiply by 100 for percentage)
-   Game names in Chinese may be null if the game doesn't have a Chinese store page
-   The data collection focused particularly on Chinese localisation and review sentiment, allowing for comparison between different language communities
-   The RDS conversion ensures proper handling of UTF-8 encoding for international characters
