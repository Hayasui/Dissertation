# Steam Data Collector Documentation

*by Huixian Chen*

## Executive Summary

The Steam Data Collector is a comprehensive tool that extracts, processes, and analyses game data from the Steam platform using its API. The script efficiently identifies genuine games, collects detailed metadata, and organises results for research purposes. Special attention is given to language localisation (particularly Chinese), review sentiment analysis across different languages, and user-defined tags that provide richer categorisation than official genres.

## 1. Technical Requirements

### 1.1 Software Requirements

-   **Python 3.6+** with the following packages:
    -   requests: For API calls and web scraping
    -   pandas: For data manipulation and exporting
    -   concurrent.futures: For parallel processing
    -   typing: For type annotations
    -   Other standard libraries: time, json, os, pickle, re, subprocess, sys, hashlib, threading, datetime
-   **R 4.x+** (optional but recommended for RDS file generation)
    -   The script expects R to be installed at `C:\PROGRA~1\R\R-44~1.3\bin\x64\Rscript.exe` by default but will attempt to find it in the system PATH if not present at the specified location

### 1.2 API Authentication

-   **Steam API Keys**: The script uses multiple API keys to handle rate limiting and API quotas. By default, it includes placeholder API keys which should be replaced with your own valid keys in a production environment. You can get a valid key from [Steam Community :: Steam Web API Key](https://steamcommunity.com/dev/apikey)

### 1.3 System Requirements

-   **Storage Space**: Sufficient storage to accommodate the cache directory and output files
-   **Memory**: At least 4GB RAM recommended for processing large datasets
-   **Network Connection**: Reliable internet connection for API calls

## 2. Research Purpose

The primary purpose of this script is to collect comprehensive data about Steam games for research and analysis purposes. It's particularly designed to investigate:

1.  **Game Localisation**: How games support different languages, with special focus on Chinese (both Simplified and Traditional) compared to English
2.  **Review Sentiment Analysis**: How reviews' number and ratio varies across language communities
3.  **Pricing Differences**: How games are priced in different regions (USD vs CNY)
4.  **Game Popularity**: Current player counts and total review volume
5.  **Game Categorisation**: Collection of user-defined tags for more nuanced classification of games beyond publisher-defined genres

## 3. Data Collection Methodology

### 3.1 Filtering Criteria

The script employs sophisticated filtering to identify genuine games:

-   **Name-based Filtering**: Uses regex patterns to identify and exclude DLCs, soundtracks, tools, etc.
-   **Whitelist Protection**: Maintains a whitelist of games that might be incorrectly filtered
-   **API Relationship Data**: Checks API metadata to identify if an app is a game or DLC
-   **Review Volume Threshold**: Requires a minimum number of reviews (200 by default)
-   **Release Date Filtering**: Excludes games released before 2010

### 3.2 User-Defined Tags Collection

The script collects user-defined tags from multiple sources to ensure comprehensive categorisation data:

1.  **API Categories**: Collects basic categories from the Steam API
2.  **API Tags Field**: Extracts tags from the Steam API tags field, handling both dictionary and list formats
3.  **Store Page Scraping**: When API data is insufficient (fewer than 5 tags), the script attempts to extract the "Popular user-defined tags" section from the game's Steam store page
4.  **Tag Deduplication**: Removes duplicate tags while preserving the original order, ensuring the most relevant tags appear first

This enhanced tag collection provides much richer categorisation than the limited publisher-defined genres, offering valuable insights into how the community perceives and classifies games.

## 4. Script Workflow

The script operates through the following sequential steps:

### 4.1 Initialisation

-   Sets up output directories and cache structure
-   Initialises the API key manager and rate limiter
-   Loads checkpoint data if resuming a previous run

### 4.2 Testing Phase

-   Performs a series of tests to verify API connectivity and functionality
-   Checks caching mechanism, CSV deduplication, and API key rotation
-   Tests user tag extraction functionality
-   Waits for user confirmation before proceeding to the main collection phase

### 4.3 App List Retrieval

-   Fetches the complete list of apps available on Steam
-   Applies initial filtering based on name patterns to exclude obvious non-games
-   Logs filtering decisions for later analysis

### 4.4 Detailed Game Processing

For each potential game, checks:

-   Review count to meet minimum threshold
-   App type data from the API to confirm it's a game
-   Release date to ensure it meets the year criteria

For each qualifying game, collects:

-   Basic metadata (name, developers, publishers, release date, etc.)
-   Supported languages with detailed localisation level
-   English and Chinese name variations
-   Pricing in USD and CNY
-   Current player count
-   Review statistics for English, Simplified Chinese, and Traditional Chinese
-   User-defined tags from multiple sources (API categories, tags field, and store page HTML when necessary)

### 4.5 Data Storage

-   Incrementally saves data to CSV to prevent data loss
-   Implements deduplication to handle duplicate entries
-   Creates checkpoint files to support resuming interrupted runs
-   Tracks API usage to manage rate limits and key rotation

### 4.6 Data Export

-   Exports data in multiple formats:
    -   CSV: For general use and Excel compatibility
    -   Feather: For high-performance data exchange with R
    -   RDS: Native R format for statistical analysis
-   Generates an R script for immediate data import and basic analysis

## 5. Output Files and Directory Structure

After running the script, your working directory will contain the following files and folders:

### 5.1 Main Output Files

-   **steam_games_data.csv**: Primary results file in CSV format with UTF-8-BOM encoding
-   **steam_games_data_final.csv**: Final processed dataset after complete run
-   **steam_games_data_final.feather**: Data in Feather format for R integration
-   **steam_games_data_final.rds**: Data in native R format (if R is available)
-   **steam_games_data_final_import.R**: Generated R script for data import and analysis

### 5.2 Process Management Files

-   **checkpoint.pkl**: Binary checkpoint file to track processing progress
-   **processed_games.json**: JSON list of successfully processed games
-   **api_usage.json**: Tracking data for API key usage and rotation

### 5.3 Logging Files

-   **dlc_filtering_log.json**: Log of apps identified as DLCs and excluded
-   **keyword_filtering_log.json**: Log of apps filtered based on name patterns

### 5.4 Directories

-   **api_cache/**: Directory containing cached API responses
    -   Organised by endpoint subdirectories for efficient retrieval
    -   Uses SHA-256 hashing for cache key generation

## 6. Usage Instructions

### 6.1 Command-Line Arguments

The script supports several command-line arguments:

-   **--app-id**: Process a specific game by its Steam App ID
-   **--app-name**: Specify a name for the game (will be fetched if not provided)
-   **--keep-cache**: Keep existing cache for the app ID
-   **--quiet**: Disable verbose logging
-   **--deduplicate**: Deduplicate the CSV file without processing games

### 6.2 Example Usage

```
python steam_data_collector_multikey_v7.py --app-id 2001120
```

## 7. Technical Features

### 7.1 Performance Optimisations

-   **Parallel Processing**: Implements adaptive parallel processing with dynamic worker adjustment based on success rates
-   **Caching**: Comprehensive caching system to prevent redundant API calls
-   **API Key Rotation**: Automatically rotates between multiple API keys to avoid rate limiting
-   **Incremental Storage**: Saves results incrementally to prevent data loss
-   **Memory-Efficient Batching**: Processes games in batches to maintain reasonable memory usage

### 7.2 Enhanced Functionality

-   **Enhanced Language Detection**: Special handling for language codes and audio support
-   **Date Parsing**: Handles various date formats from the Steam API
-   **Duplicate Prevention**: Intelligent deduplication of results
-   **Rate Limiting**: Adaptive rate limiting based on API response codes
-   **Configurable Filtering**: Detailed regex patterns for non-game detection
-   **Multi-Source Tag Collection**: Extracts user-defined tags from API data and falls back to web scraping when necessary
-   **Tag Deduplication**: Ensures unique tag collections while preserving original ordering for relevance
-   **British English Support**: All output messages and documentation use British English conventions
