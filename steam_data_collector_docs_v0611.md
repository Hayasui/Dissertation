# Steam Data Collector Script Documentation

## Overview

The Steam Data Collector is a Python script designed to collect comprehensive game data from the Steam platform using the Steam Web API. It focuses on gathering information about games, their localisation status (particularly for Chinese languages), pricing in different regions, and user reviews across different languages. The script implements efficient data collection techniques including multi-key API access, caching, rate limiting, and parallel processing. The latest version features advanced tag extraction capabilities with robust HTML parsing, comprehensive store page scraping, and tools for updating existing datasets.

## Script Components

### Key Classes

#### `SteamAPIKeyManager`

-   Manages multiple Steam API keys to work within rate limits
-   Tracks usage of each key and automatically rotates keys when approaching limits
-   Implements daily reset of usage counters
-   Saves usage statistics for persistence between runs
-   Applies penalties to keys that encounter rate limits

#### `RateLimiter`

-   Controls the rate of concurrent API requests
-   Uses semaphores to manage access to resources
-   Helps prevent hitting Steam API rate limits

### Major Functions

#### Data Collection Functions

| Function                     | Description                                                                                          |
|------------------------------|------------------------------------------------------------------------------------------------------|
| `get_all_games()`            | Retrieves the complete list of apps from Steam API                                                   |
| `process_single_game()`      | Core function that processes an individual game, applying filtering criteria and collecting all data |
| `get_game_details()`         | Fetches detailed information about a game via API (English version) and integrates store page tags  |
| `get_game_details_chinese()` | Fetches Chinese-specific details including localized name and pricing                                |
| `get_review_summary()`       | Collects review statistics for different languages                                                   |
| `get_current_player_count()` | Retrieves the current number of active players                                                       |
| `parse_language_support()`   | Analyses supported languages string to determine localisation level                                  |
| `get_store_page_tags()`      | Advanced function that extracts tags from store pages using multiple robust parsing methods         |
| `check_app_relationship()`   | Determines if an app is a DLC or non-game content                                                    |

#### Tag Management Functions

| Function                       | Description                                                         |
|--------------------------------|---------------------------------------------------------------------|
| `normalize_tag()`              | Converts tags to a standardized form for comparison                 |
| `is_semantic_duplicate()`      | Identifies semantically equivalent tags to prevent duplication      |
| `update_existing_games_tags()` | Updates existing dataset with additional tags from store pages      |
| `convert_to_list()`            | Handles various tag formats (strings, lists, etc.) for consistency  |
| `merge_tags()`                 | Combines user tags and genres with intelligent deduplication        |

#### Utility Functions

| Function                  | Description                                                               |
|---------------------------|---------------------------------------------------------------------------|
| `cached_api_request()`    | Makes API requests with efficient caching to prevent redundant calls      |
| `parse_steam_date()`      | Handles various date formats used by Steam                                |
| `clean_price()`           | Normalizes price strings by removing currency symbols                     |
| `save_data_for_r()`       | Exports collected data in R-friendly formats (CSV, Feather, RDS)          |
| `deduplicate_csv_file()`  | Removes duplicate entries from CSV files                                  |
| `initialise_results_file()` | Sets up CSV file with proper headers for incremental writing            |
| `append_game_to_results()`  | Adds a single game to the results with duplicate handling               |
| `run_r_script()`          | Executes R scripts for generating RDS files with timeout protection and robust encoding handling |

#### Process Management Functions

| Function                      | Description                                                                     |
|-------------------------------|---------------------------------------------------------------------------------|
| `process_games_in_parallel()` | Implements parallel processing of multiple games with dynamic worker adjustment and resume capability |
| `batch_generator()`           | Creates batches of games for memory-efficient processing                        |
| `save_checkpoint()`           | Maintains checkpoint data to enable resuming interrupted collection             |
| `load_checkpoint()`           | Restores state from previous checkpoints                                        |
| `press_any_key_to_continue()` | Pauses execution until user confirmation                                        |

## Data Collection Process

The script follows a systematic process for collecting game data:

1.  **Initialisation**:
    -   Set up API key management
    -   Create directory structure
    -   Initialize caching system
2.  **Retrieve Game List**:
    -   Call the Steam API to get a complete list of applications
    -   Prepare the list for detailed processing
3.  **Game Processing (for each game)**:
    -   Check review count threshold (≥ 100 reviews)
    -   Verify the app is a game (not DLC or other content)
    -   Apply the filtering criteria for game tags
    -   Collect basic game information from API
    -   Always retrieve additional tags from store pages for comprehensive coverage
    -   Fetch Chinese localisation and pricing details
    -   Retrieve and analyse language support
    -   Collect review metrics by language
    -   Gather current player count
4.  **Data Storage**:
    -   Save results incrementally to CSV
    -   Export final data in multiple formats for analysis
    -   Create R import script for seamless transition to R analysis

## Advanced Tag Extraction System

The script features a sophisticated tag extraction system that combines multiple data sources:

1.  **Multi-Source Tag Collection**:
    -   Always retrieves tags from both Steam API and store pages
    -   Combines API categories, user-defined tags, and scraped store tags
    -   Provides comprehensive tag coverage for all games
2.  **Robust HTML Parsing**:
    -   Uses BeautifulSoup for reliable HTML parsing instead of basic regex
    -   Implements multiple selector strategies for different HTML structures:
        - Primary selector: `div.glance_tags.popular_tags a.app_tag`
        - Secondary selector: `div.glance_tags a.app_tag`
        - Legacy selectors: `div.app_tags.popular_tags a.app_tag` and `div.popular_tags a.app_tag`
        - Fallback selector: `a.app_tag` (any app_tag element)
        - Enhanced regex pattern as final fallback
    -   Handles both visible and hidden tags for complete coverage
3.  **Enhanced Age Verification**:
    -   Automatically bypasses age gates on mature content
    -   Uses cookies and parameters to access age-restricted games
    -   Ensures tags are collected from all games regardless of content rating
4.  **Intelligent Tag Processing**:
    -   Filters out UI elements (like '+' add buttons)
    -   Handles various Steam HTML structures and layout changes
    -   Provides detailed logging of extraction methods used

## Dataset Update Feature

A comprehensive feature for enhancing existing datasets:

1.  **Targeted Tag Enhancement**:
    -   Updates games in existing datasets with newly extracted store page tags
    -   Preserves all other data while enhancing tag coverage
    -   Allows incremental improvement of datasets without full recollection
2.  **Controlled Processing**:
    -   Enables processing of a limited number of games for testing
    -   Implements incremental saving during updates
    -   Provides detailed progress tracking and statistics
3.  **Intelligent Tag Integration**:
    -   Adds only genuinely new tags to existing records
    -   Avoids semantic duplicates using sophisticated comparison
    -   Prevents genre duplication in tag lists
    -   Generates merged game_tags that combine user tags and genres
    -   Handles various data formats (lists, strings, JSON representations)

## Filtering Mechanism

The script implements a comprehensive filtering process to identify genuine games:

1.  **Review Threshold Filter**:
    -   Games must have at least 100 total reviews
    -   This ensures sufficient user engagement and data quality
2.  **DLC/Non-Game Identification**:
    -   Uses `check_app_relationship()` to identify DLCs and non-game content
    -   Examines app type, categories, and required apps
    -   Logs filtered apps for analysis and debugging
3.  **Required Gaming Tags Check**:
    -   Games must have at least one of the following tags:
        -   Single-player / Singleplayer
        -   Multi-player / Multiplayer
        -   PvP / Online PvP
        -   Online Co-op
    -   This helps identify actual playable games
4.  **Exclusion Tags Check**:
    -   Games are excluded if they contain any of these tags:
        -   Downloadable Content
        -   Demos
        -   Soundtracks
        -   Playtests
        -   Videos
        -   Mods
        -   Software
        -   Utilities
    -   This removes non-game content from the dataset
5.  **Additional Filters**:
    -   Games must have been released from 2010 onwards
    -   Games must have valid genre information
    -   Basic game metadata must be available

## Technical Features

### API Request Handling

-   **Multi-Key Management**: 
    -   Rotates between multiple API keys to maximize throughput
    -   Tracks usage per key with daily automatic reset
    -   Applies penalties to keys that hit rate limits
-   **Intelligent Caching**: 
    -   Stores API responses locally to reduce redundant calls
    -   Uses SHA-256 hashing for reliable cache key generation
    -   Organizes cache by endpoint for better management
-   **Rate Limiting**: 
    -   Controls request frequency to avoid hitting API limits
    -   Implements exponential backoff for failed requests
-   **Error Handling**: 
    -   Gracefully handles API errors, retries, and timeouts
    -   Provides detailed error logging for troubleshooting

### HTML Parsing and Store Page Extraction

-   **BeautifulSoup Integration**: Provides robust HTML parsing capabilities for reliable tag extraction
-   **Multiple Selector Strategies**: Handles variations in Steam's HTML structure across different page layouts
-   **Comprehensive Fallback System**: Uses multiple methods to extract data when primary methods fail
-   **Age Verification Bypass**: Automatically handles cookies and parameters to access age-restricted content
-   **Tag Filtering**: Excludes UI elements and focuses on actual game tags
-   **Steam Layout Adaptation**: Handles both current and legacy Steam store page structures

### Parallel Processing

-   **ThreadPoolExecutor**: Processes multiple games concurrently with efficient resource management
-   **Dynamic Worker Adjustment**: Adjusts number of worker threads based on success rates and error patterns
-   **Rate-Limited Execution**: Ensures parallel requests don't overwhelm the API
-   **Batch Processing**: Divides workload into manageable batches for better memory usage
-   **Resume Capability**: Properly handles existing results when resuming interrupted collections

### Persistence & Reliability

-   **Advanced Checkpointing**: Regularly saves progress to enable resuming interrupted runs with full state restoration
-   **Incremental Saving**: Writes data to CSV as soon as a game is processed
-   **Backup Mechanisms**: Creates backup files in case of data corruption
-   **Comprehensive Deduplication**: Ensures no duplicate entries in the final dataset
-   **API Usage Tracking**: Persists API key usage statistics between runs
-   **Progress Monitoring**: Detailed logging of collection progress and statistics

### Language Support Analysis

-   **Enhanced Language Detection**: Identifies interface/subtitle and audio support for multiple languages
-   **Symbol Pattern Recognition**: Uses regex patterns to identify audio language markers
-   **Language Code Mapping**: Maps between Steam's language codes and internal representation
-   **Review Analysis by Language**: Collects and analyzes reviews in different languages

### Data Processing & Quality

-   **Semantic Duplicate Detection**: Prevents addition of equivalent tags with different formatting
-   **Tag Normalization**: Standardizes tag formats for consistent comparison
-   **Data Type Handling**: Properly manages various data formats (lists, strings, JSON)
-   **Encoding Management**: Handles UTF-8 encoding properly for international content
-   **Timeout Protection**: R script execution includes timeout protection to prevent hanging

### Data Formats

The script outputs data in multiple formats:

-   **CSV**: Universal format with UTF-8-BOM encoding for Excel compatibility
-   **Feather**: Efficient binary format that preserves column types
-   **RDS**: Native R format generated via automated R script with timeout protection

## Command-Line Usage

The script supports several command-line arguments:

```
python steam_data_collector.py [options]

Options:
  --app-id ID        Process a specific game by its Steam App ID
  --app-name NAME    Optional name for the game (will be fetched if not provided)
  --keep-cache       Keep existing cache for the app ID
  --quiet            Disable verbose logging
  --deduplicate      Deduplicate the CSV file without processing games
  --update-tags      Update existing games with additional tags from store pages
  --skip-tests       Skip initial functionality tests
  --clear-cache      Clear all cached API responses before starting
  --verify           Verify data integrity of the final dataset
```

## Data Output

The script outputs several files:

-   `steam_data.csv`: Main CSV dataset with all collected game data
-   `steam_data.feather`: Binary format preserving list columns
-   `steam_data.rds`: R-native data format generated via R script
-   `processed_games.json`: JSON backup of all processed games
-   `api_usage.json`: Tracking data for API key usage
-   `checkpoint.pkl`: Checkpoint data for resuming interrupted runs
-   `dlc_filtering_log.json`: Log of apps identified as DLC/non-games

## Dependencies

The script requires the following Python packages:

-   `requests`: For making HTTP requests to the Steam API and store pages
-   `pandas`: For data manipulation and CSV/Feather output
-   `pyarrow`: For Feather file format support
-   `concurrent.futures`: For parallel processing
-   `beautifulsoup4`: For robust HTML parsing and tag extraction
-   `re`: For regular expression pattern matching
-   `time`, `datetime`: For time-based operations and timeout management
-   `os`, `sys`: For file system and environment operations
-   `pickle`: For checkpoint data serialization
-   `json`: For JSON handling
-   `hashlib`: For cache key generation
-   `subprocess`: For executing R scripts with timeout protection

## Best Practices

When running the script:

1.  **API Keys**: Use multiple valid Steam API keys for best performance
2.  **Disk Space**: Ensure sufficient disk space for caching and output files
3.  **Runtime**: Expect several hours to days for complete data collection
4.  **Throttling**: The script self-throttles to respect API limits
5.  **Resuming**: Use checkpoints to resume interrupted runs with full state restoration
6.  **Dataset Updates**: Use the `--update-tags` option to enhance existing datasets without recollecting all data
7.  **Single App Processing**: Use `--app-id` for testing or processing specific games
8.  **Cache Management**: Use `--keep-cache` to preserve existing cache for repeated runs
9.  **Deduplication**: Use `--deduplicate` to clean existing datasets
10. **Initial Testing**: Run basic tests before main collection to verify functionality
11. **Storage Location**: Avoid running this script inside cloud sync folders (OneDrive, Google Drive, etc.)
12. **Tag Coverage**: The script now provides comprehensive tag coverage by combining API and store page data sources