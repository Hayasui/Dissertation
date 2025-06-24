# Steam Data Collector Script Documentation

## Overview

The Steam Data Collector is a Python script designed to collect comprehensive game data from the Steam platform using the Steam Web API. It focuses on gathering information about games, their localisation status (particularly for Chinese languages), pricing in different regions, and user reviews across different languages. The script implements efficient data collection techniques including multi-key API access, caching, rate limiting, and parallel processing.

## Script Components

### Key Classes

#### `SteamAPIKeyManager`
- Manages multiple Steam API keys to work within rate limits
- Tracks usage of each key and automatically rotates keys when approaching limits
- Implements daily reset of usage counters
- Saves usage statistics for persistence between runs

#### `RateLimiter`
- Controls the rate of concurrent API requests
- Uses semaphores to manage access to resources
- Helps prevent hitting Steam API rate limits

### Major Functions

#### Data Collection Functions

| Function | Description |
|----------|-------------|
| `get_all_games()` | Retrieves the complete list of apps from Steam API |
| `process_single_game()` | Core function that processes an individual game, applying filtering criteria and collecting all data |
| `get_game_details()` | Fetches detailed information about a game via API (English version) |
| `get_game_details_chinese()` | Fetches Chinese-specific details including localized name and pricing |
| `get_review_summary()` | Collects review statistics for different languages |
| `get_current_player_count()` | Retrieves the current number of active players |
| `parse_language_support()` | Analyses supported languages string to determine localisation level |

#### Utility Functions

| Function | Description |
|----------|-------------|
| `cached_api_request()` | Makes API requests with efficient caching to prevent redundant calls |
| `parse_steam_date()` | Handles various date formats used by Steam |
| `get_store_page_tags()` | Extracts additional tag information from the store page when API data is insufficient |
| `clean_price()` | Normalizes price strings by removing currency symbols |
| `save_data_for_r()` | Exports collected data in R-friendly formats (CSV, Feather, RDS) |

#### Process Management Functions

| Function | Description |
|----------|-------------|
| `process_games_in_parallel()` | Implements parallel processing of multiple games with dynamic worker adjustment |
| `batch_generator()` | Creates batches of games for memory-efficient processing |
| `save_checkpoint()` | Maintains checkpoint data to enable resuming interrupted collection |
| `load_checkpoint()` | Restores state from previous checkpoints |

## Data Collection Process

The script follows a systematic process for collecting game data:

1. **Initialization**: 
   - Set up API key management
   - Create directory structure
   - Initialize caching system

2. **Retrieve Game List**:
   - Call the Steam API to get a complete list of applications
   - Prepare the list for detailed processing

3. **Game Processing (for each game)**:
   - Check review count threshold (≥ 100 reviews)
   - Verify the app is a game (not DLC or other content)
   - Apply the filtering criteria for game tags
   - Collect basic game information
   - Fetch Chinese localization and pricing details
   - Retrieve and analyze language support
   - Collect review metrics by language
   - Gather current player count

4. **Data Storage**:
   - Save results incrementally to CSV
   - Export final data in multiple formats for analysis
   - Create R import script for seamless transition to R analysis

## Filtering Mechanism

The script implements a three-step filtering process to identify genuine games:

1. **Review Threshold Filter**:
   - Games must have at least 100 total reviews
   - This ensures sufficient user engagement and data quality

2. **Required Gaming Tags Check**:
   - Games must have at least one of the following tags:
     - Single-player
     - Multi-player (or Multiplayer)
     - PvP
     - Online PvP
     - Online Co-op
   - This helps identify actual playable games

3. **Exclusion Tags Check**:
   - Games are excluded if they contain any of these tags:
     - Downloadable Content
     - Demos
     - Soundtracks
     - Playtests
     - Videos
     - Mods
     - Software
     - Utilities
   - This removes non-game content from the dataset

4. **Additional Filters**:
   - Games must have been released from 2010 onwards
   - Games must have valid genre information
   - Basic game metadata must be available

## Technical Features

### API Request Handling

- **Multi-Key Management**: Rotates between multiple API keys to maximize throughput
- **Intelligent Caching**: Stores API responses locally to reduce redundant calls
- **Rate Limiting**: Controls request frequency to avoid hitting API limits
- **Error Handling**: Gracefully handles API errors, retries, and timeouts

### Parallel Processing

- **ThreadPoolExecutor**: Processes multiple games concurrently
- **Dynamic Worker Adjustment**: Adjusts number of worker threads based on success rates
- **Rate-Limited Execution**: Ensures parallel requests don't overwhelm the API

### Persistence & Reliability

- **Checkpointing**: Regularly saves progress to enable resuming interrupted runs
- **Incremental Saving**: Writes data to CSV as soon as a game is processed
- **Backup Mechanisms**: Creates backup files in case of data corruption
- **Deduplication**: Ensures no duplicate entries in the final dataset

### Data Formats

The script outputs data in multiple formats:

- **CSV**: Universal format with UTF-8-BOM encoding for Excel compatibility
- **Feather**: Efficient binary format that preserves column types
- **RDS**: Native R format generated via automated R script

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
```

## Data Output

The script outputs several files:

- `steam_data.csv`: Main CSV dataset with all collected game data
- `steam_data.feather`: Binary format preserving list columns
- `steam_data.rds`: R-native data format generated via R script
- `processed_games.json`: JSON backup of all processed games
- `api_usage.json`: Tracking data for API key usage
- `checkpoint.pkl`: Checkpoint data for resuming interrupted runs
- `dlc_filtering_log.json`: Log of apps identified as DLC/non-games
- `keyword_filtering_log.json`: Log of apps filtered by keyword patterns

## Dependencies

The script requires the following Python packages:

- `requests`: For making HTTP requests to the Steam API
- `pandas`: For data manipulation and CSV/Feather output
- `pyarrow`: For Feather file format support
- `concurrent.futures`: For parallel processing

## Best Practices

When running the script:

1. **API Keys**: Use multiple valid Steam API keys for best performance
2. **Disk Space**: Ensure sufficient disk space for caching and output files
3. **Runtime**: Expect several hours to days for complete data collection
4. **Throttling**: The script self-throttles to respect API limits
5. **Resuming**: Use checkpoints to resume interrupted runs
