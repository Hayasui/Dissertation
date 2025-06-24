# Steam Data Collector Script Documentation v0.6.01

## Overview

The Steam Data Collector is a Python script designed to collect comprehensive game data from the Steam platform using the Steam Web API. It focuses on gathering information about games, their localisation status (particularly for Chinese languages), pricing in different regions, and user reviews across different languages. The script implements efficient data collection techniques including multi-key API access, caching, rate limiting, parallel processing, and **enhanced thread-safe operations**. With the latest updates, it now includes enhanced tag extraction capabilities, tools for updating existing datasets, **improved data integrity verification**, and **robust thread-safe processing**.

## What's New in v0.6.01

### Major Enhancements

1. **Thread-Safe Operations**
   - Added `DataCollectionState` class for thread-safe state management
   - Implemented `csv_write_lock` for thread-safe file writing
   - Enhanced parallel processing with better concurrency control

2. **Data Integrity & Reliability**
   - New `verify_data_integrity()` function for dataset validation
   - Enhanced `deduplicate_csv_file()` with better edge case handling
   - Improved error handling in CSV operations with explicit data types

3. **Cache Management**
   - New `clear_cache()` function for fresh data collection
   - `--clear-cache` command-line option
   - Better cache organization and management

4. **Processing Improvements**
   - Apps now sorted by `app_id` for consistent processing order
   - Better state management with global `state_manager`
   - Enhanced error recovery and data persistence

5. **Command-Line Enhancements**
   - `--verify` option for data integrity checks
   - `--clear-cache` option for cache management
   - Improved argument handling and validation

## Script Components

### Key Classes

#### `DataCollectionState` **[NEW]**

- **Thread-safe state management** for concurrent processing
- Tracks processed app IDs and results across multiple threads
- Provides atomic operations for adding processed apps and checking status
- Ensures data consistency in parallel processing environments

```python
class DataCollectionState:
    def __init__(self):
        self.lock = threading.Lock()
        self.processed_app_ids = set()
        self.results = []
```

#### `SteamAPIKeyManager`

- Manages multiple Steam API keys to work within rate limits
- Tracks usage of each key and automatically rotates keys when approaching limits
- Implements daily reset of usage counters
- Saves usage statistics for persistence between runs
- Applies penalties to keys that encounter rate limits

#### `RateLimiter`

- Controls the rate of concurrent API requests
- Uses semaphores to manage access to resources
- Helps prevent hitting Steam API rate limits

### Major Functions

#### Data Collection Functions

| Function                     | Description                                                                                          |
|------------------------------|------------------------------------------------------------------------------------------------------|
| `get_all_games()`            | Retrieves the complete list of apps from Steam API with **consistent app_id sorting**               |
| `process_single_game()`      | Core function that processes an individual game, applying filtering criteria and collecting all data |
| `get_game_details()`         | Fetches detailed information about a game via API (English version)                                  |
| `get_game_details_chinese()` | Fetches Chinese-specific details including localized name and pricing                                |
| `get_review_summary()`       | Collects review statistics for different languages                                                   |
| `get_current_player_count()` | Retrieves the current number of active players                                                       |
| `parse_language_support()`   | Analyses supported languages string to determine localisation level                                  |
| `get_store_page_tags()`      | Enhanced function that extracts tags from store pages using multiple reliable methods                |
| `check_app_relationship()`   | Determines if an app is a DLC or non-game content                                                    |

#### Tag Management Functions

| Function                       | Description                                                         |
|--------------------------------|---------------------------------------------------------------------|
| `normalize_tag()`              | Converts tags to a standardized form for comparison                 |
| `is_semantic_duplicate()`      | Identifies semantically equivalent tags to prevent duplication      |
| `update_existing_games_tags()` | Updates existing dataset with additional tags from store pages      |
| `convert_to_list()`            | Handles various tag formats (strings, lists, etc.) for consistency  |
| `merge_tags()`                 | Combines user tags and genres with intelligent deduplication        |

#### Data Integrity & Management Functions **[ENHANCED/NEW]**

| Function                  | Description                                                               |
|---------------------------|---------------------------------------------------------------------------|
| `verify_data_integrity()` | **[NEW]** Comprehensive validation of dataset for duplicates and missing data |
| `deduplicate_csv_file()`  | **[ENHANCED]** Improved deduplication with edge case handling and explicit dtypes |
| `clear_cache()`           | **[NEW]** Complete cache clearing for fresh data collection               |
| `append_game_to_results()` | **[ENHANCED]** Thread-safe CSV writing with improved duplicate detection |

#### Utility Functions

| Function                  | Description                                                               |
|---------------------------|---------------------------------------------------------------------------|
| `cached_api_request()`    | Makes API requests with efficient caching to prevent redundant calls      |
| `parse_steam_date()`      | Handles various date formats used by Steam                                |
| `clean_price()`           | Normalizes price strings by removing currency symbols                     |
| `save_data_for_r()`       | Exports collected data in R-friendly formats (CSV, Feather, RDS)          |
| `initialise_results_file()` | Sets up CSV file with proper headers for incremental writing            |
| `run_r_script()`          | Executes R scripts for generating RDS files with robust encoding handling |

#### Process Management Functions

| Function                      | Description                                                                     |
|-------------------------------|---------------------------------------------------------------------------------|
| `process_games_in_parallel()` | **[ENHANCED]** Thread-safe parallel processing with improved state management  |
| `batch_generator()`           | Creates batches of games for memory-efficient processing                        |
| `save_checkpoint()`           | Maintains checkpoint data to enable resuming interrupted collection             |
| `load_checkpoint()`           | **[ENHANCED]** Restores state with improved state manager integration          |
| `press_any_key_to_continue()` | Pauses execution until user confirmation                                        |

## Data Collection Process

The script follows a systematic process for collecting game data with **enhanced reliability**:

1. **Initialisation**:
   - Set up API key management
   - Initialize **thread-safe state manager**
   - Create directory structure
   - Initialize caching system
   - **Optional cache clearing** for fresh starts

2. **Retrieve Game List**:
   - Call the Steam API to get a complete list of applications
   - **Sort apps by app_id for consistent processing order**
   - Prepare the list for detailed processing

3. **Game Processing (for each game)** with **Thread-Safe Operations**:
   - Check review count threshold (≥ 100 reviews)
   - Verify the app is a game (not DLC or other content)
   - Apply the filtering criteria for game tags
   - Collect basic game information
   - Fetch Chinese localisation and pricing details
   - Retrieve and analyse language support
   - Collect review metrics by language
   - Gather current player count
   - **Thread-safe state updates and result storage**

4. **Data Storage** with **Enhanced Integrity**:
   - **Thread-safe incremental saving** to CSV
   - Export final data in multiple formats for analysis
   - Create R import script for seamless transition to R analysis
   - **Comprehensive data integrity verification**

## Tag Extraction Enhancements

The script includes significant improvements to tag extraction (unchanged from previous version):

1. **Robust HTML Parsing**: Uses BeautifulSoup for reliable HTML parsing
2. **Improved Age Verification**: Bypasses age gates on mature content
3. **Semantic Deduplication**: Recognises equivalent tags with different formatting
4. **Comprehensive Tag Collection**: Always retrieves tags from both API and store pages

## Dataset Update Feature

Enhanced dataset updating capabilities:

1. **Targeted Tag Updates**: Updates games in existing datasets with newly extracted tags
2. **Controlled Processing**: Enables processing of a limited number of games for testing
3. **Intelligent Tag Integration**: Adds only genuinely new tags to existing records
4. **Thread-Safe Updates**: **[NEW]** Ensures data consistency during concurrent operations

## Filtering Mechanism

The script implements a robust filtering process (unchanged filtering logic):

1. **Review Threshold Filter**: Games must have at least 100 total reviews
2. **DLC/Non-Game Identification**: Uses API relationship data to identify non-games
3. **Required Gaming Tags Check**: Games must have core gaming tags
4. **Exclusion Tags Check**: Removes non-game content
5. **Additional Filters**: Release date (2010+) and genre validation

## Technical Features

### **Enhanced Concurrency & Thread Safety** **[NEW]**

- **Thread-Safe State Management**: 
  - `DataCollectionState` class ensures atomic operations
  - Thread-safe tracking of processed apps and results
  - Prevents race conditions in parallel processing
- **Thread-Safe File Operations**: 
  - `csv_write_lock` prevents concurrent CSV write conflicts
  - Atomic file operations for data consistency
- **Improved Parallel Processing**: 
  - Better coordination between worker threads
  - Enhanced error handling in concurrent environments

### API Request Handling

- **Multi-Key Management**: 
  - Rotates between multiple API keys to maximize throughput
  - Tracks usage per key with daily automatic reset
  - Applies penalties to keys that hit rate limits
- **Intelligent Caching**: 
  - Stores API responses locally to reduce redundant calls
  - **Enhanced cache management** with clearing capabilities
  - Uses SHA-256 hashing for reliable cache key generation
- **Rate Limiting**: Controls request frequency to avoid hitting API limits
- **Error Handling**: Gracefully handles API errors, retries, and timeouts

### **Data Integrity & Validation** **[ENHANCED]**

- **Comprehensive Validation**: 
  - `verify_data_integrity()` checks for duplicates and missing data
  - Validates dataset consistency and completeness
  - Reports dataset statistics and potential issues
- **Enhanced Deduplication**: 
  - Improved edge case handling in CSV deduplication
  - Explicit data type handling for app_id consistency
  - Better handling of empty or corrupted files
- **Robust Error Recovery**: 
  - Enhanced error handling throughout the pipeline
  - Better backup and recovery mechanisms

### HTML Parsing

- **BeautifulSoup Integration**: Provides robust HTML parsing capabilities
- **Multiple Selector Strategies**: Handles variations in Steam's HTML structure
- **Fallback Mechanisms**: Uses multiple methods to extract data
- **Age Verification Bypass**: Accesses age-restricted content

### Persistence & Reliability

- **Enhanced Checkpointing**: **Thread-safe** progress saving and restoration
- **Improved Incremental Saving**: **Thread-safe** CSV writing with duplicate detection
- **Better Backup Mechanisms**: Enhanced file protection and recovery
- **Atomic Operations**: **[NEW]** Ensures data consistency in concurrent environments
- **Comprehensive State Tracking**: **[NEW]** Better progress monitoring and recovery

### Language Support Analysis

- **Enhanced Language Detection**: Identifies interface/subtitle and audio support
- **Symbol Pattern Recognition**: Uses regex patterns for audio language markers
- **Language Code Mapping**: Maps between Steam's codes and internal representation
- **Review Analysis by Language**: Collects and analyzes reviews in different languages

### Data Formats

The script outputs data in multiple formats:
- **CSV**: Universal format with UTF-8-BOM encoding for Excel compatibility
- **Feather**: Efficient binary format that preserves column types
- **RDS**: Native R format generated via automated R script

## Command-Line Usage

The script supports enhanced command-line arguments:

```bash
python steam_data_collector.py [options]

Options:
  --app-id ID        Process a specific game by its Steam App ID
  --app-name NAME    Optional name for the game (will be fetched if not provided)
  --keep-cache       Keep existing cache for the app ID
  --quiet            Disable verbose logging
  --deduplicate      Deduplicate the CSV file without processing games
  --update-tags      Update existing games with additional tags from store pages
  --skip-tests       Skip initial functionality tests
  --clear-cache      [NEW] Clear all cached API responses before starting
  --verify           [NEW] Verify data integrity of the final dataset
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

## Dependencies

The script requires the following Python packages:
- `requests`: For making HTTP requests to the Steam API
- `pandas`: For data manipulation and CSV/Feather output
- `pyarrow`: For Feather file format support
- `concurrent.futures`: For parallel processing
- `beautifulsoup4`: For robust HTML parsing
- `threading`: **[ENHANCED]** For thread-safe operations
- `re`: For regular expression pattern matching
- `time`, `datetime`: For time-based operations
- `os`, `sys`: For file system and environment operations
- `pickle`: For checkpoint data serialization
- `json`: For JSON handling
- `hashlib`: For cache key generation
- `subprocess`: For executing R scripts

## Best Practices

When running the script:

1. **API Keys**: Use multiple valid Steam API keys for best performance
2. **Disk Space**: Ensure sufficient disk space for caching and output files
3. **Runtime**: Expect several hours to days for complete data collection
4. **Throttling**: The script self-throttles to respect API limits
5. **Resuming**: Use checkpoints to resume interrupted runs
6. **Dataset Updates**: Use the `--update-tags` option to enhance existing datasets
7. **Single App Processing**: Use `--app-id` for testing or processing specific games
8. **Cache Management**: 
   - Use `--keep-cache` to preserve existing cache for repeated runs
   - Use `--clear-cache` **[NEW]** for completely fresh data collection
9. **Data Integrity**: 
   - Use `--verify` **[NEW]** to check dataset integrity
   - Use `--deduplicate` to clean existing datasets
10. **Thread Safety**: **[NEW]** The script now handles concurrent operations safely
11. **Don't run this script inside your OneDrive folder**

## Troubleshooting & Maintenance

### **New Maintenance Features** **[NEW]**

- **Data Integrity Checks**: Use `--verify` to validate your dataset
- **Cache Management**: Use `--clear-cache` when experiencing data inconsistencies
- **Thread Safety**: The script now handles concurrent processing more reliably
- **Enhanced Error Recovery**: Better handling of interrupted processes and corrupted data

### Performance Optimization

- **Consistent Processing Order**: Apps are now sorted by app_id for predictable behavior
- **Improved State Management**: Better tracking of progress and processed items
- **Thread-Safe Operations**: Eliminates race conditions in parallel processing
- **Enhanced Memory Management**: Better handling of large datasets

## Version History

### v0.6.01 (Current)
- **Added thread-safe operations** with `DataCollectionState` class
- **Enhanced data integrity** with verification and improved deduplication
- **New command-line options**: `--clear-cache` and `--verify`
- **Improved parallel processing** with better concurrency control
- **Consistent app processing order** with app_id sorting
- **Enhanced error handling** throughout the pipeline

### v0.3.26 (Previous)
- Enhanced tag extraction capabilities
- Tools for updating existing datasets
- Improved HTML parsing with BeautifulSoup
- Semantic tag deduplication
- Comprehensive filtering mechanism

---

*This documentation reflects the enhanced capabilities and improvements in Steam Data Collector v0.6.01, with particular focus on thread safety, data integrity, and reliability improvements.*