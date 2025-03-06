import requests
import time
import json
import pandas as pd
from datetime import datetime
import os
import pickle
import re
import subprocess
import sys
import hashlib
import concurrent.futures
import threading
from typing import List, Dict, Any, Tuple, Set

# Define multiple API keys，replace X with your own key
API_KEYS = [
    "X",          # Key 1
    "X",          # Key 2
    "X"           # Key 3
]

# Create output directory
output_dir = r"Place holder" # Please type in your working directory here
if not os.path.exists(output_dir):
    os.makedirs(output_dir)

# Create cache directory
CACHE_DIR = os.path.join(output_dir, "api_cache")
if not os.path.exists(CACHE_DIR):
    os.makedirs(CACHE_DIR)

# Set path to R executable
RSCRIPT_PATH = r"C:\PROGRA~1\R\R-44~1.3\bin\x64\Rscript.exe"  # Set to your R executable path

# Checkpoint file to track progress
CHECKPOINT_FILE = os.path.join(output_dir, "checkpoint.pkl")
PROCESSED_GAMES_FILE = os.path.join(output_dir, "processed_games.json")
API_USAGE_FILE = os.path.join(output_dir, "api_usage.json")
CSV_RESULTS_FILE = os.path.join(output_dir, "steam_games_data.csv")
DLC_LOG_FILE = os.path.join(output_dir, "dlc_filtering_log.json")
KEYWORD_LOG_FILE = os.path.join(output_dir, "keyword_filtering_log.json")

# Enhanced non-game pattern detection using regex
REFINED_NON_GAME_PATTERNS = [
    # Basic content types - clear non-games
    r'\b(sound ?track|ost)\b',                       # Soundtrack variations
    r'\bdlc\b',                                      # DLC explicit mention
    r'\bsdk\b',                                      # SDK
    
    # Improved context-aware patterns
    r'(?:^|\s)demo(?:\s|$)',                         # Demo as standalone word, not part of words like "demolition"
    r'(?:^|\s)beta(?:\s|$|[\s-]test)',               # Beta as standalone or "beta test"
    r'\beditor(?:\s+(?:pack|tool|add-?on))?$',       # Editor at the end or followed by specific words
    r'\bserver(?:\s+(?:browser|list|manager))\b',    # Server only when followed by specific words
    r'\b(?:voice|audio)\s+pack\b',                   # Voice/audio packs
    r'\b(?:art ?book|digital\s+art)\b',              # Art book variations
    r'(?:^|\s)tool(?:s|kit)?(?:\s|$)',               # Tools as standalone word
    
    # Content packs - with position context
    r'\b(?:content|skin|map|character)\s+pack\b',    # Various content packs
    r'\bexpansion(?:\s+pack)?(?:\s|$)',              # Expansion packs
    r'\badd-?on(?:\s|$)',                            # Add-on variations
    
    # Collections and bundles - with specific qualifiers
    r'^(?:the\s+)?(?:complete\s+)?collection(?:\s|$)', # Collection at start of name
    r'^(?:the\s+)?bundle(?:\s|$)',                   # Bundle at start of name
    
    # Special editions and passes
    r'\bseason\s+pass\b',                            # Season pass
    r'\b(?:digital\s+)?deluxe(?:\s+edition)?\b',     # Digital deluxe editions
    r'\bcollector\'?s(?:\s+edition)?\b',             # Collector's editions
    r'\bbooster\s+pack\b',                           # Booster packs
    
    # Additional content types
    r'\b(?:wall ?paper|avatar|theme)\b',             # Decorative items
    r'\bvr\s+content\b',                             # VR content (not VR games)
    r'\bsavegame(?:\s|$)',                           # Savegames
    r'\b(?:music|audio)\s+pack\b',                   # Music/audio packs
    
    # Refinement: Only match remake when part of a qualifier phrase
    r'\bofficial\s+remake\b',                        # Only "official remake", not just "remake"
    
    # Lesser-used content types
    r'\bsampler(?:\s|$)',                            # Sampler
    r'\btutorial(?:\s+pack)?\b',                     # Tutorial
    r'\bpreorder\s+(?:bonus|content)\b',             # Preorder bonuses (not preorder of actual game)
    r'\btraining\s+pack\b',                          # Training pack
    r'\bplayer\s+skin\b',                            # Player skin
    r'\bpreset\s+pack\b',                            # Preset packs (not just any preset)
    r'\bavatar\s+frame\b',                           # Avatar frame
    r'\blevel\s+editor\b',                           # Level editor
    r'\bmod\s+tools\b',                              # Mod tools
    r'\basset\s+pack\b',                             # Asset pack
    
    # Additional DLC indicators
    r'(?:^|\s)-\s+DLC$',                             # Names ending with "- DLC"
    r'(?:^|\s)DLC:',                                 # Names starting with "DLC:"
    r'(?:^|\s)DLC\s+\d+',                            # "DLC" followed by numbers
]

# Expanded whitelist for games that might contain filtered keywords but are actual games
EXPANDED_WHITELIST_GAMES = [
    # Original whitelist
    'demolition', 'democracy', 'demon', 'beta testing simulator',
    'server tycoon', 'soundtrack warrior', 'soundtrack attack',
    'art book games', 'the editor',
    
    # Additional common games with potentially filtered words
    'server room simulator', 'the server', 'server empire',
    'collection of saga', 'remake collection', 'remastered collection',
    'demon collection', 'server simulator', 'democracy simulator',
    'tool of warfare', 'tool of destruction', 'demon tools',
    'upgraded', 'collection chamber', 'beta squadron',
    'the collector', 'sound track zero', 'bundle of joy',
    'expansion', 'upgraded world', 'map tools',
    'server heroes', 'the collection', 'deluxe edition',
    'remake project', 'collection of mana', 'preset destiny',
    
    # Popular game franchises with potentially filtered terms
    'mass effect collection', 'half-life collection',
    'the tools of destruction', 'demon souls', 'demon slayer',
    'beta arcade', 'server wars', 'audio surf', 'audio blocks',
    'tool of war'
]

# Pre-compiled regex patterns for language support parsing
AUDIO_FOOTNOTE_PATTERN = re.compile(r'<[^>]+>([*#^+])[^<]*audio[^<]*', re.IGNORECASE)
TAG_PATTERN = re.compile(r'<[^>]+>')
SYMBOL_PATTERN = re.compile(r'[\*\#\^\+]+')

# Steam language code mapping for enhanced language detection
LANG_CODE_MAP = {
    'schinese': ['Simplified Chinese', '简体中文', 'Chinese (Simplified)'],
    'tchinese': ['Traditional Chinese', '繁體中文', 'Chinese (Traditional)'],
    'english': ['English', '英文']
}

# API Key Manager Class
class SteamAPIKeyManager:
    def __init__(self, api_keys: List[str], daily_limit: int = 100000):
        self.api_keys = api_keys
        self.daily_limit = daily_limit
        self.current_key_index = 0
        self.call_counters = {key: 0 for key in api_keys}
        self.last_rotated = datetime.now()
        # Add tracking for the last reset day
        self.last_reset_day = datetime.now().day
    
    def get_current_key(self) -> str:
        """Get the currently active API key"""
        return self.api_keys[self.current_key_index]
    
    def record_api_call(self) -> None:
        """Record an API call against the current key with automatic daily reset"""
        # Check if day has changed since last reset
        current_day = datetime.now().day
        if current_day != self.last_reset_day:
            self.reset_daily_counters()
            self.last_reset_day = current_day
            print(f"New day detected ({current_day}). API call counters have been automatically reset.")
        
        # Continue with existing functionality
        current_key = self.get_current_key()
        self.call_counters[current_key] += 1
        
        # Check if we need to rotate keys
        if self.call_counters[current_key] >= (self.daily_limit * 0.8):  # 80% of limit
            self.rotate_key()
    
    def rotate_key(self) -> bool:
        """
        Rotate to the next available API key
        Returns True if rotation was successful, False if all keys are exhausted
        """
        # Save the old key index
        old_index = self.current_key_index
        
        # Try to find a key with capacity remaining
        for _ in range(len(self.api_keys)):
            self.current_key_index = (self.current_key_index + 1) % len(self.api_keys)
            current_key = self.get_current_key()
            
            # If this key has usage room, use it
            if self.call_counters[current_key] < (self.daily_limit * 0.8):
                print(f"Rotating to next API key: {current_key[:8]}... (used {self.call_counters[current_key]} calls)")
                self.last_rotated = datetime.now()
                return True
                
        # If we get here, all keys are near their limit
        print("WARNING: All API keys are approaching their limits!")
        
        # Just go back to the key we were using before
        self.current_key_index = old_index
        return False
    
    def get_usage_stats(self) -> Dict[str, Any]:
        """Get current usage statistics for all keys"""
        return {
            "call_counters": self.call_counters,
            "current_key_index": self.current_key_index,
            "last_rotated": self.last_rotated.isoformat(),
            "last_reset_day": self.last_reset_day
        }
    
    def load_usage_stats(self, stats: Dict[str, Any]) -> None:
        """Load usage statistics from saved data"""
        if not stats:
            return
            
        self.call_counters = stats.get("call_counters", {key: 0 for key in self.api_keys})
        self.current_key_index = stats.get("current_key_index", 0)
        
        # Load the last reset day if available
        self.last_reset_day = stats.get("last_reset_day", datetime.now().day)
        
        # Ensure all keys are in the counter dict (in case new keys were added)
        for key in self.api_keys:
            if key not in self.call_counters:
                self.call_counters[key] = 0
        
        # Parse last rotated time if available
        last_rotated_str = stats.get("last_rotated")
        if last_rotated_str:
            try:
                self.last_rotated = datetime.fromisoformat(last_rotated_str)
            except ValueError:
                self.last_rotated = datetime.now()
        
    def reset_daily_counters(self) -> None:
        """Reset all counters - call this at the start of a new day"""
        self.call_counters = {key: 0 for key in self.api_keys}
        print("API call counters have been reset for a new day")

# Rate Limiter Class for API requests
class RateLimiter:
    def __init__(self, max_tokens):
        self.semaphore = threading.Semaphore(max_tokens)
    
    def acquire(self):
        return self.semaphore.acquire(blocking=True)
    
    def release(self):
        self.semaphore.release()

# Initialise the key manager
key_manager = SteamAPIKeyManager(API_KEYS)

# API Request Caching - OPTIMISED
def cached_api_request(url, params, cache_duration_hours=24):
    """
    Make API request with caching to prevent redundant calls.
    
    Args:
        url: The API endpoint URL
        params: Dictionary of request parameters
        cache_duration_hours: How long to consider cached results valid
        
    Returns:
        JSON response data
    """
    # Only use essential parameters for cache key generation
    # Extract only the parameters that actually affect the core data structure
    essential_params = {}
    
    # Keep only the most essential parameters for cache key
    if 'appid' in params:
        essential_params['appid'] = params['appid']
    if 'appids' in params:
        essential_params['appids'] = params['appids']
    
    # For other specific APIs, add their essential parameters
    if 'language' in params:  # Review API uses 'language' parameter
        essential_params['language'] = params['language']
    
    # OPTIMISATION: Improved cache structure and key generation
    endpoint = url.split("/")[-2] if "/" in url else "general"
    cache_subdir = os.path.join(CACHE_DIR, endpoint)
    if not os.path.exists(cache_subdir):
        os.makedirs(cache_subdir)
    
    # Use SHA-256 instead of MD5 and sort parameters for consistency
    sorted_params = json.dumps(sorted(essential_params.items()))
    cache_key = hashlib.sha256(f"{url}_{sorted_params}".encode()).hexdigest()
    cache_file = os.path.join(cache_subdir, f"{cache_key}.json")
    
    # Check if we have a valid cached response
    if os.path.exists(cache_file):
        # Check if the cache is still fresh
        file_age_hours = (time.time() - os.path.getmtime(cache_file)) / 3600
        if file_age_hours < cache_duration_hours:
            try:
                with open(cache_file, 'r', encoding='utf-8') as f:
                    print(f"Using cached response for {url}")
                    return json.load(f)
            except Exception as e:
                print(f"Error reading cache: {e}, will make fresh request")
    
    # Add the current API key to the actual request
    actual_params = params.copy()
    actual_params['key'] = key_manager.get_current_key()
    
    # Make the actual API request
    try:
        response = requests.get(url, params=actual_params)
        key_manager.record_api_call()
        
        # OPTIMISATION: Handle rate limiting with 429 error (TOO MANY REQUESTS)
        if response.status_code == 429:
            print(f"Rate limit exceeded (429) for key {key_manager.get_current_key()[:8]}...")
            # Apply penalty to force key rotation
            current_key = key_manager.get_current_key()
            key_manager.call_counters[current_key] += 1000  # Penalty count to force rotation
            key_manager.rotate_key()
            print(f"Rotated to key: {key_manager.get_current_key()[:8]}...")
            time.sleep(2.0)  # Add additional delay after rate limit
            return None
        
        # Only cache successful responses
        if response.status_code == 200:
            try:
                data = response.json()
                with open(cache_file, 'w', encoding='utf-8') as f:
                    json.dump(data, f)
                # Insert delay for rate limiting
                time.sleep(1.0)
                return data
            except Exception as e:
                print(f"Error caching response: {e}")
                # Insert delay for rate limiting
                time.sleep(1.0)
                return response.json()
        else:
            print(f"HTTP error {response.status_code} for URL {url}")
            # Insert delay for rate limiting
            time.sleep(1.0)
            return None
            
    except Exception as e:
        print(f"Request error: {e}")
        time.sleep(1.0)
        return None

# Unified Date Parsing
def parse_steam_date(date_str):
    """
    Parse Steam date strings which come in various formats.
    
    Args:
        date_str: Date string from Steam API
        
    Returns:
        datetime object or None if parsing fails
    """
    if not date_str or not isinstance(date_str, str):
        return None
    
    # Steam date formats, ordered from most to least common
    date_formats = [
        "%d %b, %Y",    # "23 Feb, 2020"
        "%b %d, %Y",    # "Feb 23, 2020"
        "%B %d, %Y",    # "February 23, 2020"
        "%Y-%m-%d",     # "2020-02-23"
        "%d %B, %Y",    # "23 February, 2020"
        "%Y"            # Just the year "2020"
    ]
    
    # Try each format
    for fmt in date_formats:
        try:
            return datetime.strptime(date_str, fmt)
        except ValueError:
            continue
    
    # If we couldn't parse with standard formats, try some custom parsing
    # For "Coming Soon" or future dates specified as quarters
    if "coming soon" in date_str.lower() or "tba" in date_str.lower():
        return None
    
    # For quarter releases like "Q1 2023" 
    quarter_match = re.search(r'Q([1-4])\s+(\d{4})', date_str)
    if quarter_match:
        quarter = int(quarter_match.group(1))
        year = int(quarter_match.group(2))
        month = (quarter - 1) * 3 + 1  # Q1->1, Q2->4, Q3->7, Q4->10
        return datetime(year, month, 1)
    
    print(f"Could not parse date: '{date_str}'")
    return None

# IMPROVED: New function to deduplicate CSV file
def deduplicate_csv_file(csv_path):
    """
    Remove duplicate entries from an existing CSV file.
    Keeps the last occurrence of each app_id.
    
    Args:
        csv_path: Path to the CSV file
        
    Returns:
        bool: True if successful, False otherwise
    """
    try:
        if not os.path.exists(csv_path):
            print(f"CSV file {csv_path} does not exist. Nothing to deduplicate.")
            return False
            
        # Read the CSV file
        df = pd.read_csv(csv_path, encoding='utf-8-sig')
        original_rows = len(df)
        
        # Convert app_id to string for consistent comparison
        df['app_id'] = df['app_id'].astype(str)
        
        # Drop duplicates, keeping the last occurrence (most recent data)
        df = df.drop_duplicates(subset=['app_id'], keep='last')
        new_rows = len(df)
        
        # Check if we found any duplicates
        if original_rows == new_rows:
            print(f"No duplicates found in {csv_path}")
            return True
            
        # Save back to CSV
        df.to_csv(csv_path, index=False, encoding='utf-8-sig')
        
        print(f"Deduplicated {csv_path}: Removed {original_rows - new_rows} duplicate entries")
        return True
    except Exception as e:
        print(f"Error deduplicating CSV: {e}")
        return False

# IMPROVED: Incremental Results Storage with deduplication
def initialise_results_file(output_path):
    """
    Initialise CSV file with headers for incremental writing.
    
    Args:
        output_path: Path to output CSV file
    """
    # Define all possible headers (complete set for your data)
    headers = [
        'app_id', 'english_name', 'chinese_name', 'developers', 'publishers',
        'release_date', 'supported_languages', 'genres', 'user_tags',
        'price_usd', 'price_cny', 'is_free', 'current_player_count',
        'chinese_simplified_interface_subtitles', 'chinese_simplified_audio',
        'chinese_traditional_interface_subtitles', 'chinese_traditional_audio',
        'english_interface_subtitles', 'english_audio',
        'total_reviews', 'total_positive', 'total_negative',
        'schinese_reviews', 'schinese_positive', 'schinese_negative', 'schinese_positive_ratio',
        'tchinese_reviews', 'tchinese_positive', 'tchinese_negative', 'tchinese_positive_ratio',
        'english_reviews', 'english_positive', 'english_negative', 'english_positive_ratio',
        'other_reviews'
    ]
    
    # Check if file already exists
    if os.path.exists(output_path):
        # Deduplicate the existing file
        print(f"CSV file {output_path} already exists. Checking for duplicates...")
        deduplicate_csv_file(output_path)
    else:
        # Initialise the file with headers
        with open(output_path, 'w', newline='', encoding='utf-8-sig') as f:
            pd.DataFrame(columns=headers).to_csv(f, index=False)
        print(f"Initialised new results file: {output_path}")
    
    return output_path

# Append game to results with duplicate checking
def append_game_to_results(game_data, csv_path):
    """
    Append a single game's data to the CSV file, avoiding duplicates.
    
    Args:
        game_data: Dictionary containing game data
        csv_path: Path to the CSV file
    """
    # Check if CSV exists and has content
    if os.path.exists(csv_path) and os.path.getsize(csv_path) > 0:
        try:
            # Read existing data
            existing_data = pd.read_csv(csv_path, encoding='utf-8-sig')
            
            # Check if this app_id already exists
            app_id_str = str(game_data['app_id'])
            if 'app_id' in existing_data.columns and app_id_str in existing_data['app_id'].astype(str).values:
                print(f"App ID {app_id_str} already exists in CSV. Updating existing entry.")
                # Remove existing entry
                existing_data = existing_data[existing_data['app_id'].astype(str) != app_id_str]
                
                # Combine and save entire file
                updated_data = pd.concat([existing_data, pd.DataFrame([game_data])], ignore_index=True)
                updated_data.to_csv(csv_path, index=False, encoding='utf-8-sig')
                return
        except Exception as e:
            print(f"Error checking for duplicates: {e}")
            # Fall back to direct append if there's an error
    
    # If we get here, either the file doesn't exist, is empty, or the app_id isn't in it
    try:
        # Convert game_data to a DataFrame (single row)
        game_df = pd.DataFrame([game_data])
        
        # Append to CSV without writing headers again if file exists and has content
        mode = 'a' if os.path.exists(csv_path) and os.path.getsize(csv_path) > 0 else 'w'
        header = False if os.path.exists(csv_path) and os.path.getsize(csv_path) > 0 else True
        
        with open(csv_path, mode, newline='', encoding='utf-8-sig') as f:
            game_df.to_csv(f, header=header, index=False)
            
        print(f"Appended app ID {game_data['app_id']} to CSV")
    except Exception as e:
        print(f"Error appending to CSV: {e}")
        # Try to save to a backup file
        try:
            backup_file = csv_path.replace('.csv', f'_backup_{int(time.time())}.csv')
            game_df = pd.DataFrame([game_data])
            game_df.to_csv(backup_file, index=False, encoding='utf-8-sig')
            print(f"Saved data to backup file: {backup_file}")
        except Exception as e2:
            print(f"Error saving backup: {e2}")

# Function to run R script with encoding fixes
def run_r_script(script_path):
    """
    Run an R script to generate RDS file from the feather data.
    
    Requires R to be installed and available in the PATH or at RSCRIPT_PATH.
    Handles encoding issues when reading R output.
    
    Args:
        script_path: Path to the R script
    
    Returns:
        True if successful, False otherwise
    """
    try:
        # Use specified R path if available and exists
        if RSCRIPT_PATH and os.path.exists(RSCRIPT_PATH):
            r_cmd = RSCRIPT_PATH
            print(f"Using specified R path: {r_cmd}")
        else:
            # Fallback to PATH if the specified path doesn't exist
            r_cmd = "Rscript" if sys.platform != "win32" else "Rscript.exe"
            print(f"Specified R path not found, using default command: {r_cmd}")
        
        # Run the R script without capturing output through Python's text handling
        print(f"Running R script to generate RDS file: {script_path}")
        
        # Instead of using capture_output=True and text=True which can cause encoding issues,
        # we'll use a direct approach that avoids automatic text decoding
        process = subprocess.Popen(
            [r_cmd, script_path],
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            universal_newlines=False  # Binary mode
        )
        
        # Wait for the process to complete
        stdout_binary, stderr_binary = process.communicate()
        
        # Try to decode with utf-8, but fall back to a more lenient encoding if that fails
        try:
            stdout = stdout_binary.decode('utf-8', errors='replace')
        except UnicodeDecodeError:
            stdout = stdout_binary.decode('latin-1', errors='replace')
            
        try:
            stderr = stderr_binary.decode('utf-8', errors='replace')
        except UnicodeDecodeError:
            stderr = stderr_binary.decode('latin-1', errors='replace')
            
        # Print outputs
        if stdout:
            print("R Output:")
            print(stdout)
        
        if stderr:
            print("R Warnings/Errors:")
            print(stderr)
        
        # Check return code
        if process.returncode != 0:
            print(f"R process returned non-zero exit code: {process.returncode}")
            return False
            
        return True
    except subprocess.CalledProcessError as e:
        print(f"Error running R script: {e}")
        return False
    except FileNotFoundError:
        print(f"R not found at {r_cmd}. Please check your R installation.")
        print(f"You can manually run the R script: {script_path}")
        return False
    except Exception as e:
        print(f"Error running R script: {e}")
        print(f"You can manually run the R script: {script_path}")
        return False

# Function to check if a name is in the whitelist
def is_in_whitelist(app_name):
    """
    Check if app name contains any whitelist terms (games that might be incorrectly filtered).
    
    Args:
        app_name: The app name to check
        
    Returns:
        True if in whitelist, False otherwise
    """
    app_name_lower = app_name.lower()
    return any(term in app_name_lower for term in EXPANDED_WHITELIST_GAMES)

# Function to get store page tags
def get_store_page_tags(app_id):
    """
    Try to extract more detailed tag information from the store page.
    
    Args:
        app_id: The Steam App ID
        
    Returns:
        List of tags from the store page
    """
    import re
    
    # Make a request to the store page
    url = f"https://store.steampowered.com/app/{app_id}"
    headers = {
        'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36'
    }
    
    try:
        response = requests.get(url, headers=headers)
        
        # Look for the popular tags section
        if response.status_code == 200:
            # Try to find popular tags
            popular_tags_match = re.search(r'Popular user-defined tags for this product:(.*?)</div>', response.text, re.DOTALL)
            
            if popular_tags_match:
                tags_html = popular_tags_match.group(1)
                # Extract individual tags - they're usually in <a> elements
                tags = re.findall(r'">([^<]+)</a>', tags_html)
                
                # Clean up and return
                return [tag.strip() for tag in tags if tag.strip()]
        
        return []
    except Exception as e:
        print(f"Error fetching store page tags: {e}")
        return []

# Step 3: Get game details including supported languages and release date
def get_game_details(app_id):
    # Use English language and USD currency (US country code)
    url = f"https://store.steampowered.com/api/appdetails"
    params = {
        "appids": app_id,
        "l": "english",
        "cc": "us"
    }
    
    data = cached_api_request(url, params)
    
    # If we got valid data but don't see tags, try the store page method
    if data and str(app_id) in data and data[str(app_id)]['success']:
        app_data = data[str(app_id)]['data']
        
        # If we don't have tags or very few categories, try store page
        if (('tags' not in app_data or not app_data['tags']) and 
            ('categories' not in app_data or len(app_data['categories']) < 3)):
            
            store_tags = get_store_page_tags(app_id)
            
            # If we got store tags, add them to the response
            if store_tags:
                print(f"Added {len(store_tags)} tags from store page")
                if 'tags' not in app_data:
                    app_data['tags'] = {}
                
                # Add as dictionary items (similar to Steam API format)
                for i, tag in enumerate(store_tags):
                    app_data['tags'][str(i)] = tag
    
    return data

# Get game details with Simplified Chinese localisation and CNY pricing
def get_game_details_chinese(app_id):
    url = f"https://store.steampowered.com/api/appdetails"
    params = {
        "appids": app_id,
        "l": "schinese",
        "cc": "cn"
    }
    
    return cached_api_request(url, params)

# Function to check if an app is likely a DLC or non-game
def check_app_relationship(app_id):
    """
    Get app relationship data to determine if an app is a DLC or non-game.
    
    Args:
        app_id: The Steam App ID
        
    Returns:
        Tuple of (is_dlc, reason, details) where:
          - is_dlc: Boolean indicating if app is likely a DLC/non-game
          - reason: String explaining the reason
          - details: The full app details data for reuse in other functions
    """
    # Get the app details
    details = get_game_details(app_id)
    
    if not details or str(app_id) not in details or not details[str(app_id)]['success']:
        return (False, "Could not get app details", None)
    
    app_data = details[str(app_id)]['data']
    reasons = []
    
    # Check 1: Direct type check
    app_type = app_data.get('type', '').lower()
    if app_type != 'game':
        if app_type == 'dlc':
            reasons.append(f"App has type 'dlc'")
            return (True, "; ".join(reasons), details)
        else:
            reasons.append(f"App has type '{app_type}' (not 'game')")
            return (True, "; ".join(reasons), details)
    
    # Check 2: Check for 'fullgame' reference (common in DLCs)
    if 'fullgame' in app_data:
        fullgame_id = app_data['fullgame']['appid']
        fullgame_name = app_data['fullgame']['name']
        reasons.append(f"App is related to full game '{fullgame_name}' (ID: {fullgame_id})")
        return (True, "; ".join(reasons), details)
    
    # Check 3: Check for DLC-like categories
    dlc_categories = ['downloadable content', 'dlc']
    if 'categories' in app_data:
        for category in app_data['categories']:
            if any(dlc_term in category['description'].lower() for dlc_term in dlc_categories):
                reasons.append(f"App has DLC-related category: '{category['description']}'")
                return (True, "; ".join(reasons), details)

    # Check 4: Required app check (DLCs often have required apps)
    if app_data.get('required_age', 0) == 0 and len(app_data.get('developers', [])) == 0:
        if any(pattern in app_data.get('name', '').lower() for pattern in ['dlc', 'pack', 'expansion']):
            reasons.append("App has DLC-like name with no age requirement and no developers")
            return (True, "; ".join(reasons), details)
            
    # If we get here, the app doesn't appear to be a DLC
    return (False, "Appears to be a game based on API relationship data", details)

# Step 1: Get list of all games and filter non-games
def get_all_games():
    url = "https://api.steampowered.com/ISteamApps/GetAppList/v2/"
    params = {}
    
    response = cached_api_request(url, params)
    
    if not response:
        print("Failed to get app list from Steam API")
        return []
    
    all_apps = response['applist']['apps']
    
    print(f"Total apps from Steam API: {len(all_apps)}")
    
    # Track filtering statistics
    keyword_filtered = []
    
    # Advanced regex-based filtering with whitelist protection
    filtered_apps = []
    for app in all_apps:
        app_name = app['name'].lower()
        
        # Skip apps with no name
        if not app_name or app_name == "":
            continue
            
        # Check whitelist first - if in whitelist, keep regardless of other filters
        if is_in_whitelist(app_name):
            filtered_apps.append(app)
            continue
            
        # Check against regex patterns for non-games
        matched_pattern = None
        for pattern in REFINED_NON_GAME_PATTERNS:
            if re.search(pattern, app_name):
                matched_pattern = pattern
                break
                
        if matched_pattern:
            # Log filtered app for analysis
            keyword_filtered.append({
                "app_id": app["appid"],
                "name": app["name"],
                "matched_pattern": matched_pattern
            })
        else:
            filtered_apps.append(app)
    
    # Save filtering logs for analysis
    with open(KEYWORD_LOG_FILE, 'w', encoding='utf-8') as f:
        json.dump(keyword_filtered, f, ensure_ascii=False, indent=2)
        
    print(f"Filtered to {len(filtered_apps)} potential games after name-based filtering")
    print(f"Excluded {len(keyword_filtered)} apps based on name patterns")
    
    return filtered_apps

# Step 2: Get review summary for a specific language
def get_review_summary(app_id, language):
    """
    Get the review summary for an app in a specific language
    Returns a dictionary with total, positive, and negative counts
    """
    url = f"https://store.steampowered.com/appreviews/{app_id}"
    params = {
        "json": 1,
        "language": language,
        "filter": "summary"
    }
    
    data = cached_api_request(url, params)
    
    if not data:
        # Return zeros if we couldn't get data
        return {'total': 0, 'positive': 0, 'negative': 0}
    
    if data.get('success'):
        summary = data.get('query_summary', {})
        return {
            'total': summary.get('total_reviews', 0),
            'positive': summary.get('total_positive', 0),
            'negative': summary.get('total_negative', 0)
        }
    else:
        print(f"API reported failure for {language} review summary: {data}")
        return {'total': 0, 'positive': 0, 'negative': 0}

# Function to get current player count
def get_current_player_count(app_id):
    """
    Get the current number of players for a game using Steam API
    
    Args:
        app_id: The Steam App ID
        
    Returns:
        Current player count as integer, or 0 if unavailable
    """
    url = f"https://api.steampowered.com/ISteamUserStats/GetNumberOfCurrentPlayers/v1/"
    params = {
        "appid": app_id
    }
    
    data = cached_api_request(url, params)
    
    if not data:
        return 0
    
    if data.get('response', {}).get('result') == 1:  # Success code
        return data.get('response', {}).get('player_count', 0)
    else:
        print(f"API error getting player count for {app_id}: {data}")
        return 0

# Function to clean price by removing currency symbols
def clean_price(price_str):
    """
    Remove currency symbols from price strings.
    """
    if price_str == "Free" or price_str == "Unknown":
        return price_str
    
    # Remove currency symbols and any non-numeric characters except decimal point
    cleaned_price = re.sub(r'[^\d.]', '', price_str)
    return cleaned_price

# Updated function to parse localisation details with improved language code mapping
def parse_language_support(supported_languages_str):
    """
    Parse the supported_languages string from Steam API to determine 
    localisation level for each language.
    
    Uses pre-compiled regex patterns for better performance.
    
    Simplified categories:
    - Interface/Subtitles: Basic UI translation, text, and/or subtitles
    - Audio: Has voice acting/audio dubbing
    
    Returns a dictionary with language support details
    """
    if not supported_languages_str:
        return {}
        
    # Dictionary to store language support info
    language_support = {}
    
    # Find languages with audio support by identifying footnote markers
    audio_languages = []
    
    # Use pre-compiled patterns
    # Collect all symbols that indicate audio support
    audio_symbols = set(match.group(1) for match in AUDIO_FOOTNOTE_PATTERN.finditer(supported_languages_str))
    
    # If no explicit audio symbols found but there's an asterisk footnote,
    # assume asterisk means audio (common on Steam)
    if not audio_symbols and '*' in supported_languages_str:
        audio_symbols.add('*')
    
    # Remove HTML tags for processing languages
    clean_str = TAG_PATTERN.sub('', supported_languages_str)
    
    # Split languages by comma
    languages = [lang.strip() for lang in clean_str.split(',')]
    
    # Process each language
    for lang in languages:
        # Store original language string with symbols
        original_lang = lang
        
        # Remove any footnote symbols for the base language name
        base_lang = SYMBOL_PATTERN.sub('', lang).strip()
        
        # All languages listed have Interface/Subtitles support by default
        interface_subtitles = True
        
        # Check if this language has audio support
        audio_support = any(symbol in original_lang for symbol in audio_symbols)
        
        # Store in results
        language_support[base_lang] = {
            'Interface/Subtitles': interface_subtitles,
            'Audio': audio_support
        }
    
    # OPTIMISATION: Enhanced language code mapping
    enhanced_language_support = {}
    for lang, support in language_support.items():
        # Check if this language matches any of our target languages
        for lang_code, names in LANG_CODE_MAP.items():
            if any(name in lang for name in names):
                # Store under the official code as well
                enhanced_language_support[lang_code] = support
                break
    
    # Merge the enhanced mappings back into the original dictionary
    language_support.update(enhanced_language_support)
    
    return language_support

# Process single game (reuses details data from check_app_relationship)
def process_single_game(app_id, app_name):
    """
    Process a single game and collect all relevant data.
    
    Args:
        app_id: Steam App ID
        app_name: Game name
        
    Returns:
        Dictionary with game data or None if game doesn't meet criteria
    """
    print(f"Processing game: {app_name} (App ID: {app_id})")
    
    # 1. First check review count - cheapest way to filter out many games
    all_reviews = get_review_summary(app_id, "all")
    total_review_count = all_reviews['total']
    
    # Debug logging
    print(f"App {app_id} has {total_review_count} total reviews")
    
    # Check review count criteria early
    if total_review_count < 200:
        print(f"Skipping app {app_id}: Not enough reviews")
        return None
    
    # 2. Now check if this is a DLC or non-game using API relationship data
    is_dlc, dlc_reason, details = check_app_relationship(app_id)
    if is_dlc:
        print(f"Skipping app {app_id}: Identified as DLC/non-game - {dlc_reason}")
        # Log the DLC detection for analysis
        try:
            dlc_log = []
            if os.path.exists(DLC_LOG_FILE):
                with open(DLC_LOG_FILE, 'r', encoding='utf-8') as f:
                    dlc_log = json.load(f)
            
            dlc_log.append({
                "app_id": app_id,
                "name": app_name,
                "reason": dlc_reason
            })
            
            with open(DLC_LOG_FILE, 'w', encoding='utf-8') as f:
                json.dump(dlc_log, f, ensure_ascii=False, indent=2)
        except Exception as e:
            print(f"Error logging DLC: {e}")
            
        return None
    
    # 3. Validate and extract data from the details we already have
    if not details or str(app_id) not in details or not details[str(app_id)]['success']:
        print(f"Skipping app {app_id}: Could not get game details")
        return None
    
    data = details[str(app_id)]['data']
    
    # Additional check: Games should have genres and categories
    if 'genres' not in data or len(data.get('genres', [])) == 0:
        print(f"Skipping app {app_id}: No genres")
        return None
    
    # Additional check: Games should have categories
    if 'categories' not in data or len(data.get('categories', [])) == 0:
        print(f"Skipping app {app_id}: No categories")
        return None
    
    # Extract release date
    release_date_str = data.get('release_date', {}).get('date', '')
    
    # Use the unified date parsing function
    release_date = parse_steam_date(release_date_str)
    
    # If we couldn't parse the date, skip
    if not release_date:
        print(f"Skipping app {app_id}: Could not parse release date '{release_date_str}'")
        return None
            
    # Check release date criteria
    if release_date.year < 2010:
        print(f"Skipping app {app_id}: Released before 2010 ({release_date.year})")
        return None
    
    # If we get here, the game meets all basic criteria
    print(f"Game {app_name} meets all criteria. Collecting detailed data...")
    
    # Get name
    name = data.get('name', 'Unknown')
    
    # Now get Chinese details - only for games that passed all other checks
    chinese_name = None
    price_cny = "Unknown"
    chinese_details = get_game_details_chinese(app_id)
    
    if chinese_details and str(app_id) in chinese_details and chinese_details[str(app_id)]['success']:
        chinese_data = chinese_details[str(app_id)]['data']
        chinese_name = chinese_data.get('name')
        
        # Get price in CNY
        is_free = chinese_data.get('is_free', False)
        if is_free:
            price_cny = "Free"
        else:
            chinese_price_overview = chinese_data.get('price_overview', {})
            if chinese_price_overview:
                price_cny = chinese_price_overview.get('final_formatted', 'Unknown')
                # Remove currency symbol from CNY price
                price_cny = clean_price(price_cny)
    
    # Get USA price
    price_overview = data.get('price_overview', {})
    price_usd = price_overview.get('final_formatted', 'Unknown')  # Price in USD
    is_free = data.get('is_free', False)
    if is_free:
        price_usd = "Free"
    else:
        # Remove currency symbol from USD price
        price_usd = clean_price(price_usd)
        
    # Extract genre information
    genres = [genre['description'] for genre in data.get('genres', [])]
    
    # Extract user-defined tags from multiple sources
    user_tags = []
    
    # From categories first
    if 'categories' in data:
        categories = [category['description'] for category in data.get('categories', [])]
        user_tags.extend(categories)
    
    # From tags field which might be in various formats
    if 'tags' in data:
        # If tags is a dictionary (common in newer API responses)
        if isinstance(data['tags'], dict):
            # Some API responses have tags as a dictionary with numeric keys
            user_tags.extend([tag for tag in data['tags'].values()])
        # If tags is a list
        elif isinstance(data['tags'], list):
            user_tags.extend([tag.get('description', tag) if isinstance(tag, dict) else tag for tag in data['tags']])
    
    # Try to get additional tags from the store page as a fallback
    if len(user_tags) < 5:
        store_tags = get_store_page_tags(app_id)
        if store_tags:
            user_tags.extend(store_tags)
    
    # Remove duplicates while preserving order
    seen = set()
    user_tags = [tag for tag in user_tags if not (tag in seen or seen.add(tag))]
    
    print(f"Found {len(user_tags)} user tags: {', '.join(user_tags[:5])}{'...' if len(user_tags) > 5 else ''}")
    
    # Get current player count
    current_players = get_current_player_count(app_id)
    print(f"Current player count: {current_players}")
    
    # Get supported languages and parse with our function
    supported_languages = data.get('supported_languages', '')
    language_support = parse_language_support(supported_languages)
    
    # Language name mapping from Steam format to our code variables
    language_name_mapping = {
        "Simplified Chinese": "chinese_simplified",
        "Traditional Chinese": "chinese_traditional",
        "English": "english"
    }
    
    # Initialise localisation fields for our simplified approach
    localisation = {}
    for lang_key in language_name_mapping.values():
        localisation[f"{lang_key}_interface_subtitles"] = False
        localisation[f"{lang_key}_audio"] = False
    
    # Set localisation details for our target languages
    for steam_lang, support_details in language_support.items():
        # Find matching language code
        for steam_name, lang_code in language_name_mapping.items():
            if steam_name in steam_lang:  # This allows partial matching
                localisation[f"{lang_code}_interface_subtitles"] = support_details['Interface/Subtitles']
                localisation[f"{lang_code}_audio"] = support_details['Audio']
        
        # Check for direct language code matches
        for lang_code in LANG_CODE_MAP:
            if steam_lang.lower() == lang_code:
                # Map from Steam lang code to our internal code
                if lang_code == "schinese":
                    internal_code = "chinese_simplified"
                elif lang_code == "tchinese":
                    internal_code = "chinese_traditional"
                else:
                    internal_code = lang_code
                
                localisation[f"{internal_code}_interface_subtitles"] = support_details['Interface/Subtitles']
                localisation[f"{internal_code}_audio"] = support_details['Audio']
    
    # Print localisation details for verification
    print("Localisation details:")
    for lang_name, lang_code in language_name_mapping.items():
        interface = localisation[f"{lang_code}_interface_subtitles"]
        audio = localisation[f"{lang_code}_audio"]
        print(f"  - {lang_name}: Interface/Subtitles: {interface}, Audio: {audio}")
    
    # Use the global reviews we already retrieved earlier
    review_counts = {
        "total": all_reviews['total'],
        "total_positive": all_reviews['positive'],
        "total_negative": all_reviews['negative']
    }
    
    # Get review counts for each target language
    target_languages = {
        "schinese": "Simplified Chinese",
        "tchinese": "Traditional Chinese", 
        "english": "English"
    }
    
    # Now that we've confirmed the game meets all criteria, get detailed reviews
    sentiment_ratios = {}
    for lang_code in target_languages.keys():
        # Get review summary for this language
        lang_summary = get_review_summary(app_id, lang_code)
        
        # Store the counts
        review_counts[f"{lang_code}_total"] = lang_summary['total']
        review_counts[f"{lang_code}_positive"] = lang_summary['positive']
        review_counts[f"{lang_code}_negative"] = lang_summary['negative']
        
        # Calculate sentiment ratio
        if lang_summary['total'] > 0:
            sentiment_ratios[f"{lang_code}_positive_ratio"] = round(lang_summary['positive'] / lang_summary['total'], 4)
        else:
            sentiment_ratios[f"{lang_code}_positive_ratio"] = 0
            
        print(f"  - {target_languages[lang_code]} reviews: {lang_summary['total']} (Positive: {sentiment_ratios[f'{lang_code}_positive_ratio']*100:.1f}%)")
    
    # Calculate "other languages" total count
    review_counts["other_total"] = max(0, review_counts["total"] - 
                       (review_counts["schinese_total"] + 
                        review_counts["tchinese_total"] + 
                        review_counts["english_total"]))
    
    # Store all the game data in the same format as the main collection function
    game_data = {
        'app_id': app_id,
        'english_name': name,
        'chinese_name': chinese_name,
        'developers': data.get('developers', ['Unknown']),
        'publishers': data.get('publishers', ['Unknown']),
        'release_date': release_date.strftime("%Y-%m-%d") if release_date else None,
        'supported_languages': supported_languages,
        'genres': genres,
        'user_tags': user_tags,  # Added user tags
        'price_usd': price_usd,
        'price_cny': price_cny,
        'is_free': is_free,
        'current_player_count': current_players,
        
        # Localisation status with our simplified approach
        **localisation,
        
        # Global review counts (all languages combined)
        'total_reviews': review_counts["total"],
        'total_positive': review_counts["total_positive"],
        'total_negative': review_counts["total_negative"],
        
        # Simplified Chinese review counts
        'schinese_reviews': review_counts["schinese_total"],
        'schinese_positive': review_counts["schinese_positive"],
        'schinese_negative': review_counts["schinese_negative"],
        'schinese_positive_ratio': sentiment_ratios["schinese_positive_ratio"],
        
        # Traditional Chinese review counts
        'tchinese_reviews': review_counts["tchinese_total"],
        'tchinese_positive': review_counts["tchinese_positive"],
        'tchinese_negative': review_counts["tchinese_negative"],
        'tchinese_positive_ratio': sentiment_ratios["tchinese_positive_ratio"],
        
        # English review counts
        'english_reviews': review_counts["english_total"],
        'english_positive': review_counts["english_positive"],
        'english_negative': review_counts["english_negative"],
        'english_positive_ratio': sentiment_ratios["english_positive_ratio"],
        
        # Other languages - ONLY TOTAL COUNT
        'other_reviews': review_counts["other_total"]
    }
    
    print(f"✅ Successfully collected data for {name}")
    return game_data

# Batch generator for memory-efficient processing
def batch_generator(apps, batch_size=50):
    """Generator function to yield batches of apps to process.
    
    Args:
        apps: List of app dictionaries
        batch_size: Size of each batch
        
    Yields:
        List of apps in the current batch
    """
    for i in range(0, len(apps), batch_size):
        yield apps[i:i+batch_size]

# Parallel Processing for API Requests with persistence of processed_app_ids
def process_games_in_parallel(apps, max_workers=5, batch_size=50, min_workers=2, processed_app_ids=None):
    """
    Process multiple games in parallel using ThreadPoolExecutor with dynamic worker adjustment.
    
    Args:
        apps: List of app dictionaries with 'appid' and 'name' keys
        max_workers: Maximum number of concurrent workers
        batch_size: Process this many games at a time (for checkpointing)
        min_workers: Minimum number of workers to use (even if error rate is high)
        processed_app_ids: Set of already processed app IDs (to avoid duplicates)
        
    Returns:
        Tuple of (results_list, processed_app_ids_set)
    """
    all_results = []
    if processed_app_ids is None:
        processed_app_ids = set()
    
    # Initialise CSV for incremental storage if it doesn't exist
    csv_path = initialise_results_file(CSV_RESULTS_FILE)
    
    # Process in batches to allow for periodic checkpointing
    total_batches = (len(apps) + batch_size - 1) // batch_size
    
    # Start with a conservative number of workers
    current_workers = min(max_workers, min_workers + 1)
    
    # OPTIMISATION: Create a rate limiter for API requests
    rate_limiter = RateLimiter(max_tokens=max_workers)
    
    for batch_num in range(total_batches):
        start_idx = batch_num * batch_size
        end_idx = min((batch_num + 1) * batch_size, len(apps))
        batch = apps[start_idx:end_idx]
        
        print(f"\nProcessing batch {batch_num+1}/{total_batches} ({len(batch)} games) with {current_workers} workers")
        
        batch_results = []
        batch_start_time = time.time()
        batch_errors = 0
        
        with concurrent.futures.ThreadPoolExecutor(max_workers=current_workers) as executor:
            # Create a dictionary to track futures
            future_to_app = {}
            
            # Submit jobs for each app in the batch
            for app in batch:
                app_id = str(app['appid'])
                app_name = app['name']
                
                # Skip if already processed
                if app_id in processed_app_ids:
                    print(f"Skipping {app_name} ({app_id}) - already processed")
                    continue
                
                # OPTIMISATION: Modified to use the rate limiter
                def rate_limited_process():
                    rate_limiter.acquire()
                    try:
                        return process_single_game(app_id, app_name)
                    finally:
                        rate_limiter.release()
                
                # Submit the job with rate limiting
                future = executor.submit(rate_limited_process)
                future_to_app[future] = (app_id, app_name)
            
            # Process results as they complete
            for future in concurrent.futures.as_completed(future_to_app):
                app_id, app_name = future_to_app[future]
                try:
                    result = future.result()
                    processed_app_ids.add(app_id)
                    
                    if result:
                        batch_results.append(result)
                        print(f"Successfully processed {app_name} ({app_id})")
                        
                        # Immediately save to CSV with duplicate detection
                        append_game_to_results(result, csv_path)
                except Exception as e:
                    print(f"Error processing app_id {app_id} ({app_name}): {e}")
                    processed_app_ids.add(app_id)
                    batch_errors += 1
        
        # Add batch results to overall results
        all_results.extend(batch_results)
        
        # Calculate stats for this batch
        batch_duration = time.time() - batch_start_time
        batch_success_count = len(batch_results)
        batch_total_count = len(batch)
        batch_success_rate = batch_success_count / batch_total_count if batch_total_count > 0 else 0
        batch_error_rate = batch_errors / batch_total_count if batch_total_count > 0 else 0
        
        print(f"Batch stats: Success rate: {batch_success_rate:.2f}, Error rate: {batch_error_rate:.2f}, Duration: {batch_duration:.1f}s")
        
        # Adjust number of workers based on performance
        if batch_success_rate > 0.9 and current_workers < max_workers:
            # High success rate, we can add more workers
            current_workers = min(current_workers + 1, max_workers)
            print(f"Increasing to {current_workers} workers due to high success rate")
        elif batch_error_rate > 0.4 and current_workers > min_workers:
            # High error rate, reduce workers to minimise API stress
            current_workers = max(current_workers - 1, min_workers)
            print(f"Decreasing to {current_workers} workers due to high error rate")
        
        # Save checkpoint after each batch
        save_checkpoint(processed_app_ids, end_idx, all_results)
        print(f"Batch {batch_num+1} complete. Processed {len(processed_app_ids)} games, {len(all_results)} met criteria.")
        
    return all_results, processed_app_ids

def press_any_key_to_continue():
    """
    Pause execution and wait for user to press any key before continuing.
    Works cross-platform on Windows, macOS, and Linux.
    """
    print("\n" + "="*80)
    print("TESTING PHASE COMPLETE")
    print("Press any key to begin the main data collection process...")
    print("(Ctrl+C to abort)")
    print("="*80)
    
    try:
        # For Windows
        if os.name == 'nt':
            import msvcrt
            msvcrt.getch()
        # For Unix/Linux/MacOS
        else:
            import sys, tty, termios
            fd = sys.stdin.fileno()
            old_settings = termios.tcgetattr(fd)
            try:
                tty.setraw(sys.stdin.fileno())
                sys.stdin.read(1)
            finally:
                termios.tcsetattr(fd, termios.TCSADRAIN, old_settings)
    except KeyboardInterrupt:
        print("\nProcess aborted by user.")
        sys.exit(0)
    except Exception as e:
        # Fallback method if the above methods fail
        print(f"Note: {e}")
        input("Press Enter to continue...")
    
    print("\nStarting main data collection process...")

# Function to save checkpoint
def save_checkpoint(processed_app_ids, current_index, results):
    checkpoint_data = {
        'processed_app_ids': processed_app_ids,
        'current_index': current_index
    }
    
    with open(CHECKPOINT_FILE, 'wb') as f:
        pickle.dump(checkpoint_data, f)
    
    # Save the current results as well
    with open(PROCESSED_GAMES_FILE, 'w', encoding='utf-8') as f:
        json.dump(results, f, ensure_ascii=False, indent=4)
    
    # Save API key usage stats
    with open(API_USAGE_FILE, 'w', encoding='utf-8') as f:
        json.dump(key_manager.get_usage_stats(), f, ensure_ascii=False, indent=4)
    
    print(f"Checkpoint saved. Processed {len(processed_app_ids)} games, {len(results)} met criteria.")

# Function to load checkpoint
def load_checkpoint():
    # Initialise with empty default values
    processed_app_ids = set()
    current_index = 0
    results = []
    
    # Try to load the main checkpoint
    if os.path.exists(CHECKPOINT_FILE) and os.path.exists(PROCESSED_GAMES_FILE):
        try:
            with open(CHECKPOINT_FILE, 'rb') as f:
                checkpoint_data = pickle.load(f)
            
            with open(PROCESSED_GAMES_FILE, 'r', encoding='utf-8') as f:
                results = json.load(f)
                
            processed_app_ids = checkpoint_data.get('processed_app_ids', set())
            current_index = checkpoint_data.get('current_index', 0)
            
            print(f"Resuming from checkpoint. Already processed {len(processed_app_ids)} games.")
        except Exception as e:
            print(f"Error loading main checkpoint: {e}")
    else:
        print("No valid checkpoint found. Starting from the beginning.")
    
    # Try to load API usage stats
    if os.path.exists(API_USAGE_FILE):
        try:
            with open(API_USAGE_FILE, 'r', encoding='utf-8') as f:
                api_usage_stats = json.load(f)
            
            key_manager.load_usage_stats(api_usage_stats)
            
            # Print current API usage status
            for key, count in key_manager.call_counters.items():
                masked_key = key[:8] + "..." if len(key) > 8 else key
                print(f"  API Key {masked_key}: {count} calls recorded")
            
            print(f"Current active key: {key_manager.get_current_key()[:8]}...")
        except Exception as e:
            print(f"Error loading API usage stats: {e}")
            print("API usage tracking will start fresh.")
    
    return processed_app_ids, current_index, results

# Save data in R-friendly formats
def save_data_for_r(data, base_filename):
    """
    Save data in R-friendly formats with optimised memory usage.
    Preserves list types in Feather format for direct use in R.
    
    Args:
        data: List of dictionaries with game data
        base_filename: Base filename for output files (without extension)
    """
    # Create DataFrame
    df = pd.DataFrame(data)
    
    # Deduplicate based on app_id (keep the last occurrence)
    if 'app_id' in df.columns and not df.empty:
        df['app_id'] = df['app_id'].astype(str)  # Ensure app_id is string for comparison
        df_before_dedup = len(df)
        df = df.drop_duplicates(subset=['app_id'], keep='last')
        df_after_dedup = len(df)
        
        if df_before_dedup > df_after_dedup:
            print(f"Removed {df_before_dedup - df_after_dedup} duplicate entries during final save")
    
    # 1. Save as CSV with UTF-8-BOM encoding for better compatibility with Excel and Chinese characters
    df.to_csv(f"{base_filename}.csv", index=False, encoding='utf-8-sig')
    print(f"Data saved as CSV: {base_filename}.csv")
    
    # 2. Save as Feather format preserving list types
    try:
        # No need to convert list columns to strings - Feather supports lists natively
        # Save directly to feather format
        df.to_feather(f"{base_filename}.feather")
        print(f"Data saved as Feather with native list types: {base_filename}.feather")
        
        # Add note about RDS file
        print(f"Generating RDS file automatically...")
        
        # Create R import script with improved list handling
        with open(f"{base_filename}_import.R", "w", encoding='utf-8') as f:
            f.write(f"""# R script to import Steam data
# Set explicit encoding to UTF-8
Sys.setlocale("LC_ALL", "en_US.UTF-8")
options(encoding = "UTF-8")

# First install required packages if needed
if (!require("arrow")) install.packages("arrow")
if (!require("tidyverse")) install.packages("tidyverse")

# Import from Feather format (preferred method)
library(arrow)
library(tidyverse)

# Import the data - Arrow preserves list columns directly
steam_data <- read_feather("{base_filename}.feather")

# List columns ('genres', 'developers', 'publishers', 'user_tags') are already preserved 
# as native R list columns - no need to split strings

# Save as RDS file (native R format)
saveRDS(steam_data, file = "{base_filename}.rds")
cat("Data saved as RDS file: {base_filename}.rds\\n")

# Display the first few rows
print(head(steam_data))

# Basic summary
print(summary(steam_data))

# Example analysis focusing on Chinese localisation levels
localisation_analysis <- steam_data %>%
  mutate(
    localisation_level = case_when(
      chinese_simplified_interface_subtitles & chinese_simplified_audio ~ "Full Localisation (UI/Text + Audio)",
      chinese_simplified_interface_subtitles ~ "Partial Localisation (UI/Text only)",
      TRUE ~ "No Chinese Localisation"
    )
  ) %>%
  group_by(localisation_level) %>%
  summarize(
    game_count = n(),
    avg_chinese_rating = mean(schinese_positive_ratio, na.rm = TRUE),
    avg_english_rating = mean(english_positive_ratio, na.rm = TRUE),
    avg_sentiment_diff = mean(schinese_positive_ratio - english_positive_ratio, na.rm = TRUE)
  )

print(localisation_analysis)
""")
        
        # Automatically run the R script to generate RDS file
        r_script_path = f"{base_filename}_import.R"
        run_r_script(r_script_path)
        
        print(f"Created R import script: {base_filename}_import.R")
    except Exception as e:
        print(f"Error saving feather format: {e}")
        # If feather fails, at least we have the CSV

# Function to manually process a specific game by app ID
def manually_process_game(app_id, app_name=None, clear_cache=True, verbose=True):
    """
    Manually process a specific game by its Steam app ID and update the CSV file.
    
    Args:
        app_id: The Steam app ID to process
        app_name: Optional name of the game (will be fetched if not provided)
        clear_cache: Whether to clear cached API responses for this app ID
        verbose: Enable verbose logging for troubleshooting
    
    Returns:
        True if successful, False otherwise
    """
    print(f"\n{'='*80}")
    print(f"MANUALLY PROCESSING GAME WITH APP ID: {app_id}")
    print(f"{'='*80}\n")
    
    # Clear cache if requested
    if clear_cache:
        clear_cache_for_app(app_id)
    
    # Enable verbose logging if requested
    if verbose:
        def verbose_log(message):
            print(f"[VERBOSE] {message}")
    else:
        def verbose_log(message):
            pass
    
    # If app_name is not provided, try to fetch it
    if not app_name:
        try:
            # Try to get it from game details
            details = get_game_details(app_id)
            if details and str(app_id) in details and details[str(app_id)]['success']:
                app_name = details[str(app_id)]['data'].get('name', 'Unknown Game')
                print(f"Found game name from details: {app_name}")
            else:
                # Use the GetAppList API to find the game name
                all_apps = get_all_games()
                matching_apps = [app for app in all_apps if str(app['appid']) == str(app_id)]
                
                if matching_apps:
                    app_name = matching_apps[0]['name']
                    print(f"Found game name: {app_name}")
                else:
                    app_name = f"Unknown Game ({app_id})"
                    print(f"Could not find game name, using: {app_name}")
        except Exception as e:
            app_name = f"Unknown Game ({app_id})"
            print(f"Error finding game name: {e}")
    
    # Process the game
    try:
        # Monkey-patch the get_review_summary function if verbose logging is enabled
        if verbose:
            original_get_review_summary = globals()['get_review_summary']
            
            # Create a wrapper function with verbose logging
            def verbose_get_review_summary(app_id, language):
                verbose_log(f"Getting review summary for app {app_id}, language {language}")
                result = original_get_review_summary(app_id, language)
                verbose_log(f"Review summary for {language}: {result}")
                return result
            
            # Replace the original function with our verbose version
            globals()['get_review_summary'] = verbose_get_review_summary
        
        # Process the game
        result = process_single_game(app_id, app_name)
        
        # Restore the original function if we replaced it
        if verbose:
            globals()['get_review_summary'] = original_get_review_summary
        
        if result:
            print(f"\nSuccessfully processed {app_name} ({app_id})")
            
            # Add to CSV with duplicate checking
            append_game_to_results(result, CSV_RESULTS_FILE)
            print(f"CSV file updated with data for {app_name}")
            
            return True
        else:
            print(f"\nFailed to process {app_name} ({app_id})")
            return False
    except Exception as e:
        print(f"\nError processing {app_name} ({app_id}): {e}")
        return False

# Function to clear cache for a specific app ID
def clear_cache_for_app(app_id):
    """
    Clear all cached API responses for a specific app ID.
    
    Args:
        app_id: The Steam app ID to clear cache for
    """
    # Scan all subdirectories in the cache dir
    cleared_files = 0
    for root, dirs, files in os.walk(CACHE_DIR):
        for file in files:
            file_path = os.path.join(root, file)
            try:
                # Read the cached response
                with open(file_path, 'r', encoding='utf-8') as f:
                    cache_data = json.load(f)
                
                # Check if this cache entry contains data for our app ID
                app_id_str = str(app_id)
                if app_id_str in str(cache_data):
                    os.remove(file_path)
                    cleared_files += 1
                    print(f"Cleared cache file: {file_path}")
            except Exception:
                # Skip files that can't be read or parsed
                continue
    
    print(f"Cleared {cleared_files} cache files for app ID {app_id}")

def test_data_collection():
    """
    Test basic functionality of the Steam data collector to ensure everything works.
    Tests API connectivity, data retrieval, and parsing functions.
    
    Returns:
        bool: True if all tests pass, False otherwise
    """
    print("\n" + "="*80)
    print("TESTING STEAM DATA COLLECTOR FUNCTIONALITY")
    print("="*80)
    
    test_passed = True
    
    # Test 1: Check API connectivity with a simple request
    print("\n[TEST 1] Testing API connectivity...")
    try:
        url = "https://api.steampowered.com/ISteamApps/GetAppList/v2/"
        params = {}
        response = cached_api_request(url, params)
        
        if response and 'applist' in response and 'apps' in response['applist']:
            print("✓ Successfully connected to Steam API")
            print(f"✓ Retrieved {len(response['applist']['apps'])} apps")
        else:
            print("✗ Failed to get valid response from Steam API")
            test_passed = False
    except Exception as e:
        print(f"✗ API connection error: {e}")
        test_passed = False
    
    # Test 2: Test getting details for a well-known game
    print("\n[TEST 2] Testing game details retrieval...")
    try:
        test_app_id = "2246340"  # Monster Hunter Wilds
        details = get_game_details(test_app_id)
        
        if details and test_app_id in details and details[test_app_id]['success']:
            game_name = details[test_app_id]['data']['name']
            print(f"✓ Successfully retrieved details for {game_name} (App ID: {test_app_id})")
        else:
            print(f"✗ Failed to get details for test app ID: {test_app_id}")
            test_passed = False
    except Exception as e:
        print(f"✗ Game details error: {e}")
        test_passed = False
    
    # Test 3: Test review summary function
    print("\n[TEST 3] Testing review summary retrieval...")
    try:
        test_app_id = "2246340"  # Monster Hunter Wilds
        reviews = get_review_summary(test_app_id, "english")
        
        if reviews and 'total' in reviews and reviews['total'] > 0:
            print(f"✓ Successfully retrieved reviews: {reviews['total']} total, {reviews['positive']} positive")
        else:
            print(f"✗ Failed to get valid review data for test app ID: {test_app_id}")
            test_passed = False
    except Exception as e:
        print(f"✗ Review summary error: {e}")
        test_passed = False
    
    # Test 4: Test language support parsing
    print("\n[TEST 4] Testing language support parsing...")
    try:
        test_language_str = "English<strong>*</strong>, French<strong>*</strong>, Italian<strong>*</strong>, German<strong>*</strong>, Spanish - Spain<strong>*</strong>, Japanese<strong>*</strong>, Portuguese - Brazil<strong>*</strong>, Russian<strong>*</strong>, Simplified Chinese<strong>*</strong>, Traditional Chinese, Korean<strong>*</strong>"
        language_support = parse_language_support(test_language_str)
        
        if language_support and "English" in language_support and language_support["English"]["Audio"]:
            print(f"✓ Successfully parsed language support data")
            print(f"✓ Found {len(language_support)} supported languages")
            
            # Check Chinese support specifically since it's a focus
            if "Simplified Chinese" in language_support:
                chinese_support = language_support["Simplified Chinese"]
                print(f"✓ Simplified Chinese support: Interface/Subtitles={chinese_support['Interface/Subtitles']}, Audio={chinese_support['Audio']}")
        else:
            print(f"✗ Failed to parse language support correctly")
            test_passed = False
    except Exception as e:
        print(f"✗ Language parsing error: {e}")
        test_passed = False
    
    # Test 5: Test caching mechanism
    print("\n[TEST 5] Testing cache mechanism...")
    try:
        # First request should hit the API or use existing cache
        start_time = time.time()
        first_response = cached_api_request("https://api.steampowered.com/ISteamApps/GetAppList/v2/", {})
        first_request_time = time.time() - start_time
        
        # Second request should use cache and be much faster
        start_time = time.time()
        second_response = cached_api_request("https://api.steampowered.com/ISteamApps/GetAppList/v2/", {})
        second_request_time = time.time() - start_time
        
        if second_request_time < first_request_time / 2:
            print(f"✓ Cache is working correctly")
            print(f"✓ First request: {first_request_time:.2f}s, Second (cached) request: {second_request_time:.2f}s")
        else:
            print(f"? Cache might not be optimal - First request: {first_request_time:.2f}s, Second request: {second_request_time:.2f}s")
            # Not failing the test for this, just a warning
    except Exception as e:
        print(f"✗ Cache testing error: {e}")
        test_passed = False
    
    # Test 6: Test API key rotation
    print("\n[TEST 6] Testing API key rotation...")
    try:
        initial_key = key_manager.get_current_key()
        # Record some API calls to test rotation
        for _ in range(3):
            key_manager.record_api_call()
        
        current_key = key_manager.get_current_key()
        print(f"✓ API key management is working")
        print(f"✓ Current key: {current_key[:8]}... (used {key_manager.call_counters[current_key]} calls)")
    except Exception as e:
        print(f"✗ API key rotation error: {e}")
        test_passed = False
    
    # Test 7: Test CSV deduplication
    print("\n[TEST 7] Testing CSV deduplication...")
    try:
        # Create a test CSV with duplicates
        test_csv = os.path.join(output_dir, "test_dedup.csv")
        test_data = [
            {"app_id": "123", "name": "Game 1", "value": 1},
            {"app_id": "456", "name": "Game 2", "value": 2},
            {"app_id": "123", "name": "Game 1 Updated", "value": 3}, # Duplicate app_id
            {"app_id": "789", "name": "Game 3", "value": 4}
        ]
        pd.DataFrame(test_data).to_csv(test_csv, index=False)
        
        # Run deduplication
        deduplicate_csv_file(test_csv)
        
        # Load the deduplicated file
        deduped_data = pd.read_csv(test_csv)
        
        # Check if deduplication worked
        if len(deduped_data) == 3 and "Game 1 Updated" in deduped_data["name"].values:
            print(f"✓ CSV deduplication is working correctly")
        else:
            print(f"✗ CSV deduplication failed")
            test_passed = False
            
        # Clean up test file
        os.remove(test_csv)
    except Exception as e:
        print(f"✗ CSV deduplication error: {e}")
        test_passed = False
        
    # Test 8: Test user tag extraction
    print("\n[TEST 8] Testing user tag extraction...")
    try:
        # Choose a game with known tags
        test_app_id = "1151640"  # Cyberpunk 2077
        details = get_game_details(test_app_id)
        
        if details and test_app_id in details and details[test_app_id]['success']:
            data = details[test_app_id]['data']
            
            # Extract user tags
            user_tags = []
            
            # From categories first
            if 'categories' in data:
                categories = [category['description'] for category in data.get('categories', [])]
                user_tags.extend(categories)
            
            # From tags field which might be in various formats
            if 'tags' in data:
                # If tags is a dictionary (common in newer API responses)
                if isinstance(data['tags'], dict):
                    # Some API responses have tags as a dictionary with numeric keys
                    user_tags.extend([tag for tag in data['tags'].values()])
                # If tags is a list
                elif isinstance(data['tags'], list):
                    user_tags.extend([tag.get('description', tag) if isinstance(tag, dict) else tag for tag in data['tags']])
            
            # Remove duplicates while preserving order
            seen = set()
            user_tags = [tag for tag in user_tags if not (tag in seen or seen.add(tag))]
            
            if user_tags:
                print(f"✓ Successfully extracted user tags for Cyberpunk 2077")
                print(f"✓ Found {len(user_tags)} tags, including: {', '.join(user_tags[:5])}...")
            else:
                print(f"✗ Failed to extract any user tags")
                # Try store page scraping as fallback
                store_tags = get_store_page_tags(test_app_id)
                if store_tags:
                    print(f"✓ But store page scraping found {len(store_tags)} tags: {', '.join(store_tags[:5])}...")
                else:
                    test_passed = False
        else:
            print(f"✗ Failed to get details for test app ID: {test_app_id}")
            test_passed = False
    except Exception as e:
        print(f"✗ User tag extraction error: {e}")
        test_passed = False
    
    # Final test result
    print("\n" + "="*80)
    if test_passed:
        print("ALL TESTS PASSED! The script appears to be working correctly.")
    else:
        print("SOME TESTS FAILED. Please check the errors above before proceeding.")
    print("="*80 + "\n")
    
    return test_passed

# Main execution block
if __name__ == "__main__":
    import argparse
    
    print("Starting Steam Data Collector...")
    
    # Set up argument parser for command-line options
    parser = argparse.ArgumentParser(description='Steam Data Collector')
    
    # Add command-line arguments
    parser.add_argument('--app-id', type=str, help='Process a specific game by its Steam App ID')
    parser.add_argument('--app-name', type=str, help='Optional name for the game (will be fetched if not provided)')
    parser.add_argument('--keep-cache', action='store_true', help='Keep existing cache for the app ID')
    parser.add_argument('--quiet', action='store_true', help='Disable verbose logging')
    parser.add_argument('--deduplicate', action='store_true', help='Deduplicate the CSV file without processing games')
    
    # Parse arguments
    args = parser.parse_args()
    
    # Handle the deduplicate option first
    if args.deduplicate:
        print("Deduplicating CSV file...")
        if deduplicate_csv_file(CSV_RESULTS_FILE):
            print("Deduplication complete.")
        else:
            print("Deduplication failed or not needed.")
        sys.exit(0)
    
    # Check if we're manually processing a specific game
    if args.app_id:
        # Load API key usage stats if available
        if os.path.exists(API_USAGE_FILE):
            try:
                with open(API_USAGE_FILE, 'r', encoding='utf-8') as f:
                    api_usage_stats = json.load(f)
                key_manager.load_usage_stats(api_usage_stats)
            except Exception as e:
                print(f"Error loading API usage stats: {e}")
        
        # Process the specific game
        manually_process_game(
            args.app_id, 
            app_name=args.app_name,
            clear_cache=not args.keep_cache,
            verbose=not args.quiet
        )
        
        # Save updated API key usage stats
        with open(API_USAGE_FILE, 'w', encoding='utf-8') as f:
            json.dump(key_manager.get_usage_stats(), f, ensure_ascii=False, indent=4)
            
        sys.exit(0)
    
    # Regular execution flow for batch processing
    # Run the test function first to verify everything works
    test_successful = test_data_collection()
    
    if test_successful:
        # Add pause and wait for user confirmation before continuing
        press_any_key_to_continue()
        
        # Deduplicate existing CSV file if it exists
        if os.path.exists(CSV_RESULTS_FILE):
            print("Checking for duplicates in existing CSV file...")
            deduplicate_csv_file(CSV_RESULTS_FILE)
        
        # Load checkpoint if available
        processed_app_ids, current_index, results = load_checkpoint()
        
        # Get all games from Steam
        all_apps = get_all_games()
        
        print(f"Starting to process {len(all_apps)} potential games...")
        
        # OPTIMISATION: Use batch generator for memory efficiency
        accumulated_results = []
        for batch in batch_generator(all_apps[current_index:], batch_size=50):
            batch_results, processed_app_ids = process_games_in_parallel(
                batch, 
                max_workers=5,  # Adjust based on your system and network capabilities
                batch_size=len(batch),  # Process the entire batch
                processed_app_ids=processed_app_ids  # Pass in existing set
            )
            accumulated_results.extend(batch_results)
            
            # Save interim results for extra safety
            output_file = os.path.join(output_dir, "steam_games_data_interim")
            save_data_for_r(accumulated_results, output_file)
        
        # Save final results to CSV/Feather for R
        if accumulated_results:
            output_file = os.path.join(output_dir, "steam_games_data_final")
            save_data_for_r(accumulated_results, output_file)
            
        print("\nData collection complete!")
    else:
        print("\nTest failed. Please fix the issues before running the main collection process.")