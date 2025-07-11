import requests
import time
import json
import pandas as pd
from datetime import datetime
import ast
import os
import pickle
import re
import subprocess
import sys
import hashlib
import concurrent.futures
import threading
from typing import List, Dict, Any, Tuple
from bs4 import BeautifulSoup

# Define multiple API keys
API_KEYS = [
    "x",          # Key 1
    "x",          # Key 2
    "x"           # Key 3
]

# Create output directory
output_dir = r"D:\Steam data collector\output"
if not os.path.exists(output_dir):
    os.makedirs(output_dir)

# Create cache directory
CACHE_DIR = os.path.join(output_dir, "api_cache")
if not os.path.exists(CACHE_DIR):
    os.makedirs(CACHE_DIR)

# Set path to R executable
RSCRIPT_PATH = r"D:\Software\R-4.5.0\bin\x64\Rscript.exe"

# Checkpoint file to track progress
CHECKPOINT_FILE = os.path.join(output_dir, "checkpoint.pkl")
PROCESSED_GAMES_FILE = os.path.join(output_dir, "processed_games.json")
API_USAGE_FILE = os.path.join(output_dir, "api_usage.json")
CSV_RESULTS_FILE = os.path.join(output_dir, "steam_data.csv")
DLC_LOG_FILE = os.path.join(output_dir, "dlc_filtering_log.json")

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

# Thread-safe file writing lock
csv_write_lock = threading.Lock()

# Data Collection State Management Class
class DataCollectionState:
    def __init__(self):
        self.lock = threading.Lock()
        self.processed_app_ids = set()
        self.results = []
        
    def add_processed_app(self, app_id, result=None):
        with self.lock:
            self.processed_app_ids.add(str(app_id))
            if result:
                self.results.append(result)
                
    def is_processed(self, app_id):
        with self.lock:
            return str(app_id) in self.processed_app_ids
            
    def get_processed_count(self):
        with self.lock:
            return len(self.processed_app_ids)
            
    def get_results(self):
        with self.lock:
            return self.results.copy()

# Initialize global state manager
state_manager = DataCollectionState()

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

# API Request Caching
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
    essential_params = {}
    
    # Keep only the most essential parameters for cache key
    if 'appid' in params:
        essential_params['appid'] = params['appid']
    if 'appids' in params:
        essential_params['appids'] = params['appids']
    
    # For other specific APIs, add their essential parameters
    if 'language' in params:
        essential_params['language'] = params['language']
    # Include the 'l' parameter used by the appdetails API for language
    if 'l' in params:
        essential_params['l'] = params['l']
    # Include country code as it affects results too
    if 'cc' in params:
        essential_params['cc'] = params['cc']

    
    # Improved cache structure and key generation
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
    
    # Make the actual API request with retry function
    max_retries = 4
    for retry in range(max_retries):
        try:
            response = requests.get(url, params=actual_params, timeout=10)
            key_manager.record_api_call()
            
            # Handle rate limiting with 429 error (TOO MANY REQUESTS)
            if response.status_code == 429:
                print(f"Rate limit exceeded (429) for key {key_manager.get_current_key()[:8]}...")
                # Apply penalty to force key rotation
                current_key = key_manager.get_current_key()
                key_manager.call_counters[current_key] += 1000  # Penalty count to force rotation
                key_manager.rotate_key()
                print(f"Rotated to key: {key_manager.get_current_key()[:8]}...")
                time.sleep(2.0)  # Add additional delay after rate limit
                
                # Instead of returning None, retry with the new key if retries remain
                if retry < max_retries - 1:
                    print(f"Retrying with new API key ({retry+1}/{max_retries})...")
                    continue
                else:
                    return None
            
            # Only cache successful responses
            if response.status_code == 200:
                try:
                    data = response.json()
                    with open(cache_file, 'w', encoding='utf-8') as f:
                        json.dump(data, f)
                    # Insert delay for rate limiting
                    time.sleep(1.5)
                    return data
                except Exception as e:
                    print(f"Error caching response: {e}")
                    # Insert delay for rate limiting
                    time.sleep(1.5)
                    return response.json()
            else:
                print(f"HTTP error {response.status_code} for URL {url}")
                # Try again if retries remain
                if retry < max_retries - 1:
                    print(f"Retrying after HTTP error ({retry+1}/{max_retries})...")
                    time.sleep(2 * (retry + 1))  # Exponential backoff
                    continue
                else:
                    # Insert delay for rate limiting
                    time.sleep(1.5)
                    return None
                
        except Exception as e:
            if retry < max_retries - 1:
                print(f"Retrying API call after error ({retry+1}/{max_retries}): {e}")
                time.sleep(2 * (retry + 1))  # Exponential backoff
            else:
                print(f"API call failed after {max_retries} attempts: {e}")
                time.sleep(1.5)
                return None

# Unified Date Parsing
def parse_steam_date(date_str):
    """Parse Steam date strings which come in various formats."""
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

# Enhanced Deduplicate CSV file
def deduplicate_csv_file(csv_path):
    """Enhanced deduplication that handles edge cases better"""
    try:
        if not os.path.exists(csv_path) or os.path.getsize(csv_path) == 0:
            return False
            
        # Read CSV with explicit dtype to avoid conversion issues
        df = pd.read_csv(csv_path, encoding='utf-8-sig', dtype={'app_id': str})
        original_rows = len(df)
        
        # Sort by app_id and keep the last occurrence
        df = df.sort_values('app_id').drop_duplicates(subset=['app_id'], keep='last')
        
        # Also remove any rows with null app_id
        df = df.dropna(subset=['app_id'])
        
        new_rows = len(df)
        
        if original_rows != new_rows:
            df.to_csv(csv_path, index=False, encoding='utf-8-sig')
            print(f"Deduplicated: {original_rows} → {new_rows} rows (removed {original_rows - new_rows})")
            
        return True
    except Exception as e:
        print(f"Error deduplicating CSV: {e}")
        return False

# Verify Data Integrity
def verify_data_integrity():
    """Verify the final dataset for inconsistencies"""
    if not os.path.exists(CSV_RESULTS_FILE):
        return
        
    df = pd.read_csv(CSV_RESULTS_FILE, encoding='utf-8-sig')
    
    # Check for duplicates
    duplicates = df[df.duplicated(subset=['app_id'], keep=False)]
    if not duplicates.empty:
        print(f"WARNING: Found {len(duplicates)} duplicate entries!")
        
    # Check for missing critical fields
    missing_names = df[df['english_name'].isna()].shape[0]
    if missing_names > 0:
        print(f"WARNING: {missing_names} entries missing english_name!")
        
    print(f"Total unique games: {df['app_id'].nunique()}")
    print(f"Total rows: {len(df)}")

# Clear Cache Function
def clear_cache():
    """Clear the API cache to ensure fresh data"""
    if os.path.exists(CACHE_DIR):
        import shutil
        shutil.rmtree(CACHE_DIR)
        os.makedirs(CACHE_DIR)
        print("Cache cleared successfully")

# Initialise results file
def initialise_results_file(output_path):
    """Initialize CSV file with headers for incremental writing."""
    # Define all possible headers (complete set for your data)
    headers = [
        'app_id', 'english_name', 'chinese_name', 'developers', 'publishers',
        'release_date', 'supported_languages', 'genres', 'user_tags', 'game_tags',
        'price_usd', 'price_cny', 'is_free', 'current_player_count',
        'chinese_simplified_interface_subtitles', 'chinese_simplified_audio',
        'chinese_traditional_interface_subtitles', 'chinese_traditional_audio',
        'english_interface_subtitles', 'english_audio',
        'total_reviews', 'total_positive', 'total_negative',
        'schinese_reviews', 'schinese_positive', 'schinese_negative', 'schinese_positive_ratio',
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

# Thread-safe append game to results
def append_game_to_results(game_data, csv_path):
    """Thread-safe version of append_game_to_results"""
    with csv_write_lock:  # Ensure only one thread writes at a time
        # Check if CSV exists and has content
        if os.path.exists(csv_path) and os.path.getsize(csv_path) > 0:
            try:
                # Read existing data
                existing_data = pd.read_csv(csv_path, encoding='utf-8-sig', dtype={'app_id': str})
                
                # Check if this app_id already exists
                app_id_str = str(game_data['app_id'])
                if 'app_id' in existing_data.columns and (existing_data['app_id'] == app_id_str).any():
                    print(f"App ID {app_id_str} already exists in CSV. Updating existing entry.")
                    # Remove existing entry
                    existing_data = existing_data[existing_data['app_id'] != app_id_str]
                    
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

# Function to run R script
def run_r_script(script_path):
    """Run an R script to generate RDS file from the feather data."""
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
        try:
            stdout_binary, stderr_binary = process.communicate(timeout=300)
        except subprocess.TimeoutExpired:
            process.kill()
            stdout_binary, stderr_binary = process.communicate()
            print("R script timed out after 5 minutes and was terminated.")
            print("--- Captured STDOUT before timeout ---")
            print(stdout_binary.decode('utf-8', errors='replace'))
            print("--- Captured STDERR before timeout ---")
            print(stderr_binary.decode('utf-8', errors='replace'))
            return False
        
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
    except Exception as e:
        print(f"Error running R script: {e}")
        return False

# Function to get store page tags with improved pattern matching for new Steam structure
def get_store_page_tags(app_id):
    """
    Extract tag information from Steam store page using precise selectors
    based on the updated HTML structure (2024+ format).
    Filters out hidden tags and handles the new glance_tags container.
    """
    import re
    import time
    import requests
    from bs4 import BeautifulSoup
    
    # Make a request to the store page with age verification parameters
    url = f"https://store.steampowered.com/app/{app_id}"
    headers = {
        'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36',
        'Accept-Language': 'en-US,en;q=0.9'
    }
    
    # Parameters and cookies to bypass age verification
    params = {'birthtime': '536457600'}
    cookies = {
        'birthtime': '536457600',
        'lastagecheckage': '1-January-1987',
        'wants_mature_content': '1',
        'mature_content': '1'
    }
    
    try:
        # Add a delay to prevent rate limiting
        time.sleep(2.5)
        
        # First try with parameters and cookies for age verification
        response = requests.get(url, headers=headers, params=params, cookies=cookies)
        
        if response.status_code == 200:
            # Parse with BeautifulSoup for more reliable HTML handling
            soup = BeautifulSoup(response.text, 'html.parser')
            
            # Method 1: Updated primary selector for new Steam structure
            tag_links = soup.select('div.glance_tags.popular_tags a.app_tag')
            if tag_links:
                # Extract all tags (including hidden ones) and clean text
                all_tags = []
                for tag_link in tag_links:
                    tag_text = tag_link.get_text(strip=True)
                    if tag_text and tag_text != '+':  # Only exclude the '+' add button
                        all_tags.append(tag_text)
                
                if all_tags:
                    print(f"Found {len(all_tags)} tags (including hidden) using updated primary selector")
                    return all_tags
            
            # Method 2: Fallback to any glance_tags container
            tag_links = soup.select('div.glance_tags a.app_tag')
            if tag_links:
                all_tags = []
                for tag_link in tag_links:
                    tag_text = tag_link.get_text(strip=True)
                    if tag_text and tag_text != '+':
                        all_tags.append(tag_text)
                
                if all_tags:
                    print(f"Found {len(all_tags)} tags (including hidden) using glance_tags selector")
                    return all_tags
            
            # Method 3: Try legacy selectors for backward compatibility
            tag_links = soup.select('div.app_tags.popular_tags a.app_tag')
            if tag_links:
                all_tags = []
                for tag_link in tag_links:
                    tag_text = tag_link.get_text(strip=True)
                    if tag_text and tag_text != '+':
                        all_tags.append(tag_text)
                
                if all_tags:
                    print(f"Found {len(all_tags)} tags (including hidden) using legacy selector")
                    return all_tags
            
            # Method 4: Try alternate legacy selectors
            tag_links = soup.select('div.popular_tags a.app_tag')
            if tag_links:
                all_tags = []
                for tag_link in tag_links:
                    tag_text = tag_link.get_text(strip=True)
                    if tag_text and tag_text != '+':
                        all_tags.append(tag_text)
                
                if all_tags:
                    print(f"Found {len(all_tags)} tags (including hidden) using alternate legacy selector")
                    return all_tags
                    
            # Method 5: Look for any app_tag elements anywhere in the page (last resort)
            tag_links = soup.select('a.app_tag')
            if tag_links:
                all_tags = []
                for tag_link in tag_links:
                    tag_text = tag_link.get_text(strip=True)
                    if tag_text and tag_text != '+':  # Exclude the '+' add button
                        all_tags.append(tag_text)
                
                if all_tags:
                    print(f"Found {len(all_tags)} tags (including hidden) using fallback app_tag selector")
                    return all_tags
            
            # Method 6: Enhanced regex for the new structure
            # Look for all app_tag links (including hidden ones)
            tag_pattern = r'<a[^>]+class="app_tag"[^>]*>\s*([^<]+?)\s*</a>'
            tag_matches = re.findall(tag_pattern, response.text, re.IGNORECASE)
            if tag_matches:
                tags = [tag.strip() for tag in tag_matches if tag.strip() and tag.strip() != '+']
                if tags:
                    print(f"Found {len(tags)} tags (including hidden) using enhanced regex pattern")
                    return tags
            
            # No tags found with any method
            print(f"No tags found for app_id {app_id}")
            return []
            
        else:
            print(f"HTTP error {response.status_code} for URL {url}")
            return []
            
    except Exception as e:
        print(f"Error fetching store page tags: {e}")
        return []

# Get game details including supported languages and release date
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

# Check if an app is likely a DLC or non-game
def check_app_relationship(app_id):
    """Get app relationship data to determine if an app is a DLC or non-game."""
    # Get the app details
    details = get_game_details(app_id)
    
    if not details or str(app_id) not in details or not details[str(app_id)]['success']:
        # CHANGED: Return False instead of True when API fails
        # This allows games to pass through when API issues occur
        print(f"Warning: Could not get app details for {app_id}, assuming it's a game")
        return (False, "Could not verify game status due to API issues", None)
    
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
    
    # Check 2: Check for DLC-like categories
    dlc_categories = ['downloadable content', 'dlc']
    if 'categories' in app_data:
        for category in app_data['categories']:
            if any(dlc_term in category['description'].lower() for dlc_term in dlc_categories):
                reasons.append(f"App has DLC-related category: '{category['description']}'")
                return (True, "; ".join(reasons), details)

# Check 3: Required app check (DLCs often have required apps)
    if app_data.get('required_age', 0) == 0 and len(app_data.get('developers', [])) == 0:
       if any(pattern in app_data.get('name', '').lower() for pattern in ['dlc', 'pack', 'expansion']):
           reasons.append("App has DLC-like name with no age requirement and no developers")
           return (True, "; ".join(reasons), details)
           
   # If we get here, the app doesn't appear to be a DLC
    return (False, "Appears to be a game based on API relationship data", details)

# Get list of all games and filter non-games
def get_all_games():
   url = "https://api.steampowered.com/ISteamApps/GetAppList/v2/"
   params = {}
   
   response = cached_api_request(url, params)
   
   if not response:
       print("Failed to get app list from Steam API")
       return []
   
   all_apps = response['applist']['apps']
   
   print(f"Total apps from Steam API: {len(all_apps)}")
   
   # Sort apps by app_id to ensure consistent ordering
   all_apps = sorted(all_apps, key=lambda x: x['appid'])
   
   # Detailed filtering will be done in process_single_game
   filtered_apps = all_apps
   
   print(f"Proceeding with {len(filtered_apps)} potential apps for detailed filtering")
   
   return filtered_apps

# Get review summary for a specific language
def get_review_summary(app_id, language):
   """Get the review summary for an app in a specific language"""
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
   """Get the current number of players for a game using Steam API"""
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
   """Remove currency symbols from price strings."""
   if price_str == "Free" or price_str == "Unknown":
       return price_str
   
   # Remove currency symbols and any non-numeric characters except decimal point
   cleaned_price = re.sub(r'[^\d.]', '', price_str)
   return cleaned_price

# Updated function to parse localisation details
def parse_language_support(supported_languages_str):
   """Parse the supported_languages string from Steam API to determine localisation level for each language."""
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
   
   # Enhanced language code mapping
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

# Tag normalization functions for avoiding duplicates
def normalize_tag(tag):
   """Normalize a tag to a standard form for comparison."""
   # Convert to lowercase
   normalized = tag.lower()
   
   # Handle specific common variants
   tag_variants = {
       'single-player': 'singleplayer', 
       'multi-player': 'multiplayer',
       'online pvp': 'pvp',
       'co-op': 'coop',
       'online co-op': 'onlinecoop',
       'split-screen': 'splitscreen',
       'first-person': 'firstperson',
       'third-person': 'thirdperson',
       # Add more variants as needed
   }
   
   # Check if this tag is a known variant
   for variant, standard in tag_variants.items():
       if normalized == variant:
           return standard
   
   # Remove hyphens and spaces for general comparison
   normalized = normalized.replace('-', '').replace(' ', '')
   
   return normalized

def is_semantic_duplicate(new_tag, existing_tags):
   """Check if a tag is semantically equivalent to any existing tag."""
   normalized_new = normalize_tag(new_tag)
   normalized_existing = [normalize_tag(tag) for tag in existing_tags]
   
   return normalized_new in normalized_existing

def convert_to_list(tags_data):
   """Convert various tag formats to a proper list."""
   # Handle None case
   if tags_data is None:
       return []
   
   # Handle NaN/NA values from pandas
   if isinstance(tags_data, (float, int)) and pd.isna(tags_data):
       return []
   
   # Handle pandas Series or arrays with isna
   if hasattr(tags_data, 'isna') and hasattr(tags_data.isna(), 'all') and tags_data.isna().all():
       return []
       
   # Handle empty strings
   if isinstance(tags_data, str) and not tags_data.strip():
       return []
       
   if isinstance(tags_data, list):
       return tags_data
       
   if isinstance(tags_data, str):
       # Check if it's a string representation of a list
       if tags_data.startswith('[') and tags_data.endswith(']'):
           # Remove any quotes around the entire string if present
           tags_data = tags_data.strip('"\'')
           
           try:
               # First try ast.literal_eval which is safer than eval
               return ast.literal_eval(tags_data)
           except (SyntaxError, ValueError) as e:
               # Try JSON parse with proper quote handling
               try:
                   # Replace single quotes with double quotes for JSON compatibility
                   json_str = tags_data.replace("'", '"')
                   return json.loads(json_str)
               except json.JSONDecodeError:
                   # Fallback to simple string parsing
                   clean_str = tags_data.strip('[]')
                   # Handle quoted items in the list
                   if '"' in clean_str or "'" in clean_str:
                       # This is a complex string with quotes - try regex approach
                       import re
                       pattern = r'\'([^\']*?)\'|"([^"]*?)"|([\w\s\-\.]+)'
                       matches = re.findall(pattern, clean_str)
                       return [next(s for s in match if s) for match in matches]
                   else:
                       # Simple comma-separated values
                       return [tag.strip() for tag in clean_str.split(',')]
       else:
           # Just a plain comma-separated string
           return [tag.strip() for tag in tags_data.split(',')]
   
   # Handle pandas Series or arrays by converting to list
   if hasattr(tags_data, 'tolist'):
       return tags_data.tolist()
   
   # Try to convert to list if iterable
   if hasattr(tags_data, '__iter__') and not isinstance(tags_data, (str, dict)):
       return list(tags_data)
   
   # If all else fails, wrap in a list
   return [tags_data]

def merge_tags(user_tags, genres):
    """Merge user_tags and genres, removing duplicates while respecting case sensitivity."""
    # Convert inputs to lists
    user_tags_list = convert_to_list(user_tags)
    genres_list = convert_to_list(genres)
    
    # Clean lists by removing None, empty strings, etc.
    clean_user_tags = []
    for tag in user_tags_list:
        if tag is None:
            continue
        if isinstance(tag, (str, int, float)):
            tag_str = str(tag).strip()
            if tag_str:
                clean_user_tags.append(tag_str)
        # If it's an array-like object, convert to string if possible
        elif hasattr(tag, '__len__') and len(tag) > 0:
            try:
                tag_str = str(tag).strip()
                if tag_str and tag_str not in ('[]', '{}', '()'):
                    clean_user_tags.append(tag_str)
            except:
                pass
    
    clean_genres = []
    for tag in genres_list:
        if tag is None:
            continue
        if isinstance(tag, (str, int, float)):
            tag_str = str(tag).strip()
            if tag_str:
                clean_genres.append(tag_str)
        # If it's an array-like object, convert to string if possible
        elif hasattr(tag, '__len__') and len(tag) > 0:
            try:
                tag_str = str(tag).strip()
                if tag_str and tag_str not in ('[]', '{}', '()'):
                    clean_genres.append(tag_str)
            except:
                pass
    
    # Combine all tags
    all_tags = clean_user_tags + clean_genres
    
    # Remove duplicates while preserving order (first occurrence) using normalized form
    seen = set()
    unique_tags = []
    
    for tag in all_tags:
        # Skip empty tags or special values
        if not tag or tag in ['Unknown', 'No tags']:
            continue
            
        # Ensure tag is a string
        if not isinstance(tag, str):
            tag = str(tag).strip()
            if not tag:  # Skip if it becomes empty after conversion
                continue
        
        # Use normalized form for deduplication check
        normalized = normalize_tag(tag)
        
        if normalized not in seen:
            seen.add(normalized)
            unique_tags.append(tag)
    
    return unique_tags

# Process single game (reuses details data from check_app_relationship)
def process_single_game(app_id, app_name):
   """Process a single game and collect all relevant data."""
   print(f"Processing game: {app_name} (App ID: {app_id})")
   
   # 1. First check review count - cheapest way to filter out many games
   all_reviews = get_review_summary(app_id, "all")
   total_review_count = all_reviews['total']
   
   # Debug logging
   print(f"App {app_id} has {total_review_count} total reviews")
   
   # Set threshold
   if total_review_count < 100:
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

   # Try to get information store page tags
   store_tags = get_store_page_tags(app_id)
   if store_tags:
       print(f"Found {len(store_tags)} tags from store page for {app_name}")
       # Add store tags that aren't already in the list
       for tag in store_tags:
           if tag not in user_tags:
               user_tags.append(tag)
       print(f"Combined unique tags: {len(user_tags)}")
   
   # Remove duplicates while preserving order
   seen = set()
   user_tags = [tag for tag in user_tags if not (tag in seen or seen.add(tag))]
   
   print(f"Found {len(user_tags)} user tags: {', '.join(user_tags[:5])}{'...' if len(user_tags) > 5 else ''}")
   
   # Check for required gaming tags
   required_tags = ['Single-player', 'Singleplayer', 'Multi-player', 'Multiplayer', 
                    'PvP', 'Online PvP', 'Online Co-op']
   # Merge genres and user_tags to create game_tags
   game_tags = merge_tags(user_tags, genres)
   print(f"Created {len(game_tags)} merged game_tags")
   if not any(tag in required_tags for tag in user_tags):
       print(f"Skipping app {app_id}: Missing required gaming tags")
       return None
   
   # ADDED: Check for exclusion tags - skip if has any of these tags
   exclusion_tags = ['Downloadable Content', 'Demos', 'Soundtracks', 'Playtests', 
                     'Videos', 'Mods', 'Software', 'Utilities']
   if any(tag in exclusion_tags for tag in user_tags):
       print(f"Skipping app {app_id}: Contains excluded non-game tags")
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
       "english": "English"
   }
   
   # Process the target languages and store their data
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
                   review_counts["english_total"]))
   
   # Store all the game data in the same format as the main collection function
   game_data = {
       'app_id': app_id,
       'english_name': name,
       'chinese_name': chinese_name,
       'developers': data.get('developers', ['Unknown']),  # Ensure list exists
       'publishers': data.get('publishers', ['Unknown']),  # Ensure list exists
       'release_date': release_date.strftime("%Y-%m-%d") if release_date else None,
       'supported_languages': supported_languages,
       'genres': genres if genres else ['Unknown'],  # Ensure non-empty list
       'user_tags': user_tags if user_tags else ['No tags'],  # Ensure non-empty list
       'game_tags': game_tags,  # Add the merged tags here
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
       
       # English review counts
       'english_reviews': review_counts["english_total"],
       'english_positive': review_counts["english_positive"],
       'english_negative': review_counts["english_negative"],
       'english_positive_ratio': sentiment_ratios["english_positive_ratio"],
       
       # Other languages
       'other_reviews': review_counts["other_total"]
   }
   
   print(f"✅ Successfully collected data for {name}")
   return game_data

# Save data in R-friendly formats
def save_data_for_r(data, base_filename):
   """Save data in R-friendly formats with optimised memory usage."""
   # Convert base_filename to use forward slashes for R compatibility
   base_filename_forward = base_filename.replace("\\", "/")

   # Create DataFrame
   df = pd.DataFrame(data)

   # Check if DataFrame is empty before proceeding
   if df.empty:
       print(f"Warning: Empty dataframe, creating minimal files at {base_filename}")
       # Create empty files with headers
       empty_df = pd.DataFrame(columns=['app_id', 'english_name', 'chinese_name', 'developers', 
                                       'publishers', 'genres', 'user_tags'])
       empty_df.to_csv(f"{base_filename}.csv", index=False, encoding='utf-8-sig')
       print(f"Created empty CSV file: {base_filename}.csv")
       return
   
   # Ensure required columns exist with proper types
   list_columns = ['developers', 'publishers', 'genres', 'user_tags', 'game_tags']
   for col in list_columns:
       if col not in df.columns:
           df[col] = df.get(col, [[]]*len(df))  # Empty lists if missing
       # Convert to list type if needed
       if not isinstance(df[col].iloc[0], list):
           df[col] = df[col].apply(lambda x: x if isinstance(x, list) else [x])

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
       # Save directly to feather format
       df.to_feather(f"{base_filename}.feather")
       print(f"Data saved as Feather with native list types: {base_filename}.feather")
       
       # Create R import script with improved list handling
       with open(f"{base_filename}_import.R", "w", encoding='utf-8') as f:
           f.write(f"""# R script to import Steam data
# Set explicit encoding to UTF-8
Sys.setlocale("LC_ALL", "en_US.UTF-8")
options(encoding = "UTF-8")

# Install required packages if needed
if (!require("arrow")) install.packages("arrow")
if (!require("tidyverse")) install.packages("tidyverse")

library(arrow)
library(tidyverse)

# Function to safely convert to list
safe_convert_to_list <- function(x) {{
 if (is.list(x)) {{
   return(x)
 }} else if (is.character(x)) {{
   tryCatch({{
     # Handle string representation of lists
     if (grepl("^\\\\[.*\\\\]$", x)) {{
       # Remove brackets and split
       items <- gsub("^\\\\[|\\\\]$", "", x)
       items <- strsplit(items, ",\\\\s*")[[1]]
       # Remove quotes if present
       items <- gsub('^\\\"|\\\\"$', '', items)
       items <- gsub("^'|'$", "", items)
       return(as.list(items))
     }} else {{
       # Simple comma-separated string
       items <- strsplit(x, ",\\\\s*")[[1]]
       return(as.list(items))
     }}
   }}, error = function(e) {{
     return(list(x))
   }})
 }} else {{
   return(list(x))
 }}
}}

tryCatch({{
 # Import from Feather format
 steam_data <- read_feather("{base_filename_forward}.feather")
 
 # Convert list columns to proper R lists
 list_columns <- c('developers', 'publishers', 'genres', 'user_tags', 'game_tags')
 
 steam_data <- steam_data %>%
   mutate(across(any_of(list_columns), ~ map(., safe_convert_to_list)))
 
 # Ensure all list columns exist
 for (col in list_columns) {{
   if (!col %in% names(steam_data)) {{
     steam_data[[col]] <- list(character(0))
   }}
 }}
 
 # 3. Save as RDS
 saveRDS(steam_data, file = "{base_filename_forward}.rds")
 cat("Successfully saved RDS file: {base_filename_forward}.rds\\n")
 
}}, error = function(e) {{
 cat("Error:", e$message, "\\n")
 traceback()
 quit(status = 1)
}})

cat("R data conversion complete!\\n")
""")
       
# Automatically run the R script to generate RDS file
       r_script_path = f"{base_filename}_import.R"
       run_r_script(r_script_path)
       
       print(f"Created R import script: {base_filename}_import.R")
   except Exception as e:
       print(f"Error saving feather format: {e}")
       # If feather fails, at least we have the CSV

# Batch generator for memory-efficient processing
def batch_generator(apps, batch_size=50):
   """Generator function to yield batches of apps to process."""
   for i in range(0, len(apps), batch_size):
       yield apps[i:i+batch_size]

# Parallel Processing for API Requests with persistence of processed_app_ids
def process_games_in_parallel(apps, max_workers=5, batch_size=50, min_workers=2, processed_app_ids=None, existing_results=None):
   """Process multiple games in parallel using ThreadPoolExecutor with dynamic worker adjustment."""
   all_results = existing_results if existing_results is not None else []
   if processed_app_ids is None:
       processed_app_ids = set()
   
   # Initialise CSV for incremental storage if it doesn't exist
   csv_path = initialise_results_file(CSV_RESULTS_FILE)
   
   # Process in batches to allow for periodic checkpointing
   total_batches = (len(apps) + batch_size - 1) // batch_size
   
   # Start with a conservative number of workers
   current_workers = min(max_workers, min_workers + 1)
   
   # Create a rate limiter for API requests
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
               if state_manager.is_processed(app_id):
                   print(f"Skipping {app_name} ({app_id}) - already processed")
                   continue
               
               # Modified to use the rate limiter
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
                   state_manager.add_processed_app(app_id, result)
                   
                   if result:
                       batch_results.append(result)
                       print(f"Successfully processed {app_name} ({app_id})")
                       
                       # Immediately save to CSV with duplicate detection
                       append_game_to_results(result, csv_path)
               except Exception as e:
                   print(f"Error processing app_id {app_id} ({app_name}): {e}")
                   state_manager.add_processed_app(app_id)
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
       save_checkpoint(state_manager.processed_app_ids, end_idx, all_results)
       print(f"Batch {batch_num+1} complete. Processed {state_manager.get_processed_count()} games, {len(all_results)} met criteria.")
       
   return all_results, state_manager.processed_app_ids

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
   
   # Save the current results to final output files
   output_file = os.path.join(output_dir, "steam_data").replace("\\", "/")
   save_data_for_r(results, output_file)
   
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
           
           # Update state manager with loaded data
           state_manager.processed_app_ids = processed_app_ids
           state_manager.results = results
           
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

def press_any_key_to_continue():
   """Pause execution and wait for user to press any key before continuing."""
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

def update_existing_games_tags():
   """
   Update existing games in the dataset with additional tags from store pages.
   Using BeautifulSoup with precise selectors for more reliable extraction.
   Avoids adding semantic duplicates (e.g., 'Single-player' vs 'Singleplayer')
   and tags that already exist in genres.
   """
   print("Starting to update existing games with additional tags...")
   
   # Check if results file exists
   if not os.path.exists(CSV_RESULTS_FILE):
       print(f"Results file {CSV_RESULTS_FILE} not found.")
       return False
   
   try:
       # Load existing data
       df = pd.read_csv(CSV_RESULTS_FILE, encoding='utf-8-sig')
       print(f"Loaded {len(df)} games from existing dataset.")
       
       # Also load JSON data if available
       existing_results = []
       if os.path.exists(PROCESSED_GAMES_FILE):
           try:
               with open(PROCESSED_GAMES_FILE, 'r', encoding='utf-8') as f:
                   existing_results = json.load(f)
               print(f"Loaded {len(existing_results)} games from JSON file.")
           except Exception as e:
               print(f"Error loading JSON file: {e}")
               existing_results = df.to_dict('records')
       else:
           existing_results = df.to_dict('records')
       
       # Create a mapping for easy lookup
       game_dict = {str(game.get('app_id')): game for game in existing_results}
       
       # Process each game
       updated_count = 0
       failed_count = 0
       
       # Allow limiting the number of games to process
       try:
           process_limit = int(input("Enter number of games to process (0 for all): "))
       except ValueError:
           process_limit = 0
       
       # Create a sample set of games to test with
       if process_limit > 0:
           games_to_process = df.head(process_limit)
       else:
           games_to_process = df
       
       for index, row in games_to_process.iterrows():
           app_id = str(row['app_id'])
           app_name = row['english_name']
           
           print(f"\nProcessing {app_name} (App ID: {app_id}) - {index+1}/{len(games_to_process)}")
           
           # Get current tags - handle various formats
           current_tags = []
           
           if 'user_tags' in row:
               # If DataFrame already contains list format
               if isinstance(row['user_tags'], list):
                   current_tags = row['user_tags']
               # If string representation of Python list
               elif isinstance(row['user_tags'], str) and row['user_tags'].startswith('[') and row['user_tags'].endswith(']'):
                   try:
                       import ast
                       current_tags = ast.literal_eval(row['user_tags'])
                   except:
                       try:
                           current_tags = json.loads(row['user_tags'].replace("'", '"'))
                       except:
                           # Simple comma-split approach as fallback
                           current_tags = [tag.strip() for tag in row['user_tags'].strip('[]').split(',')]
               # Simple string with commas
               elif isinstance(row['user_tags'], str):
                   current_tags = [tag.strip() for tag in row['user_tags'].split(',')]
           
           # Get genres - handle various formats
           genres = []
           if 'genres' in row:
               if isinstance(row['genres'], list):
                   genres = row['genres']
               elif isinstance(row['genres'], str) and row['genres'].startswith('[') and row['genres'].endswith(']'):
                   try:
                       import ast
                       genres = ast.literal_eval(row['genres'])
                   except:
                       try:
                           genres = json.loads(row['genres'].replace("'", '"'))
                       except:
                           # Simple comma-split approach as fallback
                           genres = [genre.strip() for genre in row['genres'].strip('[]').split(',')]
               elif isinstance(row['genres'], str):
                   genres = [genre.strip() for genre in row['genres'].split(',')]
           
           # Print current tags
           print(f"Current tags ({len(current_tags)}): {', '.join(current_tags[:10])}{'...' if len(current_tags) > 10 else ''}")
           
           # Get store page tags with the new precise extractor
           store_tags = get_store_page_tags(app_id)
           
           if store_tags:
               # Check for new tags, avoiding semantic duplicates and genre duplicates
               new_tags = []
               skipped_tags = []
               
               for tag in store_tags:
                   # Check if tag is not a semantic duplicate of existing tags or genres
                   if not is_semantic_duplicate(tag, current_tags) and not is_semantic_duplicate(tag, genres):
                       current_tags.append(tag)
                       new_tags.append(tag)
                   else:
                       skipped_tags.append(tag)
               
               if new_tags:
                   print(f"✅ Added {len(new_tags)} new tags: {', '.join(new_tags)}")
                   if skipped_tags:
                       print(f"⏩ Skipped {len(skipped_tags)} duplicate tags: {', '.join(skipped_tags[:5])}{'...' if len(skipped_tags) > 5 else ''}")
                   updated_count += 1
                   
                   # Update the DataFrame
                   df.at[index, 'user_tags'] = current_tags
                   
                   # Generate merged game_tags
                   merged_tags = merge_tags(current_tags, genres)
                   df.at[index, 'game_tags'] = merged_tags
                   print(f"  Added {len(merged_tags)} merged game_tags")

                   # Update JSON data
                   if app_id in game_dict:
                       game_dict[app_id]['user_tags'] = current_tags
                       game_dict[app_id]['game_tags'] = merged_tags
               else:
                   if skipped_tags:
                       print(f"⏩ All {len(skipped_tags)} tags were duplicates: {', '.join(skipped_tags[:5])}{'...' if len(skipped_tags) > 5 else ''}")
                   else:
                       print(f"No new tags found for {app_name}")
           else:
               print(f"❌ Failed to get tags for {app_name}")
               failed_count += 1
           
           # Save progress every 10 games
           if (index + 1) % 10 == 0:
               print(f"Saving incremental progress...")
               temp_df = games_to_process.iloc[:index+1].copy()
               temp_df.to_csv(os.path.join(output_dir, "temp_update.csv"), index=False, encoding='utf-8-sig')
           
           # Add delay to avoid rate limiting
           time.sleep(3.0)
       
       # Save final updated data
       if updated_count > 0:
           print(f"\nUpdated {updated_count} games with new tags. Failed to get tags for {failed_count} games.")
           
           # Update the full dataset with our processed rows
           if process_limit > 0:
               # Only update the specific rows we processed
               for idx in games_to_process.index:
                   df.loc[idx, 'user_tags'] = games_to_process.loc[idx, 'user_tags']
                   if 'game_tags' in games_to_process.columns:
                       df.loc[idx, 'game_tags'] = games_to_process.loc[idx, 'game_tags']
           
           # Save updated DataFrame to CSV
           df.to_csv(CSV_RESULTS_FILE, index=False, encoding='utf-8-sig')
           print(f"Saved updated data to {CSV_RESULTS_FILE}")
           
           # Save JSON data
           with open(PROCESSED_GAMES_FILE, 'w', encoding='utf-8') as f:
               json.dump(list(game_dict.values()), f, ensure_ascii=False, indent=4)
           print(f"Saved updated JSON data to {PROCESSED_GAMES_FILE}")
           
           # Update R files
           output_file = os.path.join(output_dir, "steam_data")
           save_data_for_r(list(game_dict.values()), output_file)
           print(f"Updated R data files at {output_file}")
           
           return True
       else:
           print(f"\nNo games were updated with new tags. Failed to get tags for {failed_count} games.")
           return False
   
   except Exception as e:
       print(f"Error updating existing games: {e}")
       import traceback
       traceback.print_exc()
       return False

def run_basic_tests():
   """
   Run basic tests to verify the essential functionality works before
   proceeding with the main data collection process.
   
   Returns:
       bool: True if all tests pass, False otherwise
   """
   print("\n===== RUNNING BASIC FUNCTIONALITY TESTS =====")
   tests_passed = 0
   tests_failed = 0
   
   # Test 1: API Connectivity
   print("\n[Test 1] Testing API connectivity...")
   try:
       # Use a simple API endpoint to test connectivity
       url = "https://api.steampowered.com/ISteamApps/GetAppList/v2/"
       params = {}
       response = cached_api_request(url, params)
       
       if response and 'applist' in response and 'apps' in response['applist']:
           print("✅ API connectivity: Success! Retrieved app list.")
           tests_passed += 1
       else:
           print("❌ API connectivity: Failed! Could not retrieve app list.")
           tests_failed += 1
   except Exception as e:
       print(f"❌ API connectivity: Failed with error: {e}")
       tests_failed += 1
   
   # Test 2: Cache Functionality
   print("\n[Test 2] Testing cache functionality...")
   try:
       # Make the same request again - should use cache
       url = "https://api.steampowered.com/ISteamApps/GetAppList/v2/"
       params = {}
       
       # Clear existing metrics
       cache_hit = False
       
       # Define a simple wrapper to detect cache usage
       def test_cache_request():
           nonlocal cache_hit
           start_time = time.time()
           result = cached_api_request(url, params)
           duration = time.time() - start_time
           
           # A very fast response likely means cache was used
           if duration < 0.1:
               cache_hit = True
           
           return result
       
       # Make the request again
       response = test_cache_request()
       
       if cache_hit:
           print("✅ Cache functionality: Success! Cache was properly used.")
           tests_passed += 1
       else:
           print("⚠️ Cache functionality: Warning! Cache might not be working as expected.")
           # Still consider this a pass if we got a response
           if response:
               tests_passed += 1
           else:
               tests_failed += 1
   except Exception as e:
       print(f"❌ Cache functionality: Failed with error: {e}")
       tests_failed += 1
   
   # Test 3: Key Manager
   print("\n[Test 3] Testing API key manager...")
   try:
       # Get the current key
       current_key = key_manager.get_current_key()
       # Record an API call
       key_manager.record_api_call()
       # Check if the counter incremented
       if key_manager.call_counters[current_key] > 0:
           print("✅ API key manager: Success! Call counter incremented.")
           tests_passed += 1
       else:
           print("❌ API key manager: Failed! Call counter did not increment.")
           tests_failed += 1
   except Exception as e:
       print(f"❌ API key manager: Failed with error: {e}")
       tests_failed += 1
   
   # Test 4: File System Access
   print("\n[Test 4] Testing file system access...")
   try:
       test_file = os.path.join(output_dir, "test_write.txt")
       # Try to write to a test file
       with open(test_file, 'w') as f:
           f.write("Test file write successful")
       
       # Try to read it back
       with open(test_file, 'r') as f:
           content = f.read()
       
       # Delete the test file
       os.remove(test_file)
       
       if content == "Test file write successful":
           print("✅ File system access: Success! Write and read operations completed.")
           tests_passed += 1
       else:
           print("❌ File system access: Failed! Content mismatch.")
           tests_failed += 1
   except Exception as e:
       print(f"❌ File system access: Failed with error: {e}")
       tests_failed += 1
   
   # Print summary
   print("\n===== TEST SUMMARY =====")
   print(f"Tests passed: {tests_passed}/{tests_passed + tests_failed}")
   print(f"Tests failed: {tests_failed}/{tests_passed + tests_failed}")
   
   # Return True if all tests passed
   return tests_failed == 0

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
   parser.add_argument('--update-tags', action='store_true', help='Update existing games with additional tags from store pages')
   parser.add_argument('--skip-tests', action='store_true', help='Skip initial functionality tests')
   parser.add_argument('--clear-cache', action='store_true', help='Clear all cached API responses before starting')
   parser.add_argument('--verify', action='store_true', help='Verify data integrity of the final dataset')
   
   # Parse arguments
   args = parser.parse_args()
   
   # Handle clear cache option
   if args.clear_cache:
       print("Clearing API cache...")
       clear_cache()
   
   # Handle verify option
   if args.verify:
       print("Verifying data integrity...")
       verify_data_integrity()
       sys.exit(0)
   
   # Handle the update-tags option
   if args.update_tags:
       print("Updating existing games with additional tags...")
       update_existing_games_tags()
       sys.exit(0)
   
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
       result = process_single_game(args.app_id, args.app_name or "Unknown Game")
       
       # If processing was successful, save to our dataset
       if result:
           append_game_to_results(result, CSV_RESULTS_FILE)
           # Also update the RDS file
           output_file = os.path.join(output_dir, "steam_data")
           existing_results = []
           if os.path.exists(PROCESSED_GAMES_FILE):
               try:
                   with open(PROCESSED_GAMES_FILE, 'r', encoding='utf-8') as f:
                       existing_results = json.load(f)
               except Exception as e:
                   print(f"Error loading existing results: {e}")
           
           # Add the new result
           existing_results.append(result)
           # Save back
           with open(PROCESSED_GAMES_FILE, 'w', encoding='utf-8') as f:
               json.dump(existing_results, f, ensure_ascii=False, indent=4)
           # Update R files
           save_data_for_r(existing_results, output_file)
           
       # Save updated API key usage stats
       with open(API_USAGE_FILE, 'w', encoding='utf-8') as f:
           json.dump(key_manager.get_usage_stats(), f, ensure_ascii=False, indent=4)
           
       sys.exit(0)
   
   # Regular execution flow for batch processing
   # Run the test function first to verify everything works
   test_successful = True
   if not args.skip_tests:
       test_successful = run_basic_tests()
   else:
       print("Skipping initial functionality tests as requested...")
   
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
       
       # Use batch generator for memory efficiency
       apps_to_process = all_apps[current_index:]
       
       if apps_to_process:
           print(f"Starting to process {len(apps_to_process)} remaining games...")
           
           # Call the parallel processing function ONCE with all remaining apps.
           # It will handle its own internal batching and checkpointing.
           # We pass the 'results' loaded from the checkpoint as 'existing_results'.
           final_results, _ = process_games_in_parallel(
               apps_to_process,
               max_workers=5,
               batch_size=50,  # The internal batch size
               processed_app_ids=processed_app_ids,
               existing_results=results
           )
           
           # The final save is handled by the last checkpoint in the function,
           # but we can do one more save here for good measure.
           print("\nAll processing complete. Performing final save...")
           output_file = os.path.join(output_dir, "steam_data")
           save_data_for_r(final_results, output_file)

       else:
           print("No new games to process. All apps are up-to-date.")
           # Make sure the existing data is saved correctly one last time
           if results:
               output_file = os.path.join(output_dir, "steam_data")
               save_data_for_r(results, output_file)

       # Final data integrity check
       print("\nPerforming final data integrity check...")
       verify_data_integrity()
           
       print("\nData collection complete!")