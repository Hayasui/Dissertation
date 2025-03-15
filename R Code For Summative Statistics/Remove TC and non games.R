# Script to update the Steam games dataset
# This code:
# 1. Combines Traditional Chinese reviews with other language reviews
# 2. Removes Traditional Chinese specific metrics
# 3. Keeps only games with 'Single-player' or 'Multi-player' in user_tags

# Load required libraries
library(readr)
library(dplyr)

# Read the original dataset from RDS file
steam_data <- readRDS("steam_games_data.rds")

# Function to check if a game has single-player or multi-player tags
has_gameplay_tag <- function(tags) {
  # Check if tags is NULL or NA
  if (is.null(tags) || all(is.na(tags))) {
    return(FALSE)
  }
  
  # Convert to lowercase for case-insensitive matching
  tags_lower <- tolower(tags)
  
  # Check if any of the tags contain "single-player" or "multi-player"
  any(grepl("single-player", tags_lower) | grepl("multi-player", tags_lower))
}

# Create updated dataset by:
# 1. Adding tchinese_reviews to other_reviews, handling NA values
# 2. Removing the tchinese-related columns
# 3. Filtering games to keep only those with single-player or multi-player tags
steam_data_updated <- steam_data %>%
  # Combine Traditional Chinese reviews with other reviews
  # Using coalesce to handle any NA values
  mutate(other_reviews = coalesce(other_reviews, 0) + coalesce(tchinese_reviews, 0)) %>%
  # Remove Traditional Chinese specific columns
  select(-tchinese_positive, -tchinese_negative, -tchinese_positive_ratio, -tchinese_reviews) %>%
  # Filter to keep only games with single-player or multi-player tags
  filter(sapply(user_tags, has_gameplay_tag))

# Save the updated dataset as RDS
saveRDS(steam_data_updated, "steam_games_data_updated.rds")

# Print confirmation message
cat("Dataset updated successfully!\n")
cat("'steam_games_data_filtered.rds'.\n")

# Display summary of the changes
cat("\nSummary of changes:\n")
cat("- Combined Traditional Chinese reviews with other language reviews\n")
cat("- Removed columns: tchinese_positive, tchinese_negative, tchinese_positive_ratio, tchinese_reviews\n")
cat("- Filtered to keep only games with 'Single-player' or 'Multi-player' in user_tags\n")
cat("- Original dataset size:", nrow(steam_data), "games\n")
cat("- Updated dataset size:", nrow(steam_data_updated), "games\n")
cat("- Removed", nrow(steam_data) - nrow(steam_data_updated), "games without Single/Multi-player tags\n")
