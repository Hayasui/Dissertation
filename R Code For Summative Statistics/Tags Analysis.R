# Load necessary libraries
library(tidyverse)    # For data manipulation and ggplot2
library(RColorBrewer) # For colour palettes
library(lubridate)    # For date handling
library(scales)       # For better axis formatting
library(knitr)        # For table formatting
library(stringr)      # For string manipulation
library(ggrepel)

# Define unified colour palettes for consistent visualization (matching Price Analysis file)
main_palette <- "Set1"      # For primary categorical variables
alt_palette <- "Set2"       # For secondary categorical variables
sequential_blue <- "Blues"  # For sequential data (blue shades)
sequential_red <- "Reds"    # For alternative sequential data (red shades)
divergent_palette <- "RdBu" # For diverging data (positive/negative)

# Read the RDS file
steam_data <- readRDS("steam_games_data_updated.rds")

# Extract the year from the release date if not already done
if(!"release_year" %in% colnames(steam_data)) {
  steam_data$release_year <- year(as.Date(steam_data$release_date))
}

# -----------------------------
# Helper functions to process tags and genres
# -----------------------------

# Function to extract items from the string representation of lists
extract_items <- function(list_str) {
  if(is.null(list_str) || is.na(list_str) || list_str == "NULL" || list_str == "") {
    return(character(0))
  }
  
  # Remove the outer brackets and split by comma
  items <- str_extract_all(list_str, "'[^']*'")[[1]]
  # Remove the quotes
  items <- str_replace_all(items, "'", "")
  return(items)
}

# Function to process the combined tags and genres
process_combined_tags <- function(df) {
  # For each row, extract and combine tags and genres
  combined_list <- mapply(function(tags, genres) {
    unique(c(extract_items(tags), extract_items(genres)))
  }, df$user_tags, df$genres, SIMPLIFY = FALSE)
  
  return(combined_list)
}

# Apply the function to create a combined tags-genres list
steam_data$combined_tags <- process_combined_tags(steam_data)

# Create a flag for any Chinese localisation (if not already present)
if(!"has_chinese_loc" %in% colnames(steam_data)) {
  steam_data <- steam_data %>%
    mutate(
      has_chinese_loc = chinese_simplified_interface_subtitles == TRUE | 
        chinese_simplified_audio == TRUE |
        chinese_traditional_interface_subtitles == TRUE |
        chinese_traditional_audio == TRUE,
      has_sc_loc = chinese_simplified_interface_subtitles == TRUE | 
        chinese_simplified_audio == TRUE
    )
}

# -----------------------------
# 1. Frequency for each user tag/genre
# -----------------------------

# Unnest the combined tags to count frequencies
tag_frequencies <- unlist(steam_data$combined_tags) %>%
  table() %>%
  sort(decreasing = TRUE) %>%
  as.data.frame()

colnames(tag_frequencies) <- c("Tag", "Frequency")

# Calculate percentage
tag_frequencies <- tag_frequencies %>%
  mutate(Percentage = Frequency / nrow(steam_data) * 100)

# Display the top 20 tags
top_20_tags <- head(tag_frequencies, 20)
print(top_20_tags)

# Visualize the top 20 tags
ggplot(top_20_tags, aes(x = reorder(Tag, Frequency), y = Frequency)) +
  geom_bar(stat = "identity", fill = brewer.pal(8, sequential_blue)[6]) +
  geom_text(aes(label = Frequency), hjust = -0.2, size = 3) +
  coord_flip() +
  labs(
    title = "Top 20 Most Common User Tags and Genres",
    subtitle = "Frequency of occurrence across all games",
    x = NULL,
    y = "Number of Games",
    caption = "Source: Steam API Data"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold")
  )

# Visualize percentage distribution
ggplot(top_20_tags, aes(x = reorder(Tag, Percentage), y = Percentage)) +
  geom_bar(stat = "identity", fill = brewer.pal(8, sequential_blue)[6]) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), hjust = -0.2, size = 3) +
  coord_flip() +
  labs(
    title = "Top 20 Most Common User Tags and Genres (Percentage)",
    subtitle = "Percentage of games with each tag",
    x = NULL,
    y = "Percentage of Games",
    caption = "Source: Steam API Data"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold")
  ) +
  scale_y_continuous(limits = c(0, max(top_20_tags$Percentage) * 1.1),
                     labels = function(x) paste0(round(x, 0), "%"))

# -----------------------------
# 2. Genre-specific localisation: Which user tag/genres have highest/lowest Chinese localisation rates
# -----------------------------

# Function to calculate localisation rates for each tag
calculate_tag_loc_rates <- function(tag_list) {
  # Map over all tags, calculating localisation rates for each
  tag_loc_rates <- lapply(names(tag_list), function(tag) {
    # Subset to games with this tag
    games_with_tag <- steam_data[sapply(steam_data$combined_tags, function(tags) tag %in% tags),]
    
    # Calculate localisation rates
    loc_rate <- sum(games_with_tag$has_chinese_loc, na.rm = TRUE) / nrow(games_with_tag) * 100
    sc_loc_rate <- sum(games_with_tag$has_sc_loc, na.rm = TRUE) / nrow(games_with_tag) * 100
    
    # Create a data frame with the results
    data.frame(
      Tag = tag,
      Games_Count = nrow(games_with_tag),
      Chinese_Loc_Rate = loc_rate,
      SC_Loc_Rate = sc_loc_rate
    )
  })
  
  # Combine results and sort
  result <- do.call(rbind, tag_loc_rates) %>%
    arrange(desc(Chinese_Loc_Rate))
  
  return(result)
}

# Get tag frequency counts
tag_counts <- table(unlist(steam_data$combined_tags))

# Only analyze tags that appear in at least 50 games (for meaningful results)
frequent_tags <- names(tag_counts[tag_counts >= 50])
frequent_tag_counts <- tag_counts[tag_counts >= 50]

# Calculate localisation rates for frequent tags
tag_loc_rates <- calculate_tag_loc_rates(frequent_tag_counts)

# Display top and bottom 10 tags by localisation rate
top_10_loc <- head(tag_loc_rates, 10)
bottom_10_loc <- tail(tag_loc_rates, 10)

print("Top 10 tags with highest Chinese localisation rates:")
print(top_10_loc)

print("Bottom 10 tags with lowest Chinese localisation rates:")
print(bottom_10_loc)

# Visualize top 10 tags with highest localisation rates
ggplot(top_10_loc, aes(x = reorder(Tag, Chinese_Loc_Rate), y = Chinese_Loc_Rate)) +
  geom_bar(stat = "identity", fill = brewer.pal(8, sequential_blue)[6]) +
  geom_text(aes(label = paste0(round(Chinese_Loc_Rate, 1), "%")), hjust = -0.2, size = 3) +
  coord_flip() +
  labs(
    title = "Top 10 Tags with Highest Chinese Localisation Rates",
    subtitle = "Percentage of games with Chinese language support by tag",
    x = NULL,
    y = "Localisation Rate (%)",
    caption = "Source: Steam API Data\nOnly includes tags present in at least 50 games"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold")
  ) +
  scale_y_continuous(limits = c(0, 100),
                     labels = function(x) paste0(round(x, 0), "%"))

# Visualize bottom 10 tags with lowest localisation rates
ggplot(bottom_10_loc, aes(x = reorder(Tag, -Chinese_Loc_Rate), y = Chinese_Loc_Rate)) +
  geom_bar(stat = "identity", fill = brewer.pal(8, sequential_red)[6]) +
  geom_text(aes(label = paste0(round(Chinese_Loc_Rate, 1), "%")), hjust = 1.2, size = 3, color = "white") +
  coord_flip() +
  labs(
    title = "Bottom 10 Tags with Lowest Chinese Localisation Rates",
    subtitle = "Percentage of games with Chinese language support by tag",
    x = NULL,
    y = "Localisation Rate (%)",
    caption = "Source: Steam API Data\nOnly includes tags present in at least 50 games"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold")
  ) +
  scale_y_continuous(limits = c(0, 100),
                     labels = function(x) paste0(round(x, 0), "%"))

# -----------------------------
# 3. Tag association with localisation: Most common user tags/genres for games with/without SC localisation
# -----------------------------

# Separate games with and without SC localisation
games_with_sc <- steam_data %>% filter(has_sc_loc == TRUE)
games_without_sc <- steam_data %>% filter(has_sc_loc == FALSE)

# Calculate tag frequencies for games with SC localisation
tags_with_sc <- unlist(games_with_sc$combined_tags) %>%
  table() %>%
  sort(decreasing = TRUE) %>%
  as.data.frame()

colnames(tags_with_sc) <- c("Tag", "Frequency")
tags_with_sc <- tags_with_sc %>%
  mutate(Percentage = Frequency / nrow(games_with_sc) * 100) %>%
  arrange(desc(Percentage))

# Calculate tag frequencies for games without SC localisation
tags_without_sc <- unlist(games_without_sc$combined_tags) %>%
  table() %>%
  sort(decreasing = TRUE) %>%
  as.data.frame()

colnames(tags_without_sc) <- c("Tag", "Frequency")
tags_without_sc <- tags_without_sc %>%
  mutate(Percentage = Frequency / nrow(games_without_sc) * 100) %>%
  arrange(desc(Percentage))

# Display top 10 tags for each group
print("Top 10 tags for games WITH SC localisation:")
print(head(tags_with_sc, 10))

print("Top 10 tags for games WITHOUT SC localisation:")
print(head(tags_without_sc, 10))

# Visualize top 10 tags for games with SC localisation
ggplot(head(tags_with_sc, 10), aes(x = reorder(Tag, Percentage), y = Percentage)) +
  geom_bar(stat = "identity", fill = brewer.pal(8, sequential_blue)[6]) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), hjust = -0.2, size = 3) +
  coord_flip() +
  labs(
    title = "Top 10 Tags in Games WITH Simplified Chinese Localisation",
    subtitle = "Percentage occurrence among games with SC support",
    x = NULL,
    y = "Percentage of Games",
    caption = "Source: Steam API Data"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold")
  ) +
  scale_y_continuous(limits = c(0, max(head(tags_with_sc, 10)$Percentage) * 1.1),
                     labels = function(x) paste0(round(x, 0), "%"))

# Visualize top 10 tags for games without SC localisation
ggplot(head(tags_without_sc, 10), aes(x = reorder(Tag, Percentage), y = Percentage)) +
  geom_bar(stat = "identity", fill = brewer.pal(8, sequential_red)[6]) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), hjust = -0.2, size = 3) +
  coord_flip() +
  labs(
    title = "Top 10 Tags in Games WITHOUT Simplified Chinese Localisation",
    subtitle = "Percentage occurrence among games without SC support",
    x = NULL,
    y = "Percentage of Games",
    caption = "Source: Steam API Data"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold")
  ) +
  scale_y_continuous(limits = c(0, max(head(tags_without_sc, 10)$Percentage) * 1.1),
                     labels = function(x) paste0(round(x, 0), "%"))

# Compare the difference in tag prevalence between localised and non-localised games
# Join the two datasets
tags_comparison <- tags_with_sc %>%
  select(Tag, Percentage) %>%
  rename(With_SC = Percentage) %>%
  left_join(
    tags_without_sc %>%
      select(Tag, Percentage) %>%
      rename(Without_SC = Percentage),
    by = "Tag"
  ) %>%
  mutate(
    Without_SC = ifelse(is.na(Without_SC), 0, Without_SC),
    Difference = With_SC - Without_SC
  ) %>%
  arrange(desc(Difference))

# Show tags with the biggest positive and negative differences
print("Tags more common in games WITH SC localisation:")
print(head(tags_comparison, 10))

print("Tags more common in games WITHOUT SC localisation:")
print(tail(tags_comparison, 10))

# Visualize the biggest differences
top_differences <- rbind(
  head(tags_comparison, 10) %>% mutate(Group = "More common with SC"),
  tail(tags_comparison, 10) %>% mutate(Group = "More common without SC")
)

ggplot(top_differences, aes(x = reorder(Tag, Difference), y = Difference, fill = Group)) +
  geom_bar(stat = "identity") +
  geom_text(aes(
    label = paste0(round(abs(Difference), 1), "%"),
    hjust = ifelse(Difference > 0, -0.2, 1.2),
    color = ifelse(Difference > 0, "black", "white")
  ), size = 3) +
  scale_fill_manual(values = c(
    "More common with SC" = brewer.pal(8, sequential_blue)[6],
    "More common without SC" = brewer.pal(8, sequential_red)[6]
  )) +
  scale_color_manual(values = c("white", "black"), guide = "none") +
  coord_flip() +
  labs(
    title = "Tags with Largest Differences in Prevalence by SC Localisation Status",
    subtitle = "Percentage point difference between games with vs. without SC support",
    x = NULL,
    y = "Percentage Point Difference",
    caption = "Source: Steam API Data"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.title = element_blank(),
    legend.position = "top"
  )

# -----------------------------
# 4. Genre preferences: Top 10 User tag/genres with highest proportion of SC reviews
# -----------------------------

# Function to calculate SC review proportion for games with each tag
calculate_tag_sc_review_prop <- function(tag_list) {
  # Map over all tags
  tag_review_props <- lapply(names(tag_list), function(tag) {
    # Subset to games with this tag
    games_with_tag <- steam_data[sapply(steam_data$combined_tags, function(tags) tag %in% tags),]
    
    # Total reviews across all languages
    total_reviews <- sum(games_with_tag$total_reviews, na.rm = TRUE)
    
    # SC reviews
    sc_reviews <- sum(games_with_tag$schinese_reviews, na.rm = TRUE)
    
    # Calculate proportion
    sc_proportion <- sc_reviews / total_reviews * 100
    
    data.frame(
      Tag = tag,
      Games_Count = nrow(games_with_tag),
      Total_Reviews = total_reviews,
      SC_Reviews = sc_reviews,
      SC_Proportion = sc_proportion
    )
  })
  
  # Combine results and sort
  result <- do.call(rbind, tag_review_props) %>%
    arrange(desc(SC_Proportion))
  
  return(result)
}

# Only analyze tags that appear in a significant number of games and have substantial reviews
# This ensures we're looking at meaningful patterns
tag_review_props <- calculate_tag_sc_review_prop(frequent_tag_counts) %>%
  filter(Total_Reviews >= 5000)  # Minimum threshold for total reviews

# Display top and bottom tags by SC review proportion
top_10_sc_reviews <- head(tag_review_props, 10)
bottom_10_sc_reviews <- tail(tag_review_props, 10)

print("Top 10 tags with highest proportion of SC reviews:")
print(top_10_sc_reviews)

print("Bottom 10 tags with lowest proportion of SC reviews:")
print(bottom_10_sc_reviews)

# Visualize top 10 tags with highest SC review proportion
ggplot(top_10_sc_reviews, aes(x = reorder(Tag, SC_Proportion), y = SC_Proportion)) +
  geom_bar(stat = "identity", fill = brewer.pal(8, sequential_blue)[6]) +
  geom_text(aes(label = paste0(round(SC_Proportion, 1), "%")), hjust = -0.2, size = 3) +
  coord_flip() +
  labs(
    title = "Top 10 Tags with Highest Proportion of SC Reviews",
    subtitle = "Percentage of reviews in Simplified Chinese by tag",
    x = NULL,
    y = "SC Review Proportion (%)",
    caption = "Source: Steam API Data\nOnly includes tags with at least 5,000 total reviews"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold")
  ) +
  scale_y_continuous(limits = c(0, max(top_10_sc_reviews$SC_Proportion) * 1.1),
                     labels = function(x) paste0(round(x, 0), "%"))

# Visualize bottom 10 tags with lowest SC review proportion
ggplot(bottom_10_sc_reviews, aes(x = reorder(Tag, -SC_Proportion), y = SC_Proportion)) +
  geom_bar(stat = "identity", fill = brewer.pal(8, sequential_red)[6]) +
  geom_text(aes(label = paste0(round(SC_Proportion, 1), "%")), hjust = 1.2, size = 3, color = "white") +
  coord_flip() +
  labs(
    title = "Bottom 10 Tags with Lowest Proportion of SC Reviews",
    subtitle = "Percentage of reviews in Simplified Chinese by tag",
    x = NULL,
    y = "SC Review Proportion (%)",
    caption = "Source: Steam API Data\nOnly includes tags with at least 5,000 total reviews"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold")
  ) +
  scale_y_continuous(limits = c(0, 100),
                     labels = function(x) paste0(round(x, 0), "%"))

# -----------------------------
# 5. Cultural preference indicators: Tags with differences in positive review ratios between SC and English
# -----------------------------

# Define minimum games required for analysis if not already defined
if(!exists("min_games_for_analysis")) {
  min_games_for_analysis <- 50  # Minimum number of games with a tag for reliable analysis
}

# Calculate the sentiment gap by tag
tag_sentiment_gap <- steam_data %>%
  filter(english_reviews >= 20 & schinese_reviews >= 20) %>%  # Ensure enough reviews in both languages
  rowwise() %>%
  mutate(
    sc_positive_ratio = schinese_positive / schinese_reviews * 100,
    en_positive_ratio = english_positive / english_reviews * 100,
    sentiment_gap = sc_positive_ratio - en_positive_ratio
  ) %>%
  ungroup() %>%
  select(combined_tags, sc_positive_ratio, en_positive_ratio, sentiment_gap) %>%
  unnest(combined_tags) %>%
  rename(tag = combined_tags) %>%
  group_by(tag) %>%
  summarise(
    game_count = n(),
    avg_sc_positive = mean(sc_positive_ratio, na.rm = TRUE),
    avg_en_positive = mean(en_positive_ratio, na.rm = TRUE),
    avg_sentiment_gap = mean(sentiment_gap, na.rm = TRUE),
    median_sentiment_gap = median(sentiment_gap, na.rm = TRUE),
    sc_higher_count = sum(sentiment_gap > 0, na.rm = TRUE),
    sc_higher_percent = sc_higher_count / game_count * 100
  ) %>%
  filter(game_count >= min_games_for_analysis) %>%  # Ensure enough games for reliable stats
  arrange(desc(avg_sentiment_gap))

# Get tags with biggest positive and negative sentiment gaps
top_10_sc_preferred <- head(tag_sentiment_gap, 10)
top_10_en_preferred <- tail(tag_sentiment_gap, 10)

# Display the top SC-preferred tags table
print("Top 10 Tags Preferred by SC Users (Highest Positive Gap)")
print(top_10_sc_preferred %>% 
        select(tag, game_count, avg_sc_positive, avg_en_positive, avg_sentiment_gap, sc_higher_percent))

# Display the top English-preferred tags table
print("Top 10 Tags Preferred by English Users (Highest Negative Gap)")
print(top_10_en_preferred %>% 
        select(tag, game_count, avg_sc_positive, avg_en_positive, avg_sentiment_gap, sc_higher_percent))

# Create a new visualization to compare positive review ratios directly
language_comparison_viz <- bind_rows(
  # For tags preferred by English users
  top_10_en_preferred %>% 
    select(tag, avg_sc_positive, avg_en_positive) %>%
    pivot_longer(cols = c(avg_sc_positive, avg_en_positive),
                 names_to = "language",
                 values_to = "positive_ratio") %>%
    mutate(group = "English Preferred",
           language = ifelse(language == "avg_sc_positive", "Simplified Chinese", "English")),
  
  # For tags preferred by SC users
  top_10_sc_preferred %>% 
    select(tag, avg_sc_positive, avg_en_positive) %>%
    pivot_longer(cols = c(avg_sc_positive, avg_en_positive),
                 names_to = "language",
                 values_to = "positive_ratio") %>%
    mutate(group = "SC Preferred",
           language = ifelse(language == "avg_sc_positive", "Simplified Chinese", "English"))
)

# Create the side-by-side bar chart for language comparison
lang_comparison_plot <- ggplot(language_comparison_viz, aes(x = reorder(tag, positive_ratio), 
                                                            y = positive_ratio, 
                                                            fill = language)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0(round(positive_ratio, 0), "%")),
            position = position_dodge(width = 0.9),
            vjust = -0.5,
            size = 3) +
  facet_wrap(~group, scales = "free_x") +
  coord_flip() +
  scale_fill_manual(values = c("English" = "#E41A1C", "Simplified Chinese" = "#377EB8")) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(
    title = "Comparison of Positive Review Ratios by Language",
    subtitle = "For tags with the largest sentiment differences",
    x = "Tag",
    y = "Positive Review Ratio (%)",
    fill = "Language",
    caption = "Source: Steam API Data"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.y = element_text(size = 9),
    strip.text = element_text(size = 12, face = "bold"),
    strip.background = element_rect(fill = "lightgray", color = NA)
  )

print(lang_comparison_plot)

# Verify that tag_sc_review_proportion exists
if(!exists("tag_sc_review_proportion")) {
  # Calculate the proportion of SC reviews for each tag
  tag_sc_review_proportion <- steam_data %>%
    filter(total_reviews > 0) %>%  # Ensure there are reviews
    rowwise() %>%
    mutate(
      sc_review_proportion = schinese_reviews / total_reviews * 100
    ) %>%
    ungroup() %>%
    select(combined_tags, total_reviews, schinese_reviews, sc_review_proportion) %>%
    unnest(combined_tags) %>%
    rename(tag = combined_tags) %>%
    group_by(tag) %>%
    summarise(
      game_count = n(),
      total_reviews_sum = sum(total_reviews, na.rm = TRUE),
      schinese_reviews_sum = sum(schinese_reviews, na.rm = TRUE),
      avg_sc_review_proportion = mean(sc_review_proportion, na.rm = TRUE),
      aggregate_sc_proportion = schinese_reviews_sum / total_reviews_sum * 100
    ) %>%
    filter(game_count >= min_games_for_analysis) %>%  # Ensure enough games for reliable stats
    filter(total_reviews_sum >= 1000) %>%  # Ensure enough reviews for reliable stats
    arrange(desc(aggregate_sc_proportion))
}

# Create the improved scatter plot
cultural_scatter_plot <- ggplot(tag_cultural_comparison, aes(x = aggregate_sc_proportion, y = avg_sentiment_gap)) +
  geom_smooth(method = "lm", color = "darkgray", fill = "lightgray") +
  geom_point(aes(size = game_count, color = aggregate_sc_proportion), alpha = 0.7) +
  geom_text_repel(data = extreme_tags, 
                  aes(label = tag),
                  box.padding = 0.5,
                  point.padding = 0.3,
                  force = 2,
                  max.overlaps = 15,
                  size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  scale_size_continuous(name = "Number of Games", range = c(2, 10)) +
  scale_color_viridis_c(name = "SC Review %", option = "E") +
  # Calculate the x-axis limits dynamically based on the data
  scale_x_continuous(
    name = "Percentage of Reviews in Simplified Chinese",
    labels = function(x) paste0(x, "%"),
    # Set the upper limit to max value + 5% instead of hard-coded 100
    limits = c(min_x_limit, max(tag_cultural_comparison$aggregate_sc_proportion, na.rm = TRUE) * 1.05),
    breaks = function(limits) {
      # Create breaks at 5% intervals
      seq(floor(limits[1]), ceiling(limits[2]), by = 5)
    }
  ) +
  scale_y_continuous(
    name = "Sentiment Gap (SC - English) in Percentage Points",
    labels = function(y) paste0(y, "%")
  ) +
  labs(
    title = "Relationship Between Chinese Review Proportion and Sentiment Gap",
    subtitle = paste0("Correlation: ", round(cor(tag_cultural_comparison$aggregate_sc_proportion, 
                                                 tag_cultural_comparison$avg_sentiment_gap, 
                                                 use = "complete.obs"), 3)),
    caption = "Source: Steam API Data\nOnly includes tags with substantial review numbers"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right",
    panel.grid.minor = element_line(color = "lightgray", linetype = "dotted"),
    panel.grid.major = element_line(color = "lightgray")
  )

print(cultural_scatter_plot)

# Create quadrant analysis for cultural preferences
# Calculate means for creating quadrants
mean_sc_prop <- mean(tag_cultural_comparison$aggregate_sc_proportion, na.rm = TRUE)
mean_sentiment_gap <- mean(tag_cultural_comparison$avg_sentiment_gap, na.rm = TRUE)

# Add quadrant classification
tag_cultural_comparison <- tag_cultural_comparison %>%
  mutate(
    quadrant = case_when(
      aggregate_sc_proportion > mean_sc_prop & avg_sentiment_gap > mean_sentiment_gap ~ "High SC proportion, SC prefers",
      aggregate_sc_proportion > mean_sc_prop & avg_sentiment_gap < mean_sentiment_gap ~ "High SC proportion, EN prefers",
      aggregate_sc_proportion < mean_sc_prop & avg_sentiment_gap > mean_sentiment_gap ~ "Low SC proportion, SC prefers",
      aggregate_sc_proportion < mean_sc_prop & avg_sentiment_gap < mean_sentiment_gap ~ "Low SC proportion, EN prefers",
      TRUE ~ "Neutral"
    )
  )

# Count tags in each quadrant
quadrant_counts <- tag_cultural_comparison %>%
  count(quadrant) %>%
  mutate(percentage = n / sum(n) * 100)

# Display the quadrant counts
print("Distribution of Tags Across Cultural Preference Quadrants")
print(quadrant_counts)

# Create a more detailed quadrant visualization
quadrant_plot <- ggplot(tag_cultural_comparison, aes(x = aggregate_sc_proportion, y = avg_sentiment_gap)) +
  # Add quadrant lines
  geom_vline(xintercept = mean_sc_prop, linetype = "dashed", color = "darkgray") +
  geom_hline(yintercept = mean_sentiment_gap, linetype = "dashed", color = "darkgray") +
  # Add points
  geom_point(aes(size = game_count, color = quadrant), alpha = 0.7) +
  # Add selected tag labels with increased max.overlaps to handle warnings
  geom_text_repel(data = extreme_tags, 
                  aes(label = tag),
                  box.padding = 0.5,
                  point.padding = 0.3,
                  force = 2,
                  max.overlaps = 15,
                  size = 3) +
  # Format scales
  scale_size_continuous(name = "Number of Games", range = c(2, 8)) +
  scale_color_brewer(name = "Preference Pattern", palette = "Set1") +
  scale_x_continuous(
    name = "Percentage of Reviews in Simplified Chinese",
    labels = function(x) paste0(x, "%"),
    # Set upper limit based on data rather than fixed at 100%
    limits = c(min_x_limit, max(tag_cultural_comparison$aggregate_sc_proportion, na.rm = TRUE) * 1.05),
    breaks = function(limits) {
      # Create breaks at 5% intervals
      seq(floor(limits[1]), ceiling(limits[2]), by = 5)
    }
  ) +
  scale_y_continuous(
    name = "Sentiment Gap (SC - English) in Percentage Points",
    labels = function(y) paste0(y, "%")
  ) +
  # Quadrant labels removed to reduce clutter +
  # Add titles and theme
  labs(
    title = "Cultural Preference Quadrant Analysis",
    subtitle = "Mapping game tags by engagement patterns and sentiment differences",
    caption = "Source: Steam API Data\nMean lines create four preference quadrants"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right",
    # Add more space around the plot to prevent text clipping
    plot.margin = margin(1, 1, 1, 1, "cm")
  )

print(quadrant_plot)
