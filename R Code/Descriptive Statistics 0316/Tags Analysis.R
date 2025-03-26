```R
# Load necessary libraries
library(tidyverse)    # For data manipulation and ggplot2
library(RColorBrewer) # For colour palettes
library(lubridate)    # For date handling
library(scales)       # For better axis formatting
library(knitr)        # For table formatting
library(kableExtra)   # For enhanced tables
library(stringr)      # For string manipulation
library(ggrepel)      # For data analysis

# Define unified colour palettes for consistent visualization
main_palette <- "Set1"        # For primary categorical variables
alt_palette <- "Set2"         # For secondary categorical variables
sequential_blue <- "Blues"    # For sequential data (blue shades)
sequential_red <- "Reds"      # For alternative sequential data (red shades)
divergent_palette <- "RdBu"   # For diverging data (positive/negative)

# Create a custom theme for all ggplot visualisations
my_theme <- theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_line(color = "lightgray", linetype = "dotted"),
    panel.grid.major = element_line(color = "lightgray")
  )

# Function for consistent faceted chart theme
facet_theme <- function() {
  my_theme +
    theme(
      # Standardize facet strip appearance
      strip.text = element_text(size = 11, face = "bold"),
      strip.background = element_rect(fill = "lightgray", color = NA),
      
      # Consistent spacing
      panel.spacing = unit(1, "cm"),
      plot.margin = margin(1, 1, 1, 1, "cm"),
      
      # Remove horizontal grid lines for better readability in faceted charts
      panel.grid.major.y = element_blank(),
      
      # Ensure legend position is consistent with overall document theme
      legend.position = "right"
    )
}

# Function for consistent table formatting with proper caption styling
format_table <- function(df, caption = NULL, digits = 1, col.names = NULL) {
  # First create the basic table
  k_table <- kable(df, 
        caption = caption,
        digits = digits,
        col.names = col.names)
  
  # Apply custom caption styling using direct HTML modification
  # This replaces the default caption tag with one that has explicit styling
  if (!is.null(caption)) {
    k_table <- gsub('<caption>', 
                   '<caption style="color: black; text-align: left; font-weight: bold; caption-side: top; white-space: nowrap;">', 
                   k_table)
  }
  
  # Continue with the rest of the styling
  k_table %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                  font_size = 11,
                  full_width = FALSE,
                  position = "left") %>%
    row_spec(0, bold = TRUE)  # Bold header row
}

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
# 1. Tag Frequency Analysis
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

# Display the top 20 tags with consistent formatting
top_20_tags <- head(tag_frequencies, 20)
format_table(top_20_tags, 
             caption = "Top 20 Most Common User Tags and Genres",
             col.names = c("Tag", "Frequency", "Percentage"),
             digits = c(0, 0, 1))

# Visualise the top 20 tags with consistent styling
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
  my_theme

# Visualise percentage distribution with consistent styling
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
  my_theme +
  scale_y_continuous(limits = c(0, max(top_20_tags$Percentage) * 1.1),
                     labels = function(x) paste0(round(x, 1), "%"))

# -----------------------------
# 2. Genre-specific Localisation Rates
# -----------------------------

# Create a tag count threshold for analysis
min_games_for_tag <- 25

# Get tag frequencies
tag_counts <- table(unlist(steam_data$combined_tags))
frequent_tags <- names(tag_counts[tag_counts >= min_games_for_tag])

# Function to calculate localisation rates for each tag
calculate_tag_loc_rates <- function() {
  # Map over frequent tags
  tag_loc_rates <- lapply(frequent_tags, function(tag) {
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

# Calculate localisation rates for frequent tags
tag_loc_rates <- calculate_tag_loc_rates()

# Display top and bottom 20 tags by localisation rate with consistent formatting
top_20_loc <- head(tag_loc_rates, 20)
bottom_20_loc <- tail(tag_loc_rates, 20)

format_table(top_20_loc, 
             caption = "Top 20 Tags with Highest Chinese Localisation Rates",
             col.names = c("Tag", "Games Count", "Chinese Loc Rate (%)", "SC Loc Rate (%)"),
             digits = c(0, 0, 1, 1))

format_table(bottom_20_loc, 
             caption = "Bottom 20 Tags with Lowest Chinese Localisation Rates",
             col.names = c("Tag", "Games Count", "Chinese Loc Rate (%)", "SC Loc Rate (%)"),
             digits = c(0, 0, 1, 1))

# Visualise top 20 tags with highest localisation rates with consistent styling
ggplot(top_20_loc, aes(x = reorder(Tag, Chinese_Loc_Rate), y = Chinese_Loc_Rate)) +
  geom_bar(stat = "identity", fill = brewer.pal(8, sequential_blue)[6]) +
  geom_text(aes(label = paste0(round(Chinese_Loc_Rate, 1), "%")), hjust = -0.3, size = 3) +
  coord_flip() +
  labs(
    title = "Top 20 Tags with Highest Chinese Localisation Rates",
    subtitle = "Percentage of games with Chinese language support by tag",
    x = NULL,
    y = "Localisation Rate (%)",
    caption = "Source: Steam API Data\nOnly includes tags present in at least 25 games"
  ) +
  my_theme +
  theme(panel.grid.major.y = element_blank()) +  # Remove horizontal grid lines
  scale_y_continuous(limits = c(0, 100),
                     labels = function(x) paste0(round(x, 1), "%"))

# Visualise bottom 20 tags with lowest localisation rates with consistent styling
ggplot(bottom_20_loc, aes(x = reorder(Tag, -Chinese_Loc_Rate), y = Chinese_Loc_Rate)) +
  geom_bar(stat = "identity", fill = brewer.pal(8, sequential_red)[6]) +
  geom_text(aes(label = paste0(round(Chinese_Loc_Rate, 1), "%")), hjust = 1.2, size = 3, color = "white") +
  coord_flip() +
  labs(
    title = "Bottom 20 Tags with Lowest Chinese Localisation Rates",
    subtitle = "Percentage of games with Chinese language support by tag",
    x = NULL,
    y = "Localisation Rate (%)",
    caption = "Source: Steam API Data\nOnly includes tags present in at least 25 games"
  ) +
  my_theme +
  scale_y_continuous(limits = c(0, max(bottom_20_loc$Chinese_Loc_Rate) * 1.3),
                     labels = function(x) paste0(round(x, 1), "%"))

# -----------------------------
# 3. Tag Association with Localisation
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

# Display top 20 tags for each group with consistent formatting
format_table(head(tags_with_sc, 20), 
             caption = "Top 20 Tags for Games with SC Localisation",
             col.names = c("Tag", "Frequency", "Percentage with SC"),
             digits = c(0, 0, 1))

format_table(head(tags_without_sc, 20), 
             caption = "Top 20 Tags for Games without SC Localisation",
             col.names = c("Tag", "Frequency", "Percentage without SC"),
             digits = c(0, 0, 1))

# Visualise top 20 tags for games with SC localisation with consistent styling
ggplot(head(tags_with_sc, 20), aes(x = reorder(Tag, Percentage), y = Percentage)) +
  geom_bar(stat = "identity", fill = brewer.pal(8, sequential_blue)[6]) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), hjust = -0.2, size = 3) +
  coord_flip() +
  labs(
    title = "Top 20 Tags in Games with Simplified Chinese Localisation",
    subtitle = "Percentage occurrence among games with SC support",
    x = NULL,
    y = "Percentage of Games",
    caption = "Source: Steam API Data"
  ) +
  my_theme +
  scale_y_continuous(limits = c(0, max(head(tags_with_sc, 20)$Percentage) * 1.1),
                     labels = function(x) paste0(round(x, 1), "%"))

# Visualise top 20 tags for games without SC localisation with consistent styling
ggplot(head(tags_without_sc, 20), aes(x = reorder(Tag, Percentage), y = Percentage)) +
  geom_bar(stat = "identity", fill = brewer.pal(8, sequential_red)[6]) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), hjust = -0.2, size = 3) +
  coord_flip() +
  labs(
    title = "Top 20 Tags in Games without Simplified Chinese Localisation",
    subtitle = "Percentage occurrence among games without SC support",
    x = NULL,
    y = "Percentage of Games",
    caption = "Source: Steam API Data"
  ) +
  my_theme +
  scale_y_continuous(limits = c(0, max(head(tags_without_sc, 20)$Percentage) * 1.1),
                     labels = function(x) paste0(round(x, 1), "%"))

# Compare the difference in tag prevalence between SC localised and non-localised games
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
format_table(head(tags_comparison, 20), 
             caption = "Tags Most Strongly Associated with SC Localised Games",
             col.names = c("Tag", "With SC %", "Without SC %", "Difference pp"),
             digits = c(0, 1, 1, 1))

format_table(tail(tags_comparison, 20), 
             caption = "Tags Most Strongly Associated with Non-SC Localised Games",
             col.names = c("Tag", "With SC %", "Without SC %", "Difference pp"),
             digits = c(0, 1, 1, 1))

# Visualise the biggest differences
top_differences <- rbind(
  head(tags_comparison, 20) %>% mutate(Group = "More common with SC"),
  tail(tags_comparison, 20) %>% mutate(Group = "More common without SC")
)

# Modified plot with improved margins and spacing
ggplot(top_differences, aes(x = reorder(Tag, Difference), y = Difference, fill = Group)) +
  geom_bar(stat = "identity") +
  # Separate geom_text for positive differences (blue bars)
  geom_text(data = subset(top_differences, Difference > 0),
            aes(label = paste0(round(abs(Difference), 1), "%")),
            hjust = -0.3, color = "black", size = 3) +
  # Separate geom_text for negative differences (red bars)
  geom_text(data = subset(top_differences, Difference < 0),
            aes(label = paste0(round(abs(Difference), 1), "%")),
            hjust = 1.1, color = "white", size = 3) +
  scale_fill_manual(values = c(
    "More common with SC" = brewer.pal(8, sequential_blue)[6],
    "More common without SC" = brewer.pal(8, sequential_red)[6]
  )) +
  coord_flip() +
  labs(
    title = "Tag Prevalence Gap Between SC and Non-SC",
    subtitle = "Percentage point differentials highlighting genre characterisation",
    x = NULL,
    y = "Percentage Point Difference",
    caption = "Source: Steam API Data"
  ) +
  my_theme +
  theme(
    legend.title = element_blank(),
    legend.position = "top",
    panel.grid.major.y = element_blank(),
    plot.title = element_text(size = 14, face = "bold"),
    plot.margin = margin(10, 30, 10, 10, "pt")  # Increased right margin
  ) +
  scale_y_continuous(
    limits = function(x) c(min(top_differences$Difference) * 1.1, max(top_differences$Difference) * 1.3),
    expand = expansion(mult = c(0.1, 0.2))
  )

# -----------------------------
# 4. Tags with Highest SC Review Proportion
# -----------------------------

# Function to calculate SC review proportion for each tag
calculate_tag_sc_review_prop <- function() {
  # Get all tags that appear in at least one game
  all_tags <- unique(unlist(steam_data$combined_tags))
  
  # Calculate review proportions for each tag
  tag_review_props <- lapply(all_tags, function(tag) {
    # Subset to games with this tag
    games_with_tag <- steam_data[sapply(steam_data$combined_tags, function(tags) tag %in% tags),]
    
    # Calculate total reviews across all languages
    total_reviews <- sum(games_with_tag$total_reviews, na.rm = TRUE)
    
    # Calculate SC reviews
    sc_reviews <- sum(games_with_tag$schinese_reviews, na.rm = TRUE)
    
    # Calculate proportion
    sc_proportion <- if(total_reviews > 0) (sc_reviews / total_reviews) * 100 else 0
    
    data.frame(
      Tag = tag,
      Games_Count = nrow(games_with_tag),
      Total_Reviews = total_reviews,
      SC_Reviews = sc_reviews,
      SC_Proportion = sc_proportion
    )
  })
  
  # Combine results, filter and sort
  result <- do.call(rbind, tag_review_props) %>%
    filter(Total_Reviews >= 200) %>%  # Only include tags with sufficient reviews
    arrange(desc(SC_Proportion))
  
  return(result)
}

# Calculate review proportions
tag_review_props <- calculate_tag_sc_review_prop()

# Print the number of tags that meet the criteria
print(paste("Number of tags with at least 200 total reviews:", nrow(tag_review_props)))

# Get top and bottom 20 tags by SC review proportion
top_20_sc_reviews <- head(tag_review_props, 20)
bottom_20_sc_reviews <- tail(tag_review_props, 20)

# Display the results with consistent formatting
format_table(top_20_sc_reviews, 
             caption = "Top 20 Tags with Highest Proportion of SC Reviews",
             col.names = c("Tag", "Games Count", "Total Reviews", "SC Reviews", "SC Proportion (%)"),
             digits = c(0, 0, 0, 0, 1))

format_table(bottom_20_sc_reviews, 
             caption = "Bottom 20 Tags with Lowest Proportion of SC Reviews",
             col.names = c("Tag", "Games Count", "Total Reviews", "SC Reviews", "SC Proportion (%)"),
             digits = c(0, 0, 0, 0, 1))

# Visualize top 20 tags with highest SC review proportion
ggplot(top_20_sc_reviews, aes(x = reorder(Tag, SC_Proportion), y = SC_Proportion)) +
  geom_bar(stat = "identity", fill = brewer.pal(8, sequential_blue)[6]) +
  geom_text(aes(label = paste0(round(SC_Proportion, 1), "%")), hjust = -0.2, size = 3) +
  coord_flip() +
  labs(
    title = "Top 20 Tags with Highest Proportion of SC Reviews",
    subtitle = "Percentage of reviews in Simplified Chinese by tag",
    x = NULL,
    y = "SC Review Proportion (%)",
    caption = "Source: Steam API Data\nOnly includes tags with at least 200 total reviews"
  ) +
  my_theme +
  scale_y_continuous(limits = c(0, max(top_20_sc_reviews$SC_Proportion) * 1.1),
                     labels = function(x) paste0(round(x, 1), "%"))

# Visualize bottom 20 tags with lowest SC review proportion
ggplot(bottom_20_sc_reviews, aes(x = reorder(Tag, -SC_Proportion), y = SC_Proportion)) +
  geom_bar(stat = "identity", fill = brewer.pal(8, sequential_red)[6]) +
  geom_text(aes(label = paste0(round(SC_Proportion, 1), "%")), hjust = 1.2, size = 3, color = "white") +
  coord_flip() +
  labs(
    title = "Bottom 20 Tags with Lowest Proportion of SC Reviews",
    subtitle = "Percentage of reviews in Simplified Chinese by tag",
    x = NULL,
    y = "SC Review Proportion (%)",
    caption = "Source: Steam API Data\nOnly includes tags with at least 200 total reviews"
  ) +
  my_theme +
  scale_y_continuous(limits = c(0, max(bottom_20_sc_reviews$SC_Proportion) * 1.3),
                     labels = function(x) paste0(round(x, 1), "%"))

# -----------------------------
# 5. Cultural Preference Indicators
# -----------------------------

# Define minimum games required for analysis
min_games_for_analysis <- 50  # Minimum number of games with a tag for reliable analysis

# Calculate the sentiment gap by tag
tag_sentiment_gap <- steam_data %>%
  filter(english_reviews > 0 & schinese_reviews > 0) %>%  # Ensure enough reviews in both languages
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
# Select top 20 tags preferred by SC users (highest positive gap)
top_20_sc_preferred <- tag_sentiment_gap %>%
  arrange(desc(sc_higher_percent)) %>% 
  head(20)

# Select top 20 tags preferred by English users (highest negative gap)
top_20_en_preferred <- tag_sentiment_gap %>%
  arrange(sc_higher_percent) %>%  
  head(20)

# Display the tables with consistent formatting
format_table(top_20_sc_preferred %>% 
               select(tag, game_count, avg_sc_positive, avg_en_positive, avg_sentiment_gap, sc_higher_percent), 
             caption = "Top 20 Tags With Smallest SC-EN Sentiment Gap",
             col.names = c("Tag", "Games Count", "Avg SC Positive %", "Avg EN Positive %", 
                           "Sentiment Gap (pp)", "SC Higher %"),
             digits = c(0, 0, 1, 1, 1, 1))

format_table(top_20_en_preferred %>% 
               select(tag, game_count, avg_sc_positive, avg_en_positive, avg_sentiment_gap, sc_higher_percent), 
             caption = "Top 20 Tags With Largest SC-EN Sentiment Gap",
             col.names = c("Tag", "Games Count", "Avg SC Positive %", "Avg EN Positive %", 
                           "Sentiment Gap (pp)", "SC Higher %"),
             digits = c(0, 0, 1, 1, 1, 1))

# Create a more informative comparison visualisation on sentiment gap
language_comparison_vis <- bind_rows(
  # For tags most often preferred by English users
  top_20_en_preferred %>% 
    select(tag, avg_sc_positive, avg_en_positive) %>%
    head(15) %>% 
    mutate(group = "English Often Preferred",
           difference = avg_sc_positive - avg_en_positive) %>%
    arrange(difference),
  
  # For tags most often preferred by SC users
  top_20_sc_preferred %>% 
    select(tag, avg_sc_positive, avg_en_positive) %>%
    head(15) %>% 
    mutate(group = "SC Often Preferred",
           difference = avg_sc_positive - avg_en_positive) %>%
    arrange(desc(difference))
) %>%
  # Now pivot to long format for plotting
  pivot_longer(cols = c(avg_sc_positive, avg_en_positive),
               names_to = "language",
               values_to = "positive_ratio") %>%
  mutate(language = ifelse(language == "avg_sc_positive", "Simplified Chinese", "English"),
         # Create a factor version of tag that preserves our sorting
         tag_ordered = factor(tag, levels = unique(tag)))

# Create the chart
ggplot(language_comparison_vis, aes(x = tag_ordered, y = positive_ratio, fill = language)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  geom_text(aes(label = paste0(round(positive_ratio, 0), "%")),
            position = position_dodge(width = 0.8),
            hjust = -0.1,
            size = 3) +  # Standardized text size
  facet_wrap(~group, scales = "free_y", ncol = 2) +
  coord_flip() +
  scale_fill_brewer(palette = main_palette) +  
  scale_y_continuous(limits = c(0, 120), 
                     breaks = seq(0, 100, 25),
                     labels = function(x) paste0(x, "%")) +  # Consistent percentage formatting
  labs(
    title = "Comparison of Positive Review Ratios by Language",
    subtitle = "For tags grouped by review preference patterns (sorted by sentiment gap)",
    x = NULL,
    y = "Positive Review Ratio (%)",
    fill = "Language",
    caption = "Source: Steam API Data"
  ) +
  facet_theme() +
  theme(
    plot.margin = margin(1, 4, 1, 1, "cm"),  # Increased right margin from default to 4cm
    plot.title = element_text(margin = margin(b = 10))  # Add margin below title
  )

# Calculate the proportion of SC reviews for each tag for the cultural scatter plot
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
  filter(total_reviews_sum >= 200) %>%  # Ensure enough reviews for reliable stats
  arrange(desc(aggregate_sc_proportion))

# Join the sentiment gap and review proportion data
tag_cultural_comparison <- tag_sentiment_gap %>%
  inner_join(tag_sc_review_proportion, by = "tag") %>%
  select(tag, game_count.x, avg_sentiment_gap, aggregate_sc_proportion) %>%
  rename(game_count = game_count.x)

# Find the extreme tags for labeling in the scatter plot
extreme_tags <- bind_rows(
  # Tags with high SC proportion and large sentiment gaps (either direction)
  tag_cultural_comparison %>% 
    arrange(desc(aggregate_sc_proportion)) %>% 
    head(5),
  tag_cultural_comparison %>% 
    arrange(desc(abs(avg_sentiment_gap))) %>% 
    head(10)
) %>% distinct(tag, .keep_all = TRUE)

# Set the minimum x limit to avoid whitespace
min_x_limit <- max(0, min(tag_cultural_comparison$aggregate_sc_proportion, na.rm = TRUE) - 5)

# Create the cultural scatter plot with consistent styling
ggplot(tag_cultural_comparison, aes(x = aggregate_sc_proportion, y = avg_sentiment_gap)) +
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
  scale_x_continuous(
    name = "Percentage of Reviews in Simplified Chinese",
    labels = function(x) paste0(x, "%"),
    limits = c(min_x_limit, max(tag_cultural_comparison$aggregate_sc_proportion, na.rm = TRUE) * 1.05),
    breaks = function(limits) {
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
  my_theme +
  theme(
    legend.position = "right",
    panel.grid.minor = element_line(color = "lightgray", linetype = "dotted"),
    panel.grid.major = element_line(color = "lightgray")
  )

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

# Display the quadrant counts with consistent formatting
format_table(quadrant_counts, 
             caption = "Distribution of Tags Across Cultural Preference Quadrants",
             col.names = c("Preference Pattern", "Count", "Percentage"),
             digits = c(0, 0, 1))

# Create a more detailed quadrant visualization with consistent styling
ggplot(tag_cultural_comparison, aes(x = aggregate_sc_proportion, y = avg_sentiment_gap)) +
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
  scale_color_brewer(name = "Preference Pattern", palette = main_palette) +
  scale_x_continuous(
    name = "Percentage of Reviews in Simplified Chinese",
    labels = function(x) paste0(x, "%"),
    limits = c(min_x_limit, max(tag_cultural_comparison$aggregate_sc_proportion, na.rm = TRUE) * 1.05),
    breaks = function(limits) {
      seq(floor(limits[1]), ceiling(limits[2]), by = 5)
    }
  ) +
  scale_y_continuous(
    name = "Sentiment Gap (SC - English) in Percentage Points",
    labels = function(y) paste0(y, "%")
  ) +
  # Add titles and theme
  labs(
    title = "Cultural Preference Quadrant Analysis",
    subtitle = "Mapping game tags by engagement patterns and sentiment differences",
    caption = "Source: Steam API Data\nMean lines create four preference quadrants"
  ) +
  my_theme +
  theme(
    legend.position = "right",
    # Add more space around the plot to prevent text clipping
    plot.margin = margin(1, 1, 1, 1, "cm")
  )
```