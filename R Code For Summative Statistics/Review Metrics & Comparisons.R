# Load necessary libraries
library(tidyverse)    # For data manipulation and ggplot2
library(RColorBrewer) # For colour palettes
library(lubridate)    # For date handling
library(scales)       # For better axis formatting
library(knitr)        # For table formatting

# Read the RDS file
steam_data <- readRDS("steam_games_data_updated.rds")

# Extract the year from the release date if not already done
if(!"release_year" %in% colnames(steam_data)) {
  steam_data$release_year <- year(as.Date(steam_data$release_date))
}

# -----------------------------
# 1. Review distribution: Proportion of total reviews in SC vs. English vs. other languages by years
# -----------------------------

# Create a summary of review counts by language and year
review_dist_by_year <- steam_data %>%
  filter(!is.na(release_year)) %>%
  group_by(release_year) %>%
  summarise(
    Total_Reviews = sum(total_reviews, na.rm = TRUE),
    SC_Reviews = sum(schinese_reviews, na.rm = TRUE),
    English_Reviews = sum(english_reviews, na.rm = TRUE),
    Other_Reviews = sum(other_reviews, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    SC_Proportion = SC_Reviews / Total_Reviews * 100,
    English_Proportion = English_Reviews / Total_Reviews * 100,
    Other_Proportion = Other_Reviews / Total_Reviews * 100
  )

# Display the summary table
print(review_dist_by_year)

# Convert to long format for visualisation
review_dist_long <- review_dist_by_year %>%
  select(release_year, SC_Proportion, English_Proportion, Other_Proportion) %>%
  pivot_longer(
    cols = c(SC_Proportion, English_Proportion, Other_Proportion),
    names_to = "Language",
    values_to = "Proportion"
  ) %>%
  mutate(Language = case_when(
    Language == "SC_Proportion" ~ "Simplified Chinese",
    Language == "English_Proportion" ~ "English",
    Language == "Other_Proportion" ~ "Other Languages"
  ))

# Create a stacked area chart to show proportions over time
ggplot(review_dist_long, aes(x = release_year, y = Proportion, fill = Language)) +
  geom_area(position = "stack") +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Proportion of Reviews by Language Over Time",
    subtitle = "Distribution of Steam reviews across language groups",
    x = "Year",
    y = "Percentage of Total Reviews",
    fill = "Language",
    caption = "Source: Steam API Data"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_x_continuous(breaks = seq(min(review_dist_by_year$release_year), 
                                  max(review_dist_by_year$release_year), by = 1)) +
  scale_y_continuous(labels = function(x) paste0(round(x, 0), "%"))

# Also create a line chart to show the trend more clearly
ggplot(review_dist_long, aes(x = release_year, y = Proportion, color = Language, group = Language)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "Proportion of Reviews by Language Over Time",
    subtitle = "Trends in language distribution of Steam reviews",
    x = "Year",
    y = "Percentage of Total Reviews",
    color = "Language",
    caption = "Source: Steam API Data"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_x_continuous(breaks = seq(min(review_dist_by_year$release_year), 
                                  max(review_dist_by_year$release_year), by = 1)) +
  scale_y_continuous(labels = function(x) paste0(round(x, 0), "%"))

# -----------------------------
# 2. Review volume comparison: Average number of reviews per game in English vs. SC by years
# -----------------------------

# Calculate average reviews per game by language and year
review_volume_by_year <- steam_data %>%
  filter(!is.na(release_year)) %>%
  group_by(release_year) %>%
  summarise(
    Total_Games = n(),
    Avg_Total_Reviews = mean(total_reviews, na.rm = TRUE),
    Avg_SC_Reviews = mean(schinese_reviews, na.rm = TRUE),
    Avg_English_Reviews = mean(english_reviews, na.rm = TRUE),
    Avg_Other_Reviews = mean(other_reviews, na.rm = TRUE),
    .groups = "drop"
  )

# Display the summary table
print(review_volume_by_year)

# Convert to long format for visualisation
review_volume_long <- review_volume_by_year %>%
  select(release_year, Avg_SC_Reviews, Avg_English_Reviews, Avg_Other_Reviews) %>%
  pivot_longer(
    cols = c(Avg_SC_Reviews, Avg_English_Reviews, Avg_Other_Reviews),
    names_to = "Language",
    values_to = "Average_Reviews"
  ) %>%
  mutate(Language = case_when(
    Language == "Avg_SC_Reviews" ~ "Simplified Chinese",
    Language == "Avg_English_Reviews" ~ "English",
    Language == "Avg_Other_Reviews" ~ "Other Languages"
  ))

# Create a grouped bar chart for average review volume by language and year
ggplot(review_volume_long, aes(x = release_year, y = Average_Reviews, fill = Language)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Average Review Volume per Game by Language",
    subtitle = "Comparing review engagement across language groups",
    x = "Year",
    y = "Average Reviews per Game",
    fill = "Language",
    caption = "Source: Steam API Data"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_x_continuous(breaks = seq(min(review_volume_by_year$release_year), 
                                  max(review_volume_by_year$release_year), by = 1))

# Also create a line chart to show the trend more clearly
ggplot(review_volume_long, aes(x = release_year, y = Average_Reviews, color = Language, group = Language)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "Average Review Volume per Game by Language Over Time",
    subtitle = "Trends in language-specific review engagement",
    x = "Year",
    y = "Average Reviews per Game",
    color = "Language",
    caption = "Source: Steam API Data"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_x_continuous(breaks = seq(min(review_volume_by_year$release_year), 
                                  max(review_volume_by_year$release_year), by = 1))

# -----------------------------
# 3. Sentiment gap analysis: Average difference between positive review ratios in SC vs. English for the same games by years
# -----------------------------

# Calculate the sentiment gap by game and year, with manual ratio calculation
sentiment_gap_by_game <- steam_data %>%
  filter(!is.na(release_year)) %>%
  filter(schinese_reviews > 0 & english_reviews > 0) %>%  # Ensure both languages have reviews
  mutate(
    # Calculate positive ratios manually to ensure consistency
    sc_positive_ratio_calc = schinese_positive / schinese_reviews,
    english_positive_ratio_calc = english_positive / english_reviews,
    sentiment_gap = sc_positive_ratio_calc - english_positive_ratio_calc
  )

# Check that calculations match the dataset's provided ratios
sentiment_check <- sentiment_gap_by_game %>%
  summarise(
    avg_sc_ratio_provided = mean(schinese_positive_ratio, na.rm = TRUE),
    avg_sc_ratio_calculated = mean(sc_positive_ratio_calc, na.rm = TRUE),
    avg_eng_ratio_provided = mean(english_positive_ratio, na.rm = TRUE),
    avg_eng_ratio_calculated = mean(english_positive_ratio_calc, na.rm = TRUE)
  )

print("Validation of manual calculations vs. provided ratios:")
print(sentiment_check)

# Summarise the sentiment gap by year
sentiment_gap_by_year <- sentiment_gap_by_game %>%
  group_by(release_year) %>%
  summarise(
    Games_With_Both = n(),
    Avg_Sentiment_Gap = mean(sentiment_gap, na.rm = TRUE),
    Median_Sentiment_Gap = median(sentiment_gap, na.rm = TRUE),
    SC_Higher_Count = sum(sentiment_gap > 0, na.rm = TRUE),
    English_Higher_Count = sum(sentiment_gap < 0, na.rm = TRUE),
    Equal_Count = sum(sentiment_gap == 0, na.rm = TRUE),
    Avg_SC_Positive_Ratio = mean(sc_positive_ratio_calc, na.rm = TRUE),
    Avg_English_Positive_Ratio = mean(english_positive_ratio_calc, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    SC_Higher_Pct = SC_Higher_Count / Games_With_Both * 100,
    English_Higher_Pct = English_Higher_Count / Games_With_Both * 100,
    Equal_Pct = Equal_Count / Games_With_Both * 100
  )

# Display the summary table
print(sentiment_gap_by_year)

# Create an bar chart for the average sentiment gap by year
ggplot(sentiment_gap_by_year, aes(x = release_year, y = Avg_Sentiment_Gap * 100)) +
  geom_bar(stat = "identity", 
           fill = ifelse(sentiment_gap_by_year$Avg_Sentiment_Gap >= 0, brewer.pal(9, "Blues")[6], brewer.pal(9, "Reds")[6])) +
  geom_text(aes(
    label = paste0(round(Avg_Sentiment_Gap * 100, 1), "%"),
    vjust = ifelse(Avg_Sentiment_Gap >= 0, -0.5, 1.5)
  ),
  color = "black",  # Use black text for all labels
  fontface = "bold", # Make text bold
  size = 3.5) +  # Slightly increase text size
  labs(
    title = "Average Sentiment Gap Between SC and English Reviews",
    subtitle = "Positive values indicate SC reviews are more positive on average (manually calculated ratios)",
    x = "Year",
    y = "Average Sentiment Gap (Percentage Points)",
    caption = "Source: Steam API Data\nNote: Only includes games with both SC and English reviews"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_x_continuous(breaks = seq(min(sentiment_gap_by_year$release_year), 
                                  max(sentiment_gap_by_year$release_year), by = 1)) +
  scale_y_continuous(limits = c(min(sentiment_gap_by_year$Avg_Sentiment_Gap * 100) - 5, 
                                max(sentiment_gap_by_year$Avg_Sentiment_Gap * 100) + 5))

# Create a visualisation showing the percentage of games where each language has higher sentiment
sentiment_pct_long <- sentiment_gap_by_year %>%
  select(release_year, SC_Higher_Pct, English_Higher_Pct, Equal_Pct) %>%
  pivot_longer(
    cols = c(SC_Higher_Pct, English_Higher_Pct, Equal_Pct),
    names_to = "Comparison",
    values_to = "Percentage"
  ) %>%
  mutate(Comparison = case_when(
    Comparison == "SC_Higher_Pct" ~ "SC More Positive",
    Comparison == "English_Higher_Pct" ~ "English More Positive",
    Comparison == "Equal_Pct" ~ "Equal Sentiment"
  ))

# Create a stacked bar chart for the percentage breakdown
ggplot(sentiment_pct_long, aes(x = release_year, y = Percentage, fill = Comparison)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Distribution of Sentiment Comparison Between Languages",
    subtitle = "Percentage of games where each language has more positive reviews (manually calculated)",
    x = "Year",
    y = "Percentage of Games",
    fill = "Comparison Result",
    caption = "Source: Steam API Data\nNote: Only includes games with both SC and English reviews"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_x_continuous(breaks = seq(min(sentiment_gap_by_year$release_year), 
                                  max(sentiment_gap_by_year$release_year), by = 1)) +
  scale_y_continuous(labels = function(x) paste0(round(x, 0), "%"))

# Additional analysis: Compare manually calculated average positive ratios by language and year
avg_sentiment_long <- sentiment_gap_by_year %>%
  select(release_year, Avg_SC_Positive_Ratio, Avg_English_Positive_Ratio) %>%
  pivot_longer(
    cols = c(Avg_SC_Positive_Ratio, Avg_English_Positive_Ratio),
    names_to = "Language",
    values_to = "Positive_Ratio"
  ) %>%
  mutate(
    Language = case_when(
      Language == "Avg_SC_Positive_Ratio" ~ "Simplified Chinese",
      Language == "Avg_English_Positive_Ratio" ~ "English"
    ),
    Positive_Percent = Positive_Ratio * 100
  )

# Create a line chart comparing average positive ratios by language
ggplot(avg_sentiment_long, aes(x = release_year, y = Positive_Percent, 
                               color = Language, group = Language)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "Average Positive Review Ratio by Language",
    subtitle = "Comparing manually calculated sentiment between SC and English reviews",
    x = "Year",
    y = "Average Positive Review Percentage",
    color = "Language",
    caption = "Source: Steam API Data\nNote: Only includes games with both SC and English reviews"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_x_continuous(breaks = seq(min(sentiment_gap_by_year$release_year), 
                                  max(sentiment_gap_by_year$release_year), by = 1)) +
  scale_y_continuous(limits = c(0, 100),
                     labels = function(x) paste0(round(x, 0), "%"))

# Create a boxplot to show the distribution of sentiment gaps by year
ggplot(sentiment_gap_by_game, aes(x = factor(release_year), y = sentiment_gap * 100)) +
  geom_boxplot(fill = brewer.pal(9, "Blues")[3]) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Distribution of Sentiment Gaps Between SC and English Reviews by Year",
    subtitle = "Positive values indicate SC reviews are more positive (manually calculated)",
    x = "Year",
    y = "Sentiment Gap (SC - English) in Percentage Points",
    caption = "Source: Steam API Data\nNote: Only includes games with both SC and English reviews"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
