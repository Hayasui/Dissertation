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
# 1. Number of games with any form of SC/TC localisation compared to English
# -----------------------------

# Create flags for any level of localisation support
loc_summary <- steam_data %>%
  summarise(
    Total_Games = n(),
    SC_Any = sum(chinese_simplified_interface_subtitles == TRUE | 
                   chinese_simplified_audio == TRUE, na.rm = TRUE),
    TC_Any = sum(chinese_traditional_interface_subtitles == TRUE | 
                   chinese_traditional_audio == TRUE, na.rm = TRUE),
    English_Any = sum(english_interface_subtitles == TRUE | 
                        english_audio == TRUE, na.rm = TRUE)
  ) %>%
  mutate(
    SC_Percentage = SC_Any / Total_Games * 100,
    TC_Percentage = TC_Any / Total_Games * 100,
    English_Percentage = English_Any / Total_Games * 100
  )

# Display the summary
print(loc_summary)

# Convert to long format for visualization
loc_summary_long <- loc_summary %>%
  select(SC_Any, TC_Any, English_Any) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Language",
    values_to = "Count"
  ) %>%
  mutate(
    Language = case_when(
      Language == "SC_Any" ~ "Simplified Chinese",
      Language == "TC_Any" ~ "Traditional Chinese",
      Language == "English_Any" ~ "English"
    ),
    Percentage = case_when(
      Language == "Simplified Chinese" ~ loc_summary$SC_Percentage,
      Language == "Traditional Chinese" ~ loc_summary$TC_Percentage,
      Language == "English" ~ loc_summary$English_Percentage
    )
  )

# Visualise localisation support comparison
ggplot(loc_summary_long, aes(x = reorder(Language, -Count), y = Count, fill = Language)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(Count, " (", round(Percentage, 1), "%)")), 
            vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Games with Language Support by Type",
    subtitle = "Comparison of English, Simplified Chinese, and Traditional Chinese",
    x = "Language",
    y = "Number of Games",
    caption = "Source: Steam API Data"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "none"
  )

# -----------------------------
# 2. Detailed localisation breakdown
# -----------------------------

# Create a table with languages as rows and support levels as columns
support_breakdown <- data.frame(
  Language = c("English", "Simplified Chinese", "Traditional Chinese"),
  
  # 1. No support at all
  No_Support_Count = c(
    sum(steam_data$english_interface_subtitles == FALSE & steam_data$english_audio == FALSE, na.rm = TRUE),
    sum(steam_data$chinese_simplified_interface_subtitles == FALSE & steam_data$chinese_simplified_audio == FALSE, na.rm = TRUE),
    sum(steam_data$chinese_traditional_interface_subtitles == FALSE & steam_data$chinese_traditional_audio == FALSE, na.rm = TRUE)
  ),
  
  # 2. Has interface/subtitles (regardless of audio)
  Has_Interface_Count = c(
    sum(steam_data$english_interface_subtitles == TRUE, na.rm = TRUE),
    sum(steam_data$chinese_simplified_interface_subtitles == TRUE, na.rm = TRUE),
    sum(steam_data$chinese_traditional_interface_subtitles == TRUE, na.rm = TRUE)
  ),
  
  # 3. Among those with interface/subtitles, how many also have dubbing
  Has_Dubbing_Count = c(
    sum(steam_data$english_audio == TRUE, na.rm = TRUE),
    sum(steam_data$chinese_simplified_audio == TRUE, na.rm = TRUE),
    sum(steam_data$chinese_traditional_audio == TRUE, na.rm = TRUE)
  )
)

# Calculate percentages based on total games
total_games <- nrow(steam_data)
support_breakdown <- support_breakdown %>%
  mutate(
    No_Support_Pct = No_Support_Count / total_games * 100,
    Has_Interface_Pct = Has_Interface_Count / total_games * 100,
    Has_Dubbing_Pct = Has_Dubbing_Count / total_games * 100
  )

# Display the table
kable(support_breakdown, 
      caption = "Language Support Breakdown",
      digits = 1)

# Create a cleaner version for visualization purposes
support_pct <- support_breakdown %>%
  select(Language, No_Support_Pct, Has_Interface_Pct, Has_Dubbing_Pct)

# Create a visualization for these percentages
support_pct_long <- support_pct %>%
  pivot_longer(
    cols = c(No_Support_Pct, Has_Interface_Pct, Has_Dubbing_Pct),
    names_to = "Support_Type",
    values_to = "Percentage"
  ) %>%
  mutate(Support_Type = factor(
    case_when(
      Support_Type == "No_Support_Pct" ~ "1. No Language Support",
      Support_Type == "Has_Interface_Pct" ~ "2. Has Interface/Subtitles",
      Support_Type == "Has_Dubbing_Pct" ~ "3. Has Dubbing Support"
    ),
    levels = c("1. No Language Support", "2. Has Interface/Subtitles", "3. Has Dubbing Support")
  ))

# Create grouped bar chart
ggplot(support_pct_long, aes(x = Language, y = Percentage, fill = Support_Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  scale_fill_brewer(palette = "Blues") +
  labs(
    title = "Language Support Breakdown by Type",
    subtitle = "Categories are not mutually exclusive",
    x = "Language",
    y = "Percentage of Games",
    fill = "Support Level",
    caption = "Source: Steam API Data"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold")
  ) +
  scale_y_continuous(limits = c(0, 100))

# Create a heatmap version to show the percentage differences more clearly
ggplot(support_pct_long, aes(x = Support_Type, y = Language, fill = Percentage)) +
  geom_tile() +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), color = "white", fontface = "bold") +
  scale_fill_gradient(low = "steelblue", high = "darkblue") +
  labs(
    title = "Language Support Breakdown Heatmap",
    x = "Support Level",
    y = "Language",
    fill = "Percentage",
    caption = "Source: Steam API Data"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# -----------------------------
# 3. Chinese Localisation trends by year
# -----------------------------

# Create combined Chinese support categories
steam_data <- steam_data %>%
  mutate(
    chinese_support_category = case_when(
      (chinese_simplified_interface_subtitles == TRUE | chinese_simplified_audio == TRUE) &
        (chinese_traditional_interface_subtitles == TRUE | chinese_traditional_audio == TRUE) ~ "Both SC and TC",
      (chinese_simplified_interface_subtitles == TRUE | chinese_simplified_audio == TRUE) ~ "SC Only",
      (chinese_traditional_interface_subtitles == TRUE | chinese_traditional_audio == TRUE) ~ "TC Only",
      TRUE ~ "No Chinese Support"
    )
  )

# Count games by Chinese support category and year
chinese_support_by_year <- steam_data %>%
  filter(!is.na(release_year)) %>%
  group_by(release_year) %>%
  summarise(
    Total = n(),
    SC_Only = sum(chinese_support_category == "SC Only", na.rm = TRUE),
    TC_Only = sum(chinese_support_category == "TC Only", na.rm = TRUE),
    Both = sum(chinese_support_category == "Both SC and TC", na.rm = TRUE)
  ) %>%
  mutate(
    Any_Chinese = SC_Only + TC_Only + Both,
    SC_Only_Ratio = SC_Only / Total * 100,
    TC_Only_Ratio = TC_Only / Total * 100,
    Both_Ratio = Both / Total * 100,
    Any_Chinese_Ratio = Any_Chinese / Total * 100
  )

# Display the summary by year
print(chinese_support_by_year)

# Visualize absolute numbers by year
chinese_support_long <- chinese_support_by_year %>%
  select(release_year, SC_Only, TC_Only, Both) %>%
  pivot_longer(
    cols = c(SC_Only, TC_Only, Both),
    names_to = "Support_Type",
    values_to = "Count"
  ) %>%
  mutate(Support_Type = factor(Support_Type, 
                               levels = c("Both", "SC_Only", "TC_Only"),
                               labels = c("Both SC and TC", "SC Only", "TC Only")))

# Stacked bar chart of absolute numbers
ggplot(chinese_support_long, aes(x = release_year, y = Count, fill = Support_Type)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Chinese Localisation Adoption Trends by Year",
    subtitle = "Number of Games with Chinese Support",
    x = "Year",
    y = "Number of Games",
    fill = "Support Type",
    caption = "Source: Steam API Data"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_x_continuous(breaks = seq(min(chinese_support_by_year$release_year), 
                                  max(chinese_support_by_year$release_year), by = 1))

# Line chart showing the ratio trends
ratio_long <- chinese_support_by_year %>%
  select(release_year, SC_Only_Ratio, TC_Only_Ratio, Both_Ratio, Any_Chinese_Ratio) %>%
  pivot_longer(
    cols = c(SC_Only_Ratio, TC_Only_Ratio, Both_Ratio, Any_Chinese_Ratio),
    names_to = "Type",
    values_to = "Ratio"
  ) %>%
  mutate(Type = factor(Type, 
                       levels = c("Any_Chinese_Ratio", "Both_Ratio", "SC_Only_Ratio", "TC_Only_Ratio"),
                       labels = c("Any Chinese", "Both SC and TC", "SC Only", "TC Only")))

# Line chart of ratio trends
ggplot(ratio_long, aes(x = release_year, y = Ratio, color = Type, group = Type)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "Chinese Localisation Adoption Ratio Trends",
    subtitle = "Percentage of Games with Chinese Support by Year",
    x = "Year",
    y = "Percentage of Games",
    color = "Support Type",
    caption = "Source: Steam API Data"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_x_continuous(breaks = seq(min(chinese_support_by_year$release_year), 
                                  max(chinese_support_by_year$release_year), by = 1)) +
  scale_y_continuous(labels = function(x) paste0(round(x, 1), "%"))

# -----------------------------
# 3.5. Ratio of Chinese Localized Games to Total Games by Year
# -----------------------------

# Create a focused visualization of Chinese localization ratio by year
ggplot(chinese_support_by_year, aes(x = release_year, y = Any_Chinese_Ratio)) +
  geom_bar(stat = "identity", fill = brewer.pal(9, "Blues")[6]) +
  geom_text(aes(label = paste0(round(Any_Chinese_Ratio, 1), "%")), 
            vjust = -0.5, size = 3.5) +
  labs(
    title = "Ratio of Chinese Localized Games to Total Games by Year",
    subtitle = "Percentage of Steam Games with Any Chinese Support (SC or TC)",
    x = "Release Year",
    y = "Percentage of Games",
    caption = "Source: Steam API Data"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_x_continuous(breaks = seq(min(chinese_support_by_year$release_year), 
                                  max(chinese_support_by_year$release_year), by = 1)) +
  scale_y_continuous(limits = c(0, max(chinese_support_by_year$Any_Chinese_Ratio) * 1.1),
                     labels = function(x) paste0(round(x, 0), "%"))

# Also create a dual-axis chart to compare total games and localized games trends
ggplot(chinese_support_by_year) +
  geom_col(aes(x = release_year, y = Total), fill = "lightgrey", alpha = 0.7) +
  geom_line(aes(x = release_year, y = Any_Chinese * 5), 
            color = brewer.pal(9, "Reds")[7], size = 1.5, group = 1) +
  geom_point(aes(x = release_year, y = Any_Chinese * 5), 
             color = brewer.pal(9, "Reds")[7], size = 3) +
  scale_y_continuous(
    name = "Total Games Released",
    sec.axis = sec_axis(~./5, name = "Games with Chinese Localization")
  ) +
  labs(
    title = "Volume of Chinese Localized Games vs Total Releases by Year",
    subtitle = "Comparing growth trends of games with Chinese support against overall releases",
    x = "Release Year",
    caption = "Source: Steam API Data"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.y.left = element_text(color = "grey30"),
    axis.title.y.right = element_text(color = brewer.pal(9, "Reds")[7])
  ) +
  scale_x_continuous(breaks = seq(min(chinese_support_by_year$release_year), 
                                  max(chinese_support_by_year$release_year), by = 1)) +
  geom_text(aes(x = release_year, y = Any_Chinese * 5, 
                label = Any_Chinese), 
            vjust = -1, color = brewer.pal(9, "Reds")[7], size = 3)

# Create a table showing the raw numbers and percentages by year
localization_summary_table <- chinese_support_by_year %>%
  select(release_year, Total, Any_Chinese, Any_Chinese_Ratio) %>%
  arrange(desc(release_year))

# Display the table
kable(localization_summary_table, 
      col.names = c("Year", "Total Games", "Games with Chinese", "% with Chinese"),
      caption = "Chinese Localization Adoption by Year",
      digits = c(0, 0, 0, 1))

# -----------------------------
# 4. Localisation by game 'volume': Indie vs. Non-Indie
# -----------------------------

# First, let's examine what the genres actually look like
print(head(steam_data$genres, 10))

# Create a function to check if "Indie" is in the genres string
has_indie <- function(genre_str) {
  return(grepl("'Indie'", genre_str, fixed = TRUE))
}

# Apply this function to determine if a game is indie
steam_data <- steam_data %>%
  mutate(
    has_indie_genre = has_indie(genres),
    any_chinese = chinese_simplified_interface_subtitles == TRUE | 
      chinese_simplified_audio == TRUE | 
      chinese_traditional_interface_subtitles == TRUE | 
      chinese_traditional_audio == TRUE
  )

# Verify our approach is working
table(steam_data$has_indie_genre, useNA = "ifany")

# Compare Chinese localisation between indie and non-indie games
indie_localisation <- steam_data %>%
  group_by(has_indie_genre) %>%
  summarise(
    Total = n(),
    With_Chinese = sum(any_chinese, na.rm = TRUE),
    SC_Interface = sum(chinese_simplified_interface_subtitles == TRUE, na.rm = TRUE),
    SC_Audio = sum(chinese_simplified_audio == TRUE, na.rm = TRUE),
    TC_Interface = sum(chinese_traditional_interface_subtitles == TRUE, na.rm = TRUE),
    TC_Audio = sum(chinese_traditional_audio == TRUE, na.rm = TRUE)
  ) %>%
  mutate(
    Chinese_Ratio = With_Chinese / Total * 100,
    SC_Interface_Ratio = SC_Interface / Total * 100,
    SC_Audio_Ratio = SC_Audio / Total * 100,
    TC_Interface_Ratio = TC_Interface / Total * 100,
    TC_Audio_Ratio = TC_Audio / Total * 100
  )

# Display the indie vs. non-indie comparison
print(indie_localisation)

# Rename the indie genre for better readability
indie_localisation <- indie_localisation %>%
  mutate(Game_Type = ifelse(has_indie_genre, "Indie Games", "Non-Indie Games"))

# Create a grouped bar chart for the comparison
indie_loc_long <- indie_localisation %>%
  select(Game_Type, Chinese_Ratio, SC_Interface_Ratio, SC_Audio_Ratio, 
         TC_Interface_Ratio, TC_Audio_Ratio) %>%
  pivot_longer(
    cols = -Game_Type,
    names_to = "Localisation_Type",
    values_to = "Percentage"
  ) %>%
  mutate(Localisation_Type = case_when(
    Localisation_Type == "Chinese_Ratio" ~ "Any Chinese",
    Localisation_Type == "SC_Interface_Ratio" ~ "SC Interface/Subtitles",
    Localisation_Type == "SC_Audio_Ratio" ~ "SC Audio",
    Localisation_Type == "TC_Interface_Ratio" ~ "TC Interface/Subtitles",
    Localisation_Type == "TC_Audio_Ratio" ~ "TC Audio"
  ))

# Grouped bar chart comparing indie vs. non-indie localisation
ggplot(indie_loc_long, aes(x = Localisation_Type, y = Percentage, fill = Game_Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Chinese Localisation by Game Type",
    subtitle = "Comparison between Indie and Non-Indie Games",
    x = "Localisation Type",
    y = "Percentage of Games",
    fill = "Game Type",
    caption = "Source: Steam API Data"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_y_continuous(labels = function(x) paste0(round(x, 0), "%"))

# Additional analysis: Localisation trends for indie vs. non-indie games over time
indie_by_year <- steam_data %>%
  filter(!is.na(release_year)) %>%
  group_by(release_year, has_indie_genre) %>%
  summarise(
    Total = n(),
    With_Chinese = sum(any_chinese, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Chinese_Ratio = With_Chinese / Total * 100,
    Game_Type = ifelse(has_indie_genre, "Indie Games", "Non-Indie Games")
  )

# Filter to include only years with sufficient data for both types (at least 10 games)
year_type_counts <- indie_by_year %>%
  group_by(release_year, Game_Type) %>%
  summarise(count = first(Total), .groups = "drop")

valid_years <- year_type_counts %>%
  group_by(release_year) %>%
  filter(n() == 2) %>%
  filter(all(count >= 10)) %>%
  pull(release_year) %>%
  unique()

indie_by_year_filtered <- indie_by_year %>%
  filter(release_year %in% valid_years)

# Line chart showing localisation trends for indie vs. non-indie over time
ggplot(indie_by_year_filtered, aes(x = release_year, y = Chinese_Ratio, 
                                   color = Game_Type, group = Game_Type)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "Chinese Localisation Trends: Indie vs. Non-Indie Games",
    subtitle = "Percentage of Games with Chinese Support by Year",
    x = "Year",
    y = "Percentage of Games with Chinese Support",
    color = "Game Type",
    caption = "Source: Steam API Data"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_x_continuous(breaks = seq(min(indie_by_year_filtered$release_year), 
                                  max(indie_by_year_filtered$release_year), by = 1)) +
  scale_y_continuous(labels = function(x) paste0(round(x, 1), "%"))
