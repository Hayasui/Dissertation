# Load necessary libraries
library(tidyverse)    # For data manipulation and ggplot2
library(RColorBrewer) # For colour palettes
library(lubridate)    # For date handling
library(scales)       # For better axis formatting
library(knitr)        # For table formatting

# Define unified color palettes for consistent visualization
# We'll use these palettes throughout the script
main_palette <- "Set1"      # For primary categorical variables
alt_palette <- "Set2"       # For secondary categorical variables
sequential_blue <- "Blues"  # For sequential data (blue shades)
sequential_red <- "Reds"    # For alternative sequential data (red shades)
divergent_palette <- "RdBu" # For diverging data (positive/negative)

# Custom colors for currency comparison
currency_colors <- c(
  "USD Price" = "#377EB8",       # Blue from Set1
  "CNY Price" = "#E41A1C",       # Red from Set1
  "CNY Price (in USD)" = "#4DAF4A" # Green from Set1
)

# Custom colors for price type comparison
price_type_colors <- c(
  "Average Price" = "#E41A1C",   # Red from Set1
  "Median Price" = "#FB9A99"     # Light red
)

# Read the RDS file
steam_data <- readRDS("steam_games_data_updated.rds")

# Extract the year from the release date if not already done
if(!"release_year" %in% colnames(steam_data)) {
  steam_data$release_year <- year(as.Date(steam_data$release_date))
}

# Convert price columns to numeric for analysis, handling "Free" and "Unknown" values
steam_data <- steam_data %>%
  mutate(
    price_usd_numeric = case_when(
      price_usd == "Free" | is_free == TRUE ~ 0,
      price_usd == "Unknown" ~ NA_real_,
      TRUE ~ as.numeric(price_usd)
    ),
    price_cny_numeric = case_when(
      price_cny == "Free" | is_free == TRUE ~ 0,
      price_cny == "Unknown" ~ NA_real_,
      TRUE ~ as.numeric(price_cny)
    )
  )

# -----------------------------
# 1. Regional price comparison: Average USD vs. CNY pricing by years
# -----------------------------

# Check maximum prices and number of games with unknown prices
max_prices <- steam_data %>%
  summarise(
    Max_USD = max(price_usd_numeric, na.rm = TRUE),
    Max_CNY = max(price_cny_numeric, na.rm = TRUE),
    Unknown_USD = sum(price_usd == "Unknown", na.rm = TRUE),
    Unknown_CNY = sum(price_cny == "Unknown", na.rm = TRUE),
    Free_Games = sum(is_free == TRUE, na.rm = TRUE)
  )

# Display the summary
cat("Maximum price in USD:", max_prices$Max_USD, "\n")
cat("Maximum price in CNY:", max_prices$Max_CNY, "\n")
cat("Games with unknown USD price:", max_prices$Unknown_USD, "\n")
cat("Games with unknown CNY price:", max_prices$Unknown_CNY, "\n")
cat("Free-to-play games:", max_prices$Free_Games, "\n")

# Calculate average prices by year (excluding free games for a clearer price trend)
avg_price_by_year <- steam_data %>%
  filter(!is.na(release_year) & is_free == FALSE) %>%
  filter(!is.na(price_usd_numeric) & !is.na(price_cny_numeric)) %>%
  group_by(release_year) %>%
  summarise(
    Games_Count = n(),
    Avg_USD = mean(price_usd_numeric, na.rm = TRUE),
    Median_USD = median(price_usd_numeric, na.rm = TRUE),
    Avg_CNY = mean(price_cny_numeric, na.rm = TRUE),
    Median_CNY = median(price_cny_numeric, na.rm = TRUE),
    # Calculate approximate exchange rate (CNY/USD)
    Avg_Exchange_Rate = mean(price_cny_numeric / price_usd_numeric, na.rm = TRUE),
    .groups = "drop"
  )

# Display the table
kable(avg_price_by_year, 
      caption = "Average and Median Prices by Year (Paid Games Only)",
      digits = c(0, 0, 2, 2, 2, 2, 2))

# Create the dual-axis chart with three lines - USING UNIFIED COLORS
ggplot(avg_price_by_year) +
  # USD price line
  geom_line(aes(x = release_year, y = Avg_USD, color = "USD Price"), size = 1, group = 1) +
  geom_point(aes(x = release_year, y = Avg_USD, color = "USD Price"), size = 3) +
  # CNY converted to USD line (on USD axis)
  geom_line(aes(x = release_year, y = Avg_CNY/7, color = "CNY Price (in USD)"), size = 1, group = 1, linetype = "dashed") +
  geom_point(aes(x = release_year, y = Avg_CNY/7, color = "CNY Price (in USD)"), size = 3) +
  # Original CNY line (on CNY axis)
  geom_line(aes(x = release_year, y = Avg_CNY/4, color = "CNY Price"), size = 1, group = 1) +
  geom_point(aes(x = release_year, y = Avg_CNY/4, color = "CNY Price"), size = 3) +
  scale_y_continuous(
    name = "Average Price (USD)",
    limits = c(0, 25),
    sec.axis = sec_axis(~.*4, name = "Average Price (CNY)")
  ) +
  scale_color_manual(
    name = "Currency",
    values = currency_colors
  ) +
  labs(
    title = "Average Game Prices by Currency and Year",
    subtitle = "Comparing USD, CNY, and CNY converted to USD (7:1 rate)",
    x = "Release Year",
    caption = "Source: Steam API Data"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.y.left = element_text(color = currency_colors["USD Price"]),
    axis.title.y.right = element_text(color = currency_colors["CNY Price"])
  ) +
  scale_x_continuous(breaks = seq(min(avg_price_by_year$release_year), 
                                  max(avg_price_by_year$release_year), by = 1))

# Create a visualization for the USD average and median prices - UNIFIED COLORS
ggplot(avg_price_by_year) +
  geom_line(aes(x = release_year, y = Avg_USD, color = "Average Price"), size = 1) +
  geom_point(aes(x = release_year, y = Avg_USD, color = "Average Price"), size = 3) +
  geom_line(aes(x = release_year, y = Median_USD, color = "Median Price"), size = 1) +
  geom_point(aes(x = release_year, y = Median_USD, color = "Median Price"), size = 3) +
  scale_color_manual(
    name = "Price Type",
    values = price_type_colors
  ) +
  labs(
    title = "Average and Median USD Prices by Year",
    subtitle = "Comparing central tendency measures for USD game prices",
    x = "Release Year",
    y = "Price (USD)",
    caption = "Source: Steam API Data"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_x_continuous(breaks = seq(min(avg_price_by_year$release_year), 
                                  max(avg_price_by_year$release_year), by = 1))

# Create a visualization for the CNY average and median prices - UNIFIED COLORS
# Using the same price_type_colors for consistency
ggplot(avg_price_by_year) +
  geom_line(aes(x = release_year, y = Avg_CNY, color = "Average Price"), size = 1) +
  geom_point(aes(x = release_year, y = Avg_CNY, color = "Average Price"), size = 3) +
  geom_line(aes(x = release_year, y = Median_CNY, color = "Median Price"), size = 1) +
  geom_point(aes(x = release_year, y = Median_CNY, color = "Median Price"), size = 3) +
  scale_color_manual(
    name = "Price Type",
    values = price_type_colors
  ) +
  labs(
    title = "Average and Median CNY Prices by Year",
    subtitle = "Comparing central tendency measures for CNY game prices",
    x = "Release Year",
    y = "Price (CNY)",
    caption = "Source: Steam API Data"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_x_continuous(breaks = seq(min(avg_price_by_year$release_year), 
                                  max(avg_price_by_year$release_year), by = 1))

# -----------------------------
# 2. Price to localisation relationship
# -----------------------------

# Check if games with Chinese localisation command higher prices
# Create a flag for any Chinese localisation
steam_data <- steam_data %>%
  mutate(
    has_chinese_loc = chinese_simplified_interface_subtitles == TRUE | 
      chinese_simplified_audio == TRUE |
      chinese_traditional_interface_subtitles == TRUE |
      chinese_traditional_audio == TRUE,
    has_sc_loc = chinese_simplified_interface_subtitles == TRUE | 
      chinese_simplified_audio == TRUE,
    loc_depth_category = case_when(
      chinese_simplified_audio == TRUE ~ "CS interface/text + dubbing",
      chinese_simplified_interface_subtitles == TRUE ~ "Only CS interface/text",
      TRUE ~ "No CS localisation"
    )
  )

# Compare prices by localisation status and year
price_by_loc_year <- steam_data %>%
  filter(!is.na(release_year) & !is.na(price_usd_numeric)) %>%
  group_by(release_year, has_chinese_loc) %>%
  summarise(
    Games_Count = n(),
    Avg_USD = mean(price_usd_numeric, na.rm = TRUE),
    Median_USD = median(price_usd_numeric, na.rm = TRUE),
    Avg_CNY = mean(price_cny_numeric, na.rm = TRUE),
    Median_CNY = median(price_cny_numeric, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Localisation_Status = ifelse(has_chinese_loc, "With Chinese Support", "Without Chinese Support")
  )

# Display the comparison table
kable(price_by_loc_year, 
      caption = "Average Prices by Chinese Localisation Status and Year",
      digits = c(0, NA, 0, 2, 2, 2, 2, NA))

# Create a visualization for the USD price comparison - UNIFIED COLORS
ggplot(price_by_loc_year, aes(x = release_year, y = Avg_USD, 
                              color = Localisation_Status, group = Localisation_Status)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  scale_color_brewer(palette = alt_palette) +
  labs(
    title = "Average USD Price by Chinese Localisation Status",
    subtitle = "Comparing games with and without Chinese language support",
    x = "Release Year",
    y = "Average Price (USD)",
    color = "Localisation Status",
    caption = "Source: Steam API Data"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_x_continuous(breaks = seq(min(price_by_loc_year$release_year), 
                                  max(price_by_loc_year$release_year), by = 1))

# Create a visualization for the CNY price comparison - UNIFIED COLORS
ggplot(price_by_loc_year, aes(x = release_year, y = Avg_CNY, 
                              color = Localisation_Status, group = Localisation_Status)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  scale_color_brewer(palette = alt_palette) +  # Same palette as USD chart for consistency
  labs(
    title = "Average CNY Price by Chinese Localisation Status",
    subtitle = "Comparing games with and without Chinese language support",
    x = "Release Year",
    y = "Average Price (CNY)",
    color = "Localisation Status",
    caption = "Source: Steam API Data"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_x_continuous(breaks = seq(min(price_by_loc_year$release_year), 
                                  max(price_by_loc_year$release_year), by = 1))

# More detailed analysis by localisation depth
price_by_loc_depth <- steam_data %>%
  filter(!is.na(release_year) & !is.na(price_usd_numeric)) %>%
  group_by(loc_depth_category) %>%
  summarise(
    Games_Count = n(),
    Avg_USD = mean(price_usd_numeric, na.rm = TRUE),
    Median_USD = median(price_usd_numeric, na.rm = TRUE),
    Avg_CNY = mean(price_cny_numeric, na.rm = TRUE),
    Median_CNY = median(price_cny_numeric, na.rm = TRUE),
    .groups = "drop"
  )

# Display the detailed comparison table
kable(price_by_loc_depth, 
      caption = "Average Prices by SC Localisation Depth",
      digits = c(NA, 0, 2, 2, 2, 2))

# Visualize the detailed price by localisation depth for USD - UNIFIED COLORS
ggplot(price_by_loc_depth, aes(x = reorder(loc_depth_category, Avg_USD), y = Avg_USD, fill = loc_depth_category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0("$", round(Avg_USD, 2))), vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = sequential_blue) +  # Using unified sequential blue palette
  labs(
    title = "Average USD Price by SC Localisation Depth",
    subtitle = "Comparing pricing across different levels of Simplified Chinese support",
    x = "Localisation Depth",
    y = "Average Price (USD)",
    caption = "Source: Steam API Data"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "none"
  )

# Visualize the detailed price by localisation depth for CNY - UNIFIED COLORS
ggplot(price_by_loc_depth, aes(x = reorder(loc_depth_category, Avg_CNY), y = Avg_CNY, fill = loc_depth_category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0("¥", round(Avg_CNY, 0))), vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = sequential_red) +  # Using unified sequential red palette
  labs(
    title = "Average CNY Price by SC Localisation Depth",
    subtitle = "Comparing pricing across different levels of Simplified Chinese support",
    x = "Localisation Depth",
    y = "Average Price (CNY)",
    caption = "Source: Steam API Data"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "none"
  )

# -----------------------------
# 3. Price to indie relationship
# -----------------------------

# Create a function to check if "Indie" is in the genres string if not already defined
if(!"has_indie_genre" %in% colnames(steam_data)) {
  has_indie <- function(genre_str) {
    return(grepl("'Indie'", genre_str, fixed = TRUE))
  }
  
  # Apply this function to determine if a game is indie
  steam_data <- steam_data %>%
    mutate(has_indie_genre = has_indie(genres))
}

# Compare prices between indie and non-indie games by year
price_by_indie_year <- steam_data %>%
  filter(!is.na(release_year) & !is.na(price_usd_numeric)) %>%
  group_by(release_year, has_indie_genre) %>%
  summarise(
    Games_Count = n(),
    Avg_USD = mean(price_usd_numeric, na.rm = TRUE),
    Median_USD = median(price_usd_numeric, na.rm = TRUE),
    Avg_CNY = mean(price_cny_numeric, na.rm = TRUE),
    Median_CNY = median(price_cny_numeric, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Game_Type = ifelse(has_indie_genre, "Indie Games", "Non-Indie Games")
  )

# Display the comparison table
kable(price_by_indie_year, 
      caption = "Average Prices by Indie Status and Year",
      digits = c(0, NA, 0, 2, 2, 2, 2, NA))

# Create a visualization for the USD price comparison - UNIFIED COLORS
ggplot(price_by_indie_year, aes(x = release_year, y = Avg_USD, 
                                color = Game_Type, group = Game_Type)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  scale_color_brewer(palette = main_palette) +  # Using main palette for primary comparison
  labs(
    title = "Average USD Price: Indie vs. Non-Indie Games",
    subtitle = "Comparing pricing trends by game type over time",
    x = "Release Year",
    y = "Average Price (USD)",
    color = "Game Type",
    caption = "Source: Steam API Data"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_x_continuous(breaks = seq(min(price_by_indie_year$release_year), 
                                  max(price_by_indie_year$release_year), by = 1))

# Create a visualization for the CNY price comparison - UNIFIED COLORS
ggplot(price_by_indie_year, aes(x = release_year, y = Avg_CNY, 
                                color = Game_Type, group = Game_Type)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  scale_color_brewer(palette = main_palette) +  # Same palette as USD chart for consistency
  labs(
    title = "Average CNY Price: Indie vs. Non-Indie Games",
    subtitle = "Comparing pricing trends by game type over time",
    x = "Release Year",
    y = "Average Price (CNY)",
    color = "Game Type",
    caption = "Source: Steam API Data"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_x_continuous(breaks = seq(min(price_by_indie_year$release_year), 
                                  max(price_by_indie_year$release_year), by = 1))

# Calculate price difference between indie and non-indie games by year
price_diff_by_year <- price_by_indie_year %>%
  select(release_year, Game_Type, Avg_USD, Avg_CNY) %>%
  pivot_wider(
    names_from = Game_Type,
    values_from = c(Avg_USD, Avg_CNY)
  ) %>%
  mutate(
    USD_Price_Difference = `Avg_USD_Non-Indie Games` - `Avg_USD_Indie Games`,
    USD_Percentage_Difference = (USD_Price_Difference / `Avg_USD_Indie Games`) * 100,
    CNY_Price_Difference = `Avg_CNY_Non-Indie Games` - `Avg_CNY_Indie Games`,
    CNY_Percentage_Difference = (CNY_Price_Difference / `Avg_CNY_Indie Games`) * 100
  )

# Display the difference table
kable(price_diff_by_year %>% select(release_year, 
                                    `Avg_USD_Indie Games`, `Avg_USD_Non-Indie Games`, USD_Price_Difference, USD_Percentage_Difference,
                                    `Avg_CNY_Indie Games`, `Avg_CNY_Non-Indie Games`, CNY_Price_Difference, CNY_Percentage_Difference), 
      caption = "Price Difference Between Indie and Non-Indie Games by Year",
      digits = c(0, 2, 2, 2, 1, 2, 2, 2, 1))

# Visualize the USD price difference over time - UNIFIED COLORS
ggplot(price_diff_by_year, aes(x = release_year, y = USD_Price_Difference)) +
  geom_bar(stat = "identity", fill = brewer.pal(8, main_palette)[2]) +  # Consistent color from main palette
  geom_text(aes(label = paste0("$", round(USD_Price_Difference, 2))), 
            vjust = ifelse(price_diff_by_year$USD_Price_Difference >= 0, -0.5, 1.5), 
            size = 3.5) +
  labs(
    title = "USD Price Gap Between Non-Indie and Indie Games",
    subtitle = "Absolute price difference in USD by release year",
    x = "Release Year",
    y = "Price Difference (USD)",
    caption = "Source: Steam API Data"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_x_continuous(breaks = seq(min(price_diff_by_year$release_year), 
                                  max(price_diff_by_year$release_year), by = 1))

# Visualize the CNY price difference over time - UNIFIED COLORS
ggplot(price_diff_by_year, aes(x = release_year, y = CNY_Price_Difference)) +
  geom_bar(stat = "identity", fill = brewer.pal(8, main_palette)[1]) +  # Consistent color from main palette
  geom_text(aes(label = paste0("¥", round(CNY_Price_Difference, 0))), 
            vjust = ifelse(price_diff_by_year$CNY_Price_Difference >= 0, -0.5, 1.5), 
            size = 3.5) +
  labs(
    title = "CNY Price Gap Between Non-Indie and Indie Games",
    subtitle = "Absolute price difference in CNY by release year",
    x = "Release Year",
    y = "Price Difference (CNY)",
    caption = "Source: Steam API Data"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_x_continuous(breaks = seq(min(price_diff_by_year$release_year), 
                                  max(price_diff_by_year$release_year), by = 1))

# -----------------------------
# 4. Price to Early Access relationship
# -----------------------------

# Identify Early Access games from the genres field (not user_tags)
has_early_access <- function(genre_str) {
  return(grepl("'Early Access'", genre_str, fixed = TRUE))
}

# Apply the function to identify Early Access games
steam_data <- steam_data %>%
  mutate(is_early_access = has_early_access(genres))

# Check overlap between Indie and Early Access tags
indie_early_access_overlap <- steam_data %>%
  summarise(
    Total_Games = n(),
    Indie_Games = sum(has_indie_genre, na.rm = TRUE),
    Early_Access_Games = sum(is_early_access, na.rm = TRUE),
    Both_Tags = sum(has_indie_genre & is_early_access, na.rm = TRUE)
  ) %>%
  mutate(
    Indie_Percentage = Indie_Games / Total_Games * 100,
    Early_Access_Percentage = Early_Access_Games / Total_Games * 100,
    Both_Tags_Percentage = Both_Tags / Total_Games * 100,
    EA_With_Indie_Percentage = Both_Tags / Early_Access_Games * 100,
    Indie_With_EA_Percentage = Both_Tags / Indie_Games * 100
  )

# Display the overlap summary
cat("Total games:", indie_early_access_overlap$Total_Games, "\n")
cat("Indie games:", indie_early_access_overlap$Indie_Games, 
    "(", round(indie_early_access_overlap$Indie_Percentage, 2), "%)", "\n")
cat("Early Access games:", indie_early_access_overlap$Early_Access_Games, 
    "(", round(indie_early_access_overlap$Early_Access_Percentage, 2), "%)", "\n")
cat("Games with both tags:", indie_early_access_overlap$Both_Tags, 
    "(", round(indie_early_access_overlap$Both_Tags_Percentage, 2), "% of all games)", "\n")
cat("Percentage of Early Access games that are also Indie:", 
    round(indie_early_access_overlap$EA_With_Indie_Percentage, 2), "%", "\n")
cat("Percentage of Indie games that are also Early Access:", 
    round(indie_early_access_overlap$Indie_With_EA_Percentage, 2), "%", "\n")

# Create a Venn diagram-like representation using a scaled plot - UNIFIED COLORS
overlap_data <- data.frame(
  Category = c("Indie Only", "Early Access Only", "Both"),
  Count = c(
    indie_early_access_overlap$Indie_Games - indie_early_access_overlap$Both_Tags,
    indie_early_access_overlap$Early_Access_Games - indie_early_access_overlap$Both_Tags,
    indie_early_access_overlap$Both_Tags
  )
)

# Plot the overlap with unified colors
ggplot(overlap_data, aes(x = reorder(Category, Count), y = Count, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Count), vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = alt_palette) +  # Using secondary palette for consistency
  labs(
    title = "Overlap Between Indie and Early Access Games",
    subtitle = "Number of games in each category",
    x = NULL,
    y = "Number of Games",
    caption = "Source: Steam API Data"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "none"
  )

# Compare prices between Early Access, Indie, Both, and Neither
game_category <- steam_data %>%
  mutate(
    Category = case_when(
      has_indie_genre & is_early_access ~ "Both Indie & Early Access",
      has_indie_genre ~ "Indie Only",
      is_early_access ~ "Early Access Only",
      TRUE ~ "Neither"
    )
  )

# Calculate average prices by category
price_by_category <- game_category %>%
  filter(!is.na(price_usd_numeric)) %>%
  group_by(Category) %>%
  summarise(
    Games_Count = n(),
    Avg_USD = mean(price_usd_numeric, na.rm = TRUE),
    Median_USD = median(price_usd_numeric, na.rm = TRUE),
    Avg_CNY = mean(price_cny_numeric, na.rm = TRUE),
    Median_CNY = median(price_cny_numeric, na.rm = TRUE),
    Free_Games = sum(is_free, na.rm = TRUE),
    Free_Percentage = sum(is_free, na.rm = TRUE) / n() * 100,
    .groups = "drop"
  ) %>%
  arrange(desc(Avg_USD))

# Display the price by category table
kable(price_by_category, 
      caption = "Average Prices by Game Category",
      digits = c(NA, 0, 2, 2, 2, 2, 0, 1))

# Visualize the USD price comparison - UNIFIED COLORS
ggplot(price_by_category, aes(x = reorder(Category, Avg_USD), y = Avg_USD, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0("$", round(Avg_USD, 2))), vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = sequential_blue) +  # Using unified sequential blue palette
  labs(
    title = "Average USD Price by Game Category",
    subtitle = "Comparing Early Access and Indie game pricing",
    x = "Game Category",
    y = "Average Price (USD)",
    caption = "Source: Steam API Data"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

# Visualize the CNY price comparison - UNIFIED COLORS
ggplot(price_by_category, aes(x = reorder(Category, Avg_CNY), y = Avg_CNY, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0("¥", round(Avg_CNY, 0))), vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = sequential_red) +  # Using unified sequential red palette
  labs(
    title = "Average CNY Price by Game Category",
    subtitle = "Comparing Early Access and Indie game pricing",
    x = "Game Category",
    y = "Average Price (CNY)",
    caption = "Source: Steam API Data"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

# Also compare free game percentages by category - UNIFIED COLORS
ggplot(price_by_category, aes(x = reorder(Category, -Free_Percentage), y = Free_Percentage, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(Free_Percentage, 1), "%")), vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = alt_palette) +  # Using secondary palette for consistency
  labs(
    title = "Percentage of Free-to-Play Games by Category",
    subtitle = "Comparing Early Access and Indie games",
    x = "Game Category",
    y = "Percentage of Free Games",
    caption = "Source: Steam API Data"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "none"
  )

# -----------------------------
# 5. Regional price differences over time
# -----------------------------

# Calculate the price ratio between CNY and USD by year 
# (for games with both prices available and not free)
regional_price_diff <- steam_data %>%
  filter(!is.na(release_year) & !is.na(price_usd_numeric) & !is.na(price_cny_numeric)) %>%
  filter(price_usd_numeric > 0 & price_cny_numeric > 0) %>%  # Exclude free games and unknown prices
  mutate(
    price_ratio = price_cny_numeric / price_usd_numeric
  ) %>%
  group_by(release_year) %>%
  summarise(
    Games_Count = n(),
    Avg_Ratio = mean(price_ratio, na.rm = TRUE),
    Median_Ratio = median(price_ratio, na.rm = TRUE),
    Min_Ratio = min(price_ratio, na.rm = TRUE),
    Max_Ratio = max(price_ratio, na.rm = TRUE),
    SD_Ratio = sd(price_ratio, na.rm = TRUE),
    .groups = "drop"
  )

# Display the price ratio table
kable(regional_price_diff, 
      caption = "CNY/USD Price Ratio Trends by Year",
      digits = c(0, 0, 2, 2, 2, 2, 2))

# Visualize the price ratio trend with confidence intervals - UNIFIED COLORS
# Using a color from the main palette for consistency
main_color <- brewer.pal(8, main_palette)[1]  # First color from main palette

ggplot(regional_price_diff, aes(x = release_year, y = Avg_Ratio)) +
  geom_line(size = 1, color = main_color) +
  geom_point(size = 3, color = main_color) +
  geom_errorbar(aes(ymin = Avg_Ratio - SD_Ratio, ymax = Avg_Ratio + SD_Ratio),
                width = 0.2, color = main_color, alpha = 0.5) +
  geom_text(aes(label = round(Avg_Ratio, 1)), vjust = -0.8, size = 3.5) +
  labs(
    title = "Average CNY/USD Price Ratio Trend",
    subtitle = "With standard deviation error bars",
    x = "Release Year",
    y = "Average CNY/USD Price Ratio",
    caption = "Source: Steam API Data"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_x_continuous(breaks = seq(min(regional_price_diff$release_year), 
                                  max(regional_price_diff$release_year), by = 1))

# Compare price growth rates in USD and CNY
price_growth <- avg_price_by_year %>%
  arrange(release_year) %>%
  mutate(
    USD_YoY_Change = c(NA, diff(Avg_USD) / lag(Avg_USD)[-1] * 100),
    CNY_YoY_Change = c(NA, diff(Avg_CNY) / lag(Avg_CNY)[-1] * 100)
  )

# Display the price growth table
kable(price_growth %>% select(release_year, Avg_USD, USD_YoY_Change, Avg_CNY, CNY_YoY_Change), 
      caption = "Year-over-Year Price Growth Rate Comparison",
      digits = c(0, 2, 1, 2, 1))

# Visualize the price growth rates side by side - UNIFIED COLORS
growth_long <- price_growth %>%
  select(release_year, USD_YoY_Change, CNY_YoY_Change) %>%
  filter(!is.na(USD_YoY_Change)) %>%  # Remove the first year with NA
  pivot_longer(
    cols = c(USD_YoY_Change, CNY_YoY_Change),
    names_to = "Currency",
    values_to = "YoY_Change"
  ) %>%
  mutate(Currency = case_when(
    Currency == "USD_YoY_Change" ~ "US Dollar",
    Currency == "CNY_YoY_Change" ~ "Chinese Yuan"
  ))

# Create a grouped bar chart for year-over-year growth rates - UNIFIED COLORS
ggplot(growth_long, aes(x = release_year, y = YoY_Change, fill = Currency)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0(round(YoY_Change, 1), "%")), 
            position = position_dodge(width = 0.9), 
            vjust = ifelse(growth_long$YoY_Change >= 0, -0.5, 1.5),
            size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_fill_brewer(palette = main_palette) +  # Using main palette for primary comparison
  labs(
    title = "Year-over-Year Price Growth Rate Comparison",
    subtitle = "Comparing annual price changes between USD and CNY",
    x = "Release Year",
    y = "Year-over-Year Change (%)",
    fill = "Currency",
    caption = "Source: Steam API Data"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_x_continuous(breaks = seq(min(growth_long$release_year), 
                                  max(growth_long$release_year), by = 1))
